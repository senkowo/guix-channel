(define-module (yumi packages renoise)
  ;; package deps
  #:use-module (gnu packages linux) ; alsa-lib
  #:use-module (gnu packages xorg) ; libx11
  #:use-module (gnu packages base) ; which
  #:use-module (gnu packages freedesktop) ; xdg-utils
  #:use-module (gnu packages gcc) ; gcc
  #:use-module (gnu packages mp3) ; mpg123
  ;; utils
  #:use-module (guix packages) ; package, origin, ...
  #:use-module (guix download) ; download-to-store, url-fetch/tarbomb, ...
  #:use-module (guix store) ; with-store
  #:use-module (guix base32) ; bytevector->nix-base32-string
  #:use-module (guix hash) ; file-hash*
  #:use-module (guix utils) ; %current-system
  #:use-module (ice-9 match) ; match
  #:use-module (ice-9 regex) ; match syntax
  #:use-module (srfi srfi-1) ; 'hidden?
  #:use-module (guix gexp) ; replace 'install
  ;; transformations
  #:use-module (guix transformations) ; options->transformation
  #:use-module (guix profiles) ; packages->manifest
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary) ; for binary-build-system
  )

;; references:
;; https://github.com/daviwil/channel-x/blob/master/channel-x/packages/video.scm
;; https://gitlab.com/guix-gaming-channels/games ; avoid deletion of sources when gc

;; skips CPU frequency scaling check!

;; helper function
(define (get-current-system)
  (match (%current-system)
    ("x86_64-linux" "x86_64")
    ("aarch64-linux" "arm64")
    ("armhf-linux" "armhf")))

;; build renoise from this
(define (renoise-builder ver hash_x86_64 hash_arm64 hash_armhf)
  (package
    (name "renoise")
    (version ver)
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri
        ;; e.g. https://files.renoise.com/demo/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz
        (string-append "https://files.renoise.com/demo/Renoise_"
                       (string-replace-substring version "." "_")
                       "_Demo_Linux_"
                       (get-current-system)
                       ".tar.gz"))
       (sha256
        (base32
         (match (get-current-system) ; i might not keep the hashes up to date, perchance.
           ("x86_64" hash_x86_64)
           ("arm64" hash_arm64)
           ("armhf" hash_armhf))))))
    (build-system binary-build-system)
    (arguments
     (append
      ;; get a #:patchelf-plan variant relative to system type
      ;; (cant put variables in #:patchelf-plan?)
      (match (get-current-system)
        ("x86_64"
         (list #:patchelf-plan #~(list (list "renoise"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext"))
                                       (list "Resources/AudioPluginServer_x86_64"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext")))))
        ("arm64"
         (list #:patchelf-plan #~(list (list "renoise"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext"))
                                       (list "Resources/AudioPluginServer_arm64"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext")))))
        ("armhf"
         (list #:patchelf-plan #~(list (list "renoise"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext"))
                                       (list "Resources/AudioPluginServer_armhf"
                                             '("libc" "gcc" "alsa-lib" "libx11" "libxext"))))))
      ;; append to the previous list containing #:patchelf-plan
      (list #:strip-binaries? #f
            #:phases #~(modify-phases %standard-phases
                         (replace 'install
                           (lambda* (#:key outputs inputs #:allow-other-keys)
                             (let* ((out (string-append #$output))
                                    (bin (string-append #$output "/bin"))
                                    (share (string-append #$output "/share")))
                               
                               (setenv "HOME" "/tmp")
                               (setenv "XDG_DATA_HOME" share)
                               
                               ;; Fixes file permissions
                               ;; (function below is a wrapper around find-files)
                               ;; (similar to "$ find A -type B -name C -exec chmod D {} \;")
                               (define (find-and-chmod path type name-regex ch-perm)
                                 (for-each (lambda (f)
                                             (chmod f ch-perm))
                                           (find-files
                                            path 
                                            (lambda (file stat) ; checks type and name-regex:
                                              (and
                                               (cond ((equal? "d" type)
                                                      (directory-exists? file))
                                                     ((equal? "f" type)
                                                      (not (directory-exists? file)))
                                                     (t (error "invalid find-command type")))
                                               ((file-name-predicate name-regex) file stat)))
                                            #:directories? #t
                                            #:fail-on-error? #t)))
                               (find-and-chmod "." "d" ".*" '#o755)
                               (find-and-chmod "." "f" ".*" '#o644)
                               (find-and-chmod "." "f" ".*sh" '#o755)
                               (find-and-chmod "./Installer/xdg-utils" "f" "xdg-.*" '#o755)
                               (chmod "./renoise" '#o755)
                               (find-and-chmod "./Resources" "f" "AudioPluginServer_.*" '#o755)

                               ;; extract Resources to out
                               (copy-recursively "./Resources" out)

                               ;; put binary to out
                               (install-file "./renoise" out)

                               ;; link binary to out/bin
                               (let ((renoise-binary (string-append out "/renoise")))
                                 (mkdir-p bin)
                                 (symlink renoise-binary
                                          (string-append bin "/renoise"))
                                 (symlink renoise-binary
                                          (string-append bin "/renoise-" #$version)))
                               
                               ;; install desktop launcher (xdg-desktop-menu)
                               (let ((desktop-file "./Installer/renoise.desktop"))
                                 ;; first change the Exec= path to the binary:
                                 (substitute* desktop-file
                                   (("(Exec=).*( .*)$" all exec ending)
                                    (string-append exec bin "/renoise" ending)))
                                 (install-file desktop-file
                                               (string-append share "/applications")))

                               ;; install icons (xdg-icon-resource)
                               (for-each (lambda (res)
                                           (let ((icons-dir
                                                  (string-append share "/icons/hicolor/"
                                                                 res "x" res "/apps")))
                                             (mkdir-p icons-dir)
                                             (copy-file
                                              (string-append "./Installer/renoise-" res ".png")
                                              (string-append icons-dir "/renoise.png"))))
                                         '("48" "64" "128"))

                               ;; register mime types (xdg-mime)
                               (install-file "./Installer/renoise.xml"
                                             (string-append share "/mime/packages"))

                               ;; install man files
                               (install-file "./Installer/renoise.1.gz"
                                             (string-append share "/man/man1"))
                               (install-file "./Installer/renoise-pattern-effects.5.gz"
                                             (string-append share "/man/man5"))

                               )))))))
    ;; (native-inputs
    ;;  (list xdg-utils
    ;;        util-linux))
    (inputs
     (list alsa-lib
           `(,gcc "lib")
           libx11
           libxext
           mpg123))
    (supported-systems '("x86_64-linux" "aarch64-linux" "armhf-linux"))
    
    (synopsis "Modern tracker-based DAW")
    (description "Modern tracker-based DAW")
    (home-page "https://www.renoise.com/")
    (license (license:nonfree (string-append "file:///share/doc/renoise-" version
                                             "/License.txt")))))

;; The actual renoise packages below
(define-public renoise-3.4.3
  (renoise-builder "3.4.3"
                   "0nkyidxyp8r7jgdjld6f1bsk59927rw0n1n4j492ncwblwszmp0j" ; x86_64
                   "19hbfy5mwg9ywabcpbnv31caqy224si7cwpa84x1dwal2b360nix" ; arm64
                   "0hiyq4p2d3nwjzaa32zdkrcld51hx34wf97qmf2kqhkmdxs26i6m")) ; armhf


