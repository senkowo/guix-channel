(define-module (my-packages renoise)
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
  ;; #:use-module (guix licenses)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary) ; for binary-build-system
  ;; test...
  ;; #:use-module (ice-9 match)
  ;; #:use-module (ice-9 regex)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-26)
  ;; #:use-module (srfi srfi-34)
  ;; #:use-module (guix i18n)
  ;; #:use-module (guix build utils)
  )

;; references:
;; https://github.com/daviwil/channel-x/blob/master/channel-x/packages/video.scm
;; https://gitlab.com/guix-gaming-channels/games ; avoid deletion of sources when gc

;; note: make sure to git commit before updating channels

;; dont forget to implement guix build --source --root=<path> renoise
;; (is this really necessary? will running gc really require having to reinstall? doubt...)

;; alternative method: use "substitute* file ((regexp match-var…) body…) …" to edit
;; installation file, remove sudo perms, change output path dirs.

;; skip CPU frequency scaling bc it wont work!!!




(define-public renoise-source-path
  "file:///home/yui/Music/prod/misc/test/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz"
  )

;; helper function
(define (get-current-system)
  (match (%current-system)
    ("x86_64-linux" "x86_64")
    ("aarch64-linux" "arm64")
    ("armhf-linux" "armhf")))

;; renoise demo ver
(define-public renoise-demo
  (package
   (name "renoise-demo")
   (version "3.4.3")
   (source
    ;; If renoise-source-path is not defined, download and install the demo ver
    ;; from the site:
    ;; e.g. https://files.renoise.com/demo/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz
    (let ((def-uri
            (string-append
             "https://files.renoise.com/demo/Renoise_"
             (string-replace-substring version "." "_")
             "_Demo_Linux_"
             (get-current-system)
             ".tar.gz")))
      (origin
       (method url-fetch/tarbomb)
       (uri def-uri)
       (sha256
        (base32
         (match (get-current-system) ; i might not keep the hashes up to date, perchance.
           ("x86_64" "9hd7fbk0px45fxhqa7nqcnij8ls2fhpjp60v840vy2zqs9fkcr52")
           ("arm64" "0zpkaiwwxn8yh3s1d22qswshbgaxx5d8iy17hb3w256zgb722yjw")
           ("armhf" "18174b1lgsk73gxhala471ppzbrpa1cs953b5par998yqgh74znk")))))))
   (build-system binary-build-system)
   (arguments
    (append
     ;; get a #:patchelf-plan variant relative to system type
     (match (get-current-system)
       ("x86_64"
        (list #:patchelf-plan #~(list (list "renoise"
                                            '("libc" "gcc" "alsa-lib" "libx11" "libxext" "libxcursor" "libxrandr"))
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
           #:phases #~(modify-phases
                       %standard-phases
                       (replace
                        'install
                        (lambda* (#:key outputs inputs #:allow-other-keys)
                          (let* ((target (string-append #$output))
                                 (bin (string-append #$output "/bin"))
                                 (share (string-append #$output "/share"))
                                 (resources (string-append target "/lib/renoise-" #$version)))
                            
                            (setenv "HOME" "/tmp")
                            (setenv "XDG_DATA_HOME" share)
                            
                            
                            ;; ------- Method 1 --------

                            ;; (format #T "> Replacing variables in install script...~%")
			    
			    ;; (substitute*
                            ;;  "./install.sh"
                            ;;  ;; Below replaces the default target (e.g. /usr/local/) to
                            ;;  ;; the correct one (#$output).
                            ;;  (("(BINARY_PATH=).*" _ var)
                            ;;   (string-append var bin "\n"))
                            ;;  (("(SYSTEM_LOCAL_SHARE=).*" _ var)
                            ;;   (string-append var share "\n"))
                            ;;  (("(RESOURCES_PATH=).*" _ var)
                            ;;   (string-append var resources "\n"))
                            ;;  ;; Turn "if [ `id -u` -ne 0 ]; then" into the following:
                            ;;  ;; "if [ `id -u` -ne 0 ] && [ `id -u` -ne 999 ]; then"
                            ;;  ;; This prevents the script from aborting with perm error.
                            ;;  (("(if.*)(\\[.*id -u.*\\])(;.*)" _ iff brackets ending)
                            ;;   (string-append iff brackets
                            ;;                  " && [ `id -u` -ne 999 ]" ending "\n"))
                            ;;  ;; By default, a successful installation will exit with an 
                            ;;  ;; exit code of 1, which will cause the package installation
                            ;;  ;; to fail. This adds an "exit 0" before that's called.
                            ;;  (("(.*)echo.*Installation.*SUCCEEDED.*" all indent)
                            ;;   (string-append all
                            ;;                  indent "exit 0\n")))

                            ;; (invoke "cat" "./install.sh") ; debug
			    
                            ;; (format #T "> Running install script...~%")
                            ;; (invoke "sh" "./install.sh")
                            ;; (format #T "> Finished running install script...~%")

			    ;; (invoke "tree" target)

			    ;; ------misc
			    
			    ;; Fix icons issue:
			    ;; (format #T "> Install icons system-wide...~%")
			    ;; ;; Installing icons...
                            ;; (format #T "> Installing icons...~%")
                            ;; (for-each (lambda (res)
			    ;; 		(let ((dest-dir
			    ;; 		       (string-append share "/icons/hicolor/"
			    ;; 				      res "x" res "/apps")))
			    ;; 		  (mkdir-p dest-dir)
			    ;; 		  (copy-file
			    ;; 		   (string-append resources "/Installer/renoise-" res
			    ;; 				  ".png")
			    ;; 		   (string-append dest-dir "/renoise.png"))))
                            ;;           '("48" "64" "128"))
			    
			    ;; ;; Fixing file permissions...
			    
			    ;; ------- Method 2 -------

                            ;; (format #T "> Fixing file permissions...~%")
                            
                            ;; ;; wrapper around find-files (similar to "$ find A -type B
                            ;; ;; -name C -exec chmod D {} \;")
                            ;; (define (find-and-chmod path type name-regex ch-perm)
                            ;;   (for-each (lambda (f)
			    ;; 		  (chmod f ch-perm))
			    ;; 		(find-files
			    ;; 		 path 
			    ;; 		 (lambda (file stat) ; checks type and name-regex:
			    ;; 		   (and
                            ;;                 (cond ((equal? "d" type)
			    ;; 			   (directory-exists? file))
			    ;; 			  ((equal? "f" type)
			    ;; 			   (not (directory-exists? file)))
			    ;; 			  (t (error "invalid find-command type")))
                            ;;                 ((file-name-predicate name-regex) file stat)))
			    ;; 		 #:directories? #t
			    ;; 		 #:fail-on-error? #t)))
                            
                            ;; (find-and-chmod "." "d" ".*" '#o755)
                            ;; (find-and-chmod "." "f" ".*" '#o644)
                            ;; (find-and-chmod "." "f" ".*sh" '#o755)
                            ;; (find-and-chmod "./Installer/xdg-utils" "f" "xdg-.*" '#o755)
                            ;; (chmod "./renoise" '#o755)
                            ;; (find-and-chmod "./Resources" "f" "AudioPluginServer_.*" '#o755)

                            ;; ;; Installing shared resources...
                            ;; (format #T "> Installing shared resources...~%")
                            ;; (mkdir-p resources) ; vvv does not copy the src dir itself
                            ;; (copy-recursively "./Resources" resources)
                            ;; (install-file "./install.sh" resources)
                            ;; (install-file "./uninstall.sh" resources)
                            ;; (copy-recursively "./Installer"
                            ;;                   (string-append resources "/Installer"))
                            
                            ;; ;; Installing the executable...
                            ;; (format #T "> Installing the executable...~%")
                            ;; (mkdir-p bin)
                            ;; (copy-file "./renoise" ; dont install-file, want a different name
                            ;;            (string-append bin "/renoise-" #$version))
                            
                            ;; ;; Linking the executable...
                            ;; (format #T "> Linking the executable...~%")
                            ;; (symlink (string-append bin "/renoise-" #$version)
                            ;;          (string-append bin "/renoise"))

                            ;; ;; Installing the man file...
                            ;; (format #T "> Installing the man file...~%")
                            ;; (install-file "./Installer/renoise.1.gz"
			    ;; 		  (string-append share "/man/man1"
			    ;; 				 "/renoise.1.gz"))
                            ;; (install-file "./Installer/renoise-pattern-effects.5.gz"
			    ;; 		  (string-append share "/man/man5"
			    ;; 				 "/renoise-pattern-effects.5.gz"))
                            
                            ;; ;; Registering MIME types... ; what's this do? location?
                            ;; (format #T "> Registering MIME types...~%")
                            ;; (invoke "xdg-mime" "install" "--novendor"
                            ;;         (string-append resources "/Installer/renoise.xml"))

                            ;; ;; Installing icons...
                            ;; (format #T "> Installing icons...~%")
                            ;; (for-each (lambda (res) 
			    ;; 		(install-file
			    ;; 		 (string-append resources "/Installer/renoise-" res
			    ;; 				".png")
			    ;; 		 (string-append share "/icons/hicolor/"
			    ;; 				res "x" res
			    ;; 				"/apps/renoise.png")))
                            ;;           '("48" "64" "128"))
                            
                            ;; ;; Installing desktop-menu shortcuts...
                            ;; (format #T "> Installing desktop-menu shortcuts...~%")
                            ;; (invoke "xdg-desktop-menu" "install" "--novendor"
                            ;;         (string-append resources "/Installer/renoise.desktop"))

			    
			    
			    ;; ------- Method 3 -------
			    
			    
			    ;; Doing everything the NixOS install script does, bc why the hell not
			    ;; (this better work, please :sob:).
			    
			    (format #T "> Fixing file permissions...~%")
                            ;; wrapper around find-files (similar to "$ find A -type B
                            ;; -name C -exec chmod D {} \;")
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


			    (copy-recursively "./Resources" target)

			    (format #T "> Installing the executable...~%")
			    (install-file "./renoise" target)
			    
			    (format #T "> Linking the executable...~%")
			    (mkdir-p bin)
			    (symlink (string-append target "/renoise")
                                     (string-append bin "/renoise"))

			    )))))))
   ;; during install
   (native-inputs
    (list
     grep
     which
     xdg-utils
     util-linux
     ;; debug
     ))
   ;; post install
   (inputs
    (list alsa-lib
          `(,gcc "lib")
          libx11
          libxext
	  libxcursor
	  libxrandr
	  ;; testing
	  xdg-utils		  ; need xdg-icon-resource at startup?
	  mpg123
	  ))
   (supported-systems '("x86_64-linux" "aarch64-linux" "armhf-linux"))
   
   (synopsis "Modern tracker-based DAW")
   (description "Modern tracker-based DAW")
   (home-page "https://www.renoise.com/")
   (license (license:nonfree (string-append "file:///share/doc/renoise-" version
                                            "/License.txt")))))

;; just for testing
(define-public paid-renoise-ver-manifest
  (define renoise
    (package
     (inherit renoise-demo)
     (name "renoise")))
  (define transform-install-path
    (options->transformation
     '((with-source
	. "renoise=/home/yui/Music/prod/misc/test/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz"))))
  (packages->manifest (list (transform-install-path renoise))))

paid-renoise-ver-manifest


;; (local-file renoise-source-path #:recursive #t)
;; can be just a (local-file "path" #:recursive #t/f?) instead?
;; If a local path to the renoise tar file is specified, it will install
;; that. (Note that the following adds the tar file to the guix store, so
;; that it can be processed) (Also note that each paid renoise tarbomb has
;; a unique hash, so it cannot be predefined.)
;; (let ((tarball (with-store store
;;                            (download-to-store store renoise-source-path))))
;;   (origin
;;    (method url-fetch)
;;    (uri renoise-source-path)
;;    (sha256
;;     (base32 (bytevector->nix-base32-string
;;              (file-hash* tarball #:recursive? #false))))))
;; )

;; run "gcc-unhidden:lib" to get the libs
;; (define-public gcc-unhidden
;;   (package
;;    (inherit gcc)
;;    (name "gcc-unhidden")
;;    (properties (alist-delete 'hidden? (package-properties gcc)))))


