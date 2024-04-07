(define-module (yumi packages browsers)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download) ; url-fetch
  ;; #:use-module (guix hash)
    #:use-module (ice-9 match) ; match
  #:use-module (ice-9 regex) ; match:substring
  ;; #:use-module (guix utils) ; %current-system

  #:use-module (guix gexp) ; replace 'install

  ;; #:use-module (gnu packages) 

  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages xdisorg) ; startup-notification

  )





(define (librewolf-src-url ver)
  ;; Example url:
  ;; "https://gitlab.com/api/v4/projects/44042130/packages/generic/librewolf/121.0-1/librewolf-121.0-1-linux-x86_64-package.tar.bz2"
  (let* ((ver-rx (regexp-exec (make-regexp "([0-9]+.[0-9]+).([0-9]+)")
                              ver))
         (major (match:substring ver-rx 1))
         (minor (match:substring ver-rx 2)))
    (string-append "https://gitlab.com/api/v4/projects/44042130/packages/generic/librewolf/"
                   major "-" minor "/librewolf-" major "-" minor "-" "linux-"
                   (match (%current-system)
                     ("x86_64-linux" "x86_64")
                     ("aarch64-linux" "arm64"))
                   "-package.tar.bz2")))

(define-public (librewolf-bin-builder ver hash-x86_64 hash-arm64)
  (package
    (name "librewolf-bin")
    (version ver)
    (source (origin
              (method url-fetch)
              (uri (librewolf-src-url ver))
              (sha256
               (base32
                (match (%current-system)
                  ("x86_64-linux" hash-x86_64)
                  ("aarch64-linux" hash-arm64))))))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f
           #:phases #~(modify-phases %standard-phases
                        (replace 'install
                          (lambda* (#:key outputs inputs #:allow-other-keys)
                            (let* ((out (string-append #$output))
                                   (lib (string-append out "/lib"))
                                   (version #$version))

                              (setenv "HOME" "/tmp")

                              ;; chmod everything here?

                              (copy-recursively "." lib)
                              
                              
                              
                              
                              )))
                        ;; (add-after 'wrap-program 'install-desktop-entry
                        ;;   (lambda* (#:key outputs #:allow-other-keys)
                        ;;     (let* ((desktop-file "taskcluster/docker/firefox-snap/firefox.desktop")
                        ;;            (applications (string-append #$output "/share/applications")))
                        ;;       (substitute* desktop-file
                        ;;         (("^Exec=firefox") (string-append "Exec=" #$output "/bin/firefox"))
                        ;;         (("Icon=.*") "Icon=firefox\n")
                        ;;         (("NewWindow") "new-window")
                        ;;         (("NewPrivateWindow") "new-private-window")
                        ;;         (("StartupNotify=true")
                        ;;          "StartupNotify=true\nStartupWMClass=Navigator"))
                        ;;       (install-file desktop-file applications))))
                        (add-after 'install-desktop-entry 'install-icons
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let ((icon-source-dir
                                   (string-append
                                    #$output
                                    "/lib/librewolf/browser/chrome/icons/default")))
                              (for-each
                               (lambda (size)
                                 (let ((dest (string-append #$output "/share/icons/hicolor/"
                                                            size "x" size "/apps")))
                                   (mkdir-p dest)
                                   (symlink (string-append icon-source-dir
                                                           "/default" size ".png")
                                            (string-append dest "/firefox.png"))))
                               '("16" "32" "48" "64" "128"))))))
           ))

    (inputs
     (list startup-notification))
    

    (supported-systems '("x86_64-linux" "aarch64-linux"))

    (synopsis "Librewolf Web Browser")
    (description "Community-maintained fork of Firefox, focused on privacy, security and freedom.")
    (home-page "https://librewolf.net/")
    (license license:mpl2.0)
    ))

(define-public librewolf-bin-120.0.1
  (librewolf-bin-builder "120.0.1"
                         "1w4hkh57nxl1c39kyxm790y38c67hipf46j51kggsl64iixh9z4g"
                         "1w4hkh57nxl1c39kyxm790y38c67hipf46j51kggsl64iixh9z4g"))

librewolf-bin-120.0.1
