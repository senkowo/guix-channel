(define-module (my-packages renoise)
  ;; package deps
  #:use-module (guix packages) ; package, origin, ...
  #:use-module (gnu packages linux) ; alsa-lib
  #:use-module (gnu packages xorg) ; libx11
  #:use-module (gnu packages base) ; which
  #:use-module (gnu packages freedesktop) ; xdg-utils
  #:use-module (gnu packages gcc) ; gcc
  ;; utils
  ;; #:use-module (guix import utils) ; url-fetch ? ; this breaks url-fetch!!!!!
  #:use-module (guix download) ; download-to-store, url-fetch/tarbomb, ...
  #:use-module (guix gexp) ; replace 'install
  ;; for gcc-unhidden

  #:use-module (guix utils) ; target x86_64?
  ;; #:use-module ((guix download) #:select (download-to-store url-fetch ))
  #:use-module (guix store) ; with-store
  #:use-module (guix hash) ; file-hash*
  #:use-module (guix base32) ; bytevector->nix-base32-string
  #:use-module (ice-9 match) ; match
  #:use-module (ice-9 regex) ; match syntax
  #:use-module (srfi srfi-1) ; 'hidden?
  ;; other
  #:use-module (guix licenses)
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




(define-public renoise-source-path
  "file:///home/yui/Music/prod/misc/test/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz" ; untarred
  )

;; da real renoise package
(define-public renoise
  (package
   (name "renoise")
   (version "3.4.3")
   (source
    ;; (origin
    ;;  (file-name
    ;;   renoise-source-path))
    
    (let ((tarball (with-store store
			       (download-to-store store renoise-source-path))))
      
      (origin
       (method url-fetch)
       (uri renoise-source-path)
       (sha256
	(base32 (bytevector->nix-base32-string (file-hash* tarball #:recursive? #false))))))

    ;; (origin
     
    ;;  ;; (method url-fetch/tarbomb)
    ;;  ;; (uri (string-append "https://files.renoise.com/demo/Renoise_"
    ;;  ;; 			 (string-replace-substring version "." "_")
    ;;  ;; 			 "_Demo_Linux_"
    ;;  ;; 			 (match (%current-system)
    ;;  ;; 			   ("x86_64-linux" "x86_64"))
    ;;  ;; 			 ".tar.gz"))
    ;;  (method uri-fetch)
    ;;  (uri renoise-source-path)
    ;;  (sha256
    ;;   (base32
    ;;    (match (%current-system)
    ;; 	 ("x86_64-linux" "0nkyidxyp8r7jgdjld6f1bsk59927rw0n1n4j492ncwblwszmp0j")))))
    
    ;; ;; If a local path to the renoise sources (renoise-source-path) is undefined,
    ;; ;; fetch the demo tarbomb from the uri. If else, the file-path defined at
    ;; ;; renoise-source-path will be used (necessary for the full version).
    ;; (cond ((not renoise-source-path)
    ;; 	   ;; e.g. https://files.renoise.com/demo/Renoise_3_4_3_Demo_Linux_x86_64.tar.gz
    ;; 	   (let* ((def-uri (string-append
    ;; 			    "https://files.renoise.com/demo/Renoise_"
    ;; 			    (string-replace-substring version "." "_")
    ;; 			    "_Demo_Linux_"
    ;; 			    (match (%current-system)
    ;; 			      ("x86_64-linux" "x86_64")
    ;; 			      ;; ("aarch64-linux" "arm64") ; check if the install scripts are same 
    ;; 			      ;; ("armhf-linux" "armhf")
    ;; 			      )
    ;; 			    ".tar.gz")))
    ;; 	     (origin
    ;; 	      (method url-fetch/tarbomb)
    ;; 	      (uri def-uri))
    ;; 	     ;; hashes if installing from uri
    ;; 	     (sha256
    ;; 	      (base32
    ;; 	       (match (%current-system)
    ;; 		 ("x86_64-linux" "1hd7fbk0px45fxhqa7nqcnij8ls2fhpjp60v840vy2zqs9fkcr52")
    ;; 		 ;; ("aarch64-linux" "0zpkaiwwxn8yh3s1d22qswshbgaxx5d8iy17hb3w256zgb722yjw")
    ;; 		 ;; ("armhf-linux" "18174b1lgsk73gxhala471ppzbrpa1cs953b5par998yqgh74znk")
    ;; 		 ))))
    ;; 	   (t
    ;; 	    ;; no need to check hashes, because each paid renoise tarbomb has a unique hash
    ;; 	    (file-name renoise-source-path))))
    )
   (build-system binary-build-system)
   (arguments
    (list #:strip-binaries? #f ; what does this do?
	  #:patchelf-plan #~`(("renoise" ("libc" "gcc" "alsa-lib" "libx11" "libxext"))
			      ("Resources/AudioPluginServer_x86_64"
			       ("libc" "gcc" "alsa-lib" "libx11" "libxext")))
	  #:phases #~(modify-phases
		      %standard-phases
		      (replace
		       'install
		       (lambda* (#:key outputs inputs #:allow-other-keys)
			 (format #t "DEBUG: before setting variables...~%")
			 (let* ((target (string-append #$output))
				(bin (string-append #$output "/bin"))
				(share (string-append #$output "/share"))
				(resources (string-append share "/renoise-" #$version)) ; problem
				;; (default-target (string-append target "/usr/local"))
				;; (default-bin (string-append default-target "/bin"))
				;; (default-share (string-append default-target "/share"))
				;; (default-resources (string-append default-share "/renoise-"
				;; 				  #$version))
				)
			   (format #t "DEBUG: AFTERRRR setting variables...woooo!!!~%")
			   
			   (setenv "HOME" "/tmp")
			   (setenv "XDG_DATA_HOME" share)
			   
			   
			   ;; ------- Method 1 --------
			   
			   ;; (invoke "sh" "./install.sh")

			   ;; (mkdir-p bin)
			   ;; (mkdir-p share)

			   ;; ;; copy over /usr/local/bin/renoise to /bin
			   ;; (delete-file (string-append default-bin "/renoise")) ; delete symlink
			   ;; (copy-file (string-append default-bin "/renoise-" version)
			   ;; 	      bin)
			   ;; (symlink (string-append bin "renoise-" version)
			   ;; 	    (string-append bin "renoise"))

			   ;; ;; copy over /usr/local/share/renoise-# to /share
			   ;; (copy-recursively default-resources share)
			   ;; ;; install the man pages ourselves
			   ;; (invoke (string-append 
			   ;; 	    "install -D -m644 ./Installer/renoise.1.gz "
			   ;; 	    share "/man/man1/renoise.1.gz && "
			   ;; 	    "install -D -m644 ./Installer/renoise-pattern-effects.5.gz "
			   ;; 	    share "/man/man5/renoise-pattern-effects.5.gz"))

			   ;; ;; delete the stuff in the default target
			   ;; (delete-files-recursively (string-append target "/usr")) ; no more /usr?
			   
			   
			   
			   ;; ---------- Method 2 -----------

			   ;; Fixing file permissions...
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
			   
			   ;; Installing shared resources...
			   (format #T "> Installing shared resources...~%")
			   (mkdir-p resources) ; vvv does not copy the src dir itself
			   (copy-recursively "./Resources" resources)
			   (install-file "./install.sh" resources)
			   (install-file "./uninstall.sh" resources)
			   (copy-recursively "./Installer"
					     (string-append resources "/Installer"))
			   
			   ;; Installing the executable...
			   (format #T "> Installing the executable...~%")
			   (mkdir-p bin)
			   (copy-file "./renoise" ; dont install-file, want a different name
				      (string-append bin "/renoise-" #$version))
			   
			   ;; Linking the executable...
			   (format #T "> Linking the executable...~%")
			   (symlink (string-append bin "/renoise-" #$version)
				    (string-append bin "/renoise"))

			   ;; Installing the man file...
			   (format #T "> Installing the man file...~%")
			   (install-file "./Installer/renoise.1.gz"
					 (string-append share "/man/man1"
							"/renoise.1.gz"))
			   (install-file "./Installer/renoise-pattern-effects.5.gz"
					 (string-append share "/man/man5"
							"/renoise-pattern-effects.5.gz"))
			   
			   ;; Registering MIME types... ; what's this do? location?
			   (format #T "> Registering MIME types...~%")
			   (invoke "xdg-mime" "install" "--novendor"
				   (string-append resources "/Installer/renoise.xml"))

			   ;; Installing icons...
			   (format #T "> Installing icons...~%")
			   (for-each (lambda (res) 
				       (install-file
					(string-append resources "/Installer/renoise-" res
						       ".png")
					(string-append share "/icons/hicolor/"
						       res "x" res
						       "/apps/renoise.png")))
				     '("48" "64" "128"))
			   
			   ;; Installing desktop-menu shortcuts...
			   (format #T "> Installing desktop-menu shortcuts...~%")
			   (invoke "xdg-desktop-menu" "install" "--novendor"
				   (string-append resources "/Installer/renoise.desktop"))
			   
			   ;; ;; Do checks here?
			   
			   ))))))
   
   
   (native-inputs
    (list
     which
     util-linux ; chmod
     xdg-utils))
   (inputs
    (list
     alsa-lib
     `(,gcc "lib")
     libx11
     libxext
     ;; wdl
     ))
   (supported-systems '("x86_64-linux"
			;; "aarch64-linux" "armhf-linux"
			))
   
   (synopsis "Modern tracker-based DAW")
   (description "Modern tracker-based DAW")
   (home-page "https://www.renoise.com/")
   (license (license:nonfree (string-append "file:///share/doc/renoise-" version
					    "/License.txt")))))

;; run "gcc-unhidden:lib" to get the libs
(define-public gcc-unhidden
  (package
   (inherit gcc)
   (name "gcc-unhidden")
   (properties (alist-delete 'hidden? (package-properties gcc)))))


