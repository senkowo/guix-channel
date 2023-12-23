(define-module (my-packages renoise)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  ;; for gcc-unhidden
  #:use-module (gnu packages gcc)
  #:use-module (srfi srfi-1)
  ;; for renoise packaged with guix
  #:use-module (nonguix build-system binary) ; for binary-build-system
  #:use-module (guix gexp) ; replace 'install
  #:use-module ((nonguix licenses) #:prefix license:)

  #:use-module (gnu packages linux) ; alsa-lib
  #:use-module (gnu packages xorg) ; libx11
  #:use-module (gnu packages base) ; which
  #:use-module (gnu packages freedesktop) ; xdg-utils
  ;; #:use-module (guix utils)
  ;; #:use-module (ice-9 match)
  )

;; references:
;; https://github.com/daviwil/channel-x/blob/master/channel-x/packages/video.scm

;; note: make sure to git commit before updating channels

(define-public renoise-source-path
  "file:///home/yui/Music/prod/misc/test/Renoise_3_4_3_Demo_Linux_x86_64" ; untarred
  )

;; da real renoise package
(define-public renoise
  (package
   (name "renoise")
   (version "3.4.3")
   (source 
    renoise-source-path
    ;; (origin
    ;;  (method url-fetch)
    ;;  (uri renoise-source-path))
    
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
			 (let* (
				;; (target (string-append #$output "/opt")) ; where is #$output???
				(target #$output)
				(bin (string-append #$output "/bin"))
				(share (string-append #$output "/share"))
				(resources (string-append share "/renoise-" version))
				(default-target (string-append target "/usr/local"))
				(default-bin (string-append def-target "/bin"))
				(default-share (string-append def-target "/share"))
				(default-resources (string-append def-share "/renoise-" version)))
			   (setenv "HOME" "/tmp")
			   (setenv "XDG_DATA_HOME" share)
			   
			   
			   ;; ------- Method 1 --------
			   
			   (invoke "sh" "./install.sh")

			   (mkdir-p bin)
			   (mkdir-p share)

			   ;; copy over /usr/local/bin/renoise to /bin
			   (delete-file (string-append default-bin "/renoise")) ; delete symlink
			   (copy-file (string-append default-bin "/renoise-" version)
				      bin)
			   (symlink (string-append bin "renoise-" version)
				    (string-append bin "renoise"))

			   ;; copy over /usr/local/share/renoise-# to /share
			   (copy-recursively default-resources share)
			   ;; install the man pages ourselves
			   (invoke (string-append 
				    "install -D -m644 ./Installer/renoise.1.gz "
				    share "/man/man1/renoise.1.gz && "
				    "install -D -m644 ./Installer/renoise-pattern-effects.5.gz "
				    share "/man/man5/renoise-pattern-effects.5.gz"))

			   ;; delete the stuff in the default target
			   (delete-files-recursively (string-append target "/usr")) ; no more /usr?
			   
			   
			   
			   ;; ---------- Method 2 -----------
			   
			   ;; (for-each (lambda (cmd) (invoke cmd))
			   ;; 	    '("find . -type d -exec chmod 755 {}"
			   ;; 	      "find . -type f -exec chmod 644 {}"
			   ;; 	      "find . -type f -name \"*.sh\" -exec chmod 755 {}"
			   ;; 	      "find ./Installer/xdg-utils -type f -name \"xdg-*\" -exec chmod 755 {}"
			   ;; 	      "chmod 755 ./renoise"
			   ;; 	      "chmod 755 ./Resources/AudioPluginServer_*"))

			   ;; ;; Installing shared resources...
			   ;; (mkdir-p resources)
			   ;; (copy-recursively "./Resources/*" resources) ; basically cp -r
			   ;; (for-each (lambda (f) (copy-recursively f resources))
			   ;; 	    '("./install.sh"
			   ;; 	      "./uninstall.sh"
			   ;; 	      "./Installer"))
			   
			   ;; ;; Installing the executable...
			   ;; (mkdir-p bin)
			   ;; (copy-files "./renoise"
			   ;; 	      (string-append bin "/renoise-" version))
			   
			   ;; ;; Linking the executable...
			   ;; (symlink (string-append bin "/renoise-" version)
			   ;; 	   (string-append bin "/renoise"))

			   ;; ;; Installing the man file...
			   ;; (invoke (string-append
			   ;; 	   "install -D -m644 ./Installer/renoise.1.gz "
			   ;; 	   share "/man/man1/renoise.1.gz"))
			   ;; (invoke (string-append
			   ;; 	   "install -D -m644 ./Installer/renoise-pattern-effects.5.gz "
			   ;; 	   share "/man/man5/renoise-pattern-effects.5.gz"))

			   ;; ;; Registering MIME types...
			   ;; (invoke (string-append
			   ;; 	   "xdg-mime install --novendor "
			   ;; 	   resources "/Installer/renoise.xml"))

			   ;; ;; Installing icons...
			   ;; (for-each (lambda () (res)
			   ;; 	       (invoke
			   ;; 	        (string-append
			   ;; 		 "xdg-icon-resource install --novendor --size " res
			   ;; 		 " --context apps " resources "/Installer/renoise-"
			   ;; 		 res ".png renoise"))
			   ;; 	       (invoke
			   ;; 		(string-append
			   ;; 		 "xdg-icon-resource install --novendor --size " res
			   ;; 		 " --context mimetypes " resources "/Installer/renoise-"
			   ;; 		 res ".png application-x-renoise-module"))))
			   
			   ;; ;; overcomplicated, Nixpkg method, w/o xdg
			   ;; ;; (for-each (lambda (res) ; xdg-icons-resource install (apps? mimetypes?)
			   ;; ;; 	      (let* ((res-x (string-append res "x" res)) ; e.g. 48x48
			   ;; ;; 		     (dest-dir (string-append target
			   ;; ;; 					      "/share/icons/hicolor/"
			   ;; ;; 					      res-x "/apps"))
			   ;; ;; 		     (src-file (string-append "./Installer/renoise-"
			   ;; ;; 					      res ".png"))
			   ;; ;; 		     (dest-file (string-append dest-dir "/renoise.png")))
			   ;; ;; 		(mkdir-p dest-dir)
			   ;; ;; 		(copy-file src-file dest-file)))
			   ;; ;; 	    '("48" "64" "128"))
			   
			   ;; ;; Installing desktop-menu shortcuts...
			   ;; (invoke (string-append
			   ;; 	   "xdg-desktop-menu install --novendor "
			   ;; 	   resources "/Installer/renoise.desktop")
			   ;; ;; (mkdir-p (string-append target
			   ;; ;; 			  "/share/applications"))
			   ;; ;; (copy-recursively "./Installer/renoise.desktop"
			   ;; ;; 		    (string-append
			   ;; ;; 		     target "/share/applications/renoise.desktop"))
			   
			   ;; ;; Do checks here?
			   
			   ))))))
   
   
   (native-inputs
    (list
     which
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
   (license (license:nonfree (string-append "file:///share/renoise-" version
					    "/Renoise.license")))))

;; run "gcc-unhidden:lib" to get the libs
(define-public gcc-unhidden
  (package
   (inherit gcc)
   (name "gcc-unhidden")
   (properties (alist-delete 'hidden? (package-properties gcc)))))


