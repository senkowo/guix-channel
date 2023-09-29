(define-module (my-packages desktop)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake))

;; reference file:
;; https://github.com/daviwil/channel-x/blob/master/channel-x/packages/video.scm

(define-public alltray
  (package
   (name "alltray")
   (version "0.70")
   (source (origin
            (uri (git-reference
		  (url "https://github.com/mbt/alltray")
		  (commit (string-append "v" version))))
	    (method git-fetch)
            (sha256
             (base32
              "0aac5xy94cdi9wzh7ljns6scpqjrm1lgphmc9fch0bx57x8j6xlm"))))
   (build-system cmake-build-system)
   (synopsis "Tray all application")
   (description "Tray all application")
   (home-page "https://github.com/liuyug/alltray")
   (license gpl3+)))
