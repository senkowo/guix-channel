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
            (method url-fetch)
            (uri (git-reference
		  (url "https://github.com/mbt/alltray")
		  (commit version)))
            (sha256
             (base32
              "1h0jqxn5rwm44w63kg56b3lsw1nr44lg966x0r62mzxy0mwd354g"))))
   (build-system cmake-build-system)
   (synopsis "Tray all application")
   (description "Tray all application")
   (home-page "https://github.com/liuyug/alltray")
   (license gpl3+)))
