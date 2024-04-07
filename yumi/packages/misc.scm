(define-module (yumi packages misc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (srfi srfi-1)
  ;; test
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary) ; for binary-build-system
  ) ; for alist-delete

;; reference file:
;; https://github.com/daviwil/channel-x/blob/master/channel-x/packages/video.scm

;; note: make sure to git commit before updating channels

;; ask guix irc: i want to access gcc's hidden libraries, so I've created a custom package
;; definition that inherits gcc, then runs (alist-delete 'hidden? (package-properties gcc))
;; to install gcc with the hidden libs included. This workaround requires having to compile
;; gcc and it's not a great solution. I'm pretty inexperienced when it comes to hacking guix,
;; but is there a better way to write a personal package definiton, whose sole purpose is to
;; get the hidden libs in gcc? I know that
;; guix shell  -e $'(list (@@ (gnu packages gcc) gcc) "lib")'
;; does the job in a shell, but i want to install the libs in a custom profile.
;; maybe a package definiton that returns only the gcc libs, so essentially "gcc:lib" (this
;; output is no longer available in the package repos)
;; relevant discussions: https://issues.guix.gnu.org/63267

;; "inputs"?
;; https://unix.stackexchange.com/questions/600311/how-to-run-a-dynamically-compiled-32-bit-x86-binary-on-a-amd64-bit-guix-system

;; run "gcc-unhidden:lib" to get the libs
(define-public gcc-unhidden
  (package
   (inherit gcc)
   (name "gcc-unhidden")
   (properties (alist-delete 'hidden? (package-properties gcc)))
   ))

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

(define-public cagebreak-xkb-fix
  (package
    (inherit cagebreak)
    (name "cagebreak-xkb-fix")
    (inputs
     (modify-inputs (package-inputs cagebreak)
                    (append libxkbcommon)))))
