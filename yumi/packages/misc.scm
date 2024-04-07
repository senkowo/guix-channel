(define-module (yumi packages misc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gcc)
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
    (name "cagebreak")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/project-repo/cagebreak")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0firjpp7qw4kb2h1zh5pv5k0xf0jvx6x0r0s7j6y7dhlh5j0s00q"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dxwayland=true" "-Dman-pages=true")
      ;; XXX: Running cagebreak tests need more tools, such as: clang-format,
      ;; shellcheck, git, gnupg ...
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-data-dir
            (lambda _
              (substitute* '("cagebreak.c" "meson.build")
                (("/etc/") (string-append #$output "/etc/"))
                (("/usr/share/") (string-append #$output "/usr/share/"))))))))
    (native-inputs (list pkg-config scdoc))
    (inputs (list libevdev pango wlroots libxkbcommon))
    (home-page "https://github.com/project-repo/cagebreak")
    (synopsis "Tiling wayland compositor inspired by ratpoison")
    (description
     "@command{cagebreak} is a slim, keyboard-controlled, tiling compositor
for wayland conceptually based on the X11 window manager
@command{ratpoison}.")
    (license license:expat)))
