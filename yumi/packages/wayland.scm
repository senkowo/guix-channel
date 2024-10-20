(define-module (yumi packages wayland))

(use-modules (guix build-system gnu) ; make
             (guix packages) ; package
             (guix download) ; url-fetch
             ((guix licenses) #:prefix license:)
             (gnu packages freedesktop) ; wayland,libinput
             (gnu packages wm) ; wlroots
             (gnu packages pkg-config) ; pkg-config
             (guix git-download) ; git-fetch
             (guix build-system cargo) ; cargo
             (gnu packages crates-graphics) ; rust-wayland-client ?
             )

;;; To package:
;; wideriver/filtile?, lswt, flow, river-bedload?, networkmanager_dmenu (nix?), swhkd, 
;;

;;; Question for guix irc:
;; Hello, I'm planning to publish my first package to the main Guix repo, but want to clarify some things to ensure I do it correctly.
;; Question 1: will there be an expectation for me to maintain that package in the Guix repo?
;; Quettion 2: 

;; Wanting to start contributing packages to Guix repos.
;; First time doing something like this, so I want to make sure I do it properly.
;; Is there an obligation to keep those packages maintained?
;; Advice for a git novice? (only know enough to manage personal repos)
;; - e.g. git pull before any new changes to it, use specific kind of merging,
;;   commit signing, git push after changes?
;;   (check docs first for answers before posting)
;; Ettiquette: is it ettiquette to ask original project devs before submitting
;; package build?

(define-public wideriver-1.2.0
  (package
    (name "wideriver")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-courtis/wideriver")
             (commit version)))
       (sha256
        (base32
         "0mdnn35iymh4i77xzdfwha0hlibqs3y0b4al6acw9523z5yw6z96"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; (delete 'check)
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (inputs (list wayland wlroots libinput))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/alex-courtis/wideriver")
    (synopsis "Tiling window manager for the river wayland compositor")
    (description "Tiling window manager for the river wayland compositor, inspired by dwm and xmonad.")
    (license license:expat)))

(define-public lswt-1.0.4
  (package
    (name "lswt")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~leon_plickat/lswt")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0kb0167bkmwrz6441arinc00ygmaz5wgsaj7kjrhgs3rqpp1mg1s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; (delete 'check)
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (inputs (list wayland))
    ;; (native-inputs (list pkg-config)) ; not needed ig
    (home-page "https://git.sr.ht/~leon_plickat/lswt")
    (synopsis "list Wayland toplevels")
    (description "List Wayland toplevels in human readable and machine parseable formats.")
    (license license:gpl3)))

;; dependency for rust-river-flow
(define-public rust-pico-args-0.5
  (package
    (name "rust-pico-args")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pico-args" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RazrFalcon/pico-args")
    (synopsis "An ultra simple CLI arguments parser.")
    (description "An ultra simple CLI arguments parser.")
    (license license:expat)))

;; TODO: copy over dependencies rust-wayland-client rust-wayland-scanner rust-wayland-protocols from crates-graphics over to here? is this the right approach to it?
(define-public rust-river-flow-0.2.0
  (package
    (name "rust-river-flow")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stefur/flow")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1hg682np9as7qc5jjgr18ac17hy7n0hm55qyvi0njcczww93llsm"))))
    (arguments
     `(#:cargo-inputs (("rust-pico-args" ,rust-pico-args-0.5)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.31))))
    (build-system cargo-build-system)
    (home-page "https://github.com/stefur/flow")
    (synopsis "Small utility that brings some extra commands to control river")
    (description "Small utility that brings some extra commands to control river.")
    (license license:expat)))

