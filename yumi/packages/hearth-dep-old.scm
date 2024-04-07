(define-module (yumi packages hearth-dep-old)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics) ; rust-image ?
  #:use-module (gnu packages pkg-config) ; pkg-config native-inputs ?
  )

;; TODO: figure out a way to not have to specify version when
;;  inheriting packages from crates-io and etc.
;; TODO: use paths in repo for #:cargo-inputs
;; TODO: inherit dependencies and adjust their #:cargo-build-flags to
;; have "--features pkgname"?

;; To post:
;; TITLE: Network error when cargo-build-system fetches external repositories specified as dependencies in Cargo.toml
;;
;; Hello,
;; I am trying to package a Rust program using the cargo-build-system.
;; 
;; During the build phase, it tries to fetch a dependency specified in the Cargo.toml file, but fails with a network issue. But when built outside of Guix with "cargo build --release", it builds without error. It's almost as though the cargo-build-system has some kind of network sandbox that prevents connecting to websites during the build phase...
;;
;; Error logs during the build:
;;
;; starting phase `build'
;;     Updating git repository `https://github.com/hearth-rs/msdfgen-rs`
;; warning: spurious network error (3 tries remaining): failed to resolve address for github.com: Temporary failure in name resolution; class=Net (12)
;; warning: spurious network error (2 tries remaining): failed to resolve address for github.com: Temporary failure in name resolution; class=Net (12)
;; warning: spurious network error (1 tries remaining): failed to resolve address for github.com: Temporary failure in name resolution; class=Net (12)
;; error: failed to get `msdfgen` as a dependency of package `font-mud v0.1.0 (/tmp/guix-build-rust-font-mud-9999.drv-0/source)`
;;
;; The Cargo.toml file with the dependency looks like this:
;;
;; [dependencies.msdfgen]
;; git = "https://github.com/hearth-rs/msdfgen-rs"
;; branch = "fix-mac-builds"
;; default-features = false
;; features = ["ttf-parser", "png"]
;;
;; I found that the same error occurs reguardless of the website it tries to fetch from, so it's not an issue with the website. It is also not an issue with my network.
;;
;; My assumption is that either cargo-build-system disallows connecting to websites during the build phase, or that using "git = <url>" to specify a cargo dependency in Cargo.toml is buggy with cargo-build-system.
;;
;; Perhaps there is a way to override processing this dependency in Cargo.toml in my package definition? And then use a separate package definition to replace that dependency?
;;
;; The program I'm trying to package is here: https://git.disroot.org/hearth/font-mud
;; And here is my package definition:
;;
;; (define rust-font-mud-9999
;;   (package
;;     (name "rust-font-mud")
;;     (version "9999")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://git.disroot.org/hearth/font-mud")
;;              (commit "c1e6b66f459e32ee90de2a1c29b8a2124a1a9bad")))
;;        (file-name (string-append name "-" version ".tar.gz"))
;;        (sha256
;;         (base32 "19a9lra546f91a3lvjjr7y9yah8q2df8754n5ch9vwdm5hdplf54"))))
;;     (build-system cargo-build-system)
;;     (home-page "https://git.disroot.org/hearth/font-mud")
;;     (synopsis "A library for dynamically generating and packing MSDFs of text glyphs")
;;     (description "A library for dynamically generating and packing MSDFs of text glyphs.")
;;     (license license:asl2.0)))
;;
;; Any help would be greatly appreciated.
;;
;; Ari
;;




;; Dependencies for Hearth:

(define rust-bytemuck-1.13
  (package
    (inherit rust-bytemuck-1)
    (version "1.13")))

(define rust-flume-0.11
  (package
    (inherit rust-flume-0.10)
    (version "0.11")))

(define rust-ouroboros-0.18
  (package
    (inherit rust-ouroboros-0.15)
    (version "0.18")))

(define rust-tracing-0.1.37
  (package
    (inherit rust-tracing-0.1)
    (version "0.1.37")))

(define rust-wasmtime-11    ; doesn't exist in gnu/packages/crates.io ?
  (package
    (name "rust-wasmtime")
    (version "11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qig9fcdqf07mzzpkicm5wgxv0zpr28njdsqf708wxq27yf6k1iw"))))
    (build-system cargo-build-system)
    (home-page "https://wasmtime.dev/")
    (synopsis " A fast and secure runtime for WebAssembly ")
    (description "This package provides a fast and secure runtime for WebAssembly.")
    (license license:asl2.0)))

;; ;; test if i can build the font-mud dependency
(define rust-font-mud-9999
  (package
    (name "rust-font-mud")
    (version "9999")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.disroot.org/hearth/font-mud")
             (commit "c1e6b66f459e32ee90de2a1c29b8a2124a1a9bad")))
       ;; file-name?
       ;; https://guix.gnu.org/cookbook/en/html_node/Extended-example.html
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19a9lra546f91a3lvjjr7y9yah8q2df8754n5ch9vwdm5hdplf54"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (
        ("rust-glam" ,rust-glam-0.20)
        ("rust-ttf-parser" ,rust-ttf-parser-0.19) ; needs 0.19.2
        ("rust-rect-packer" ,rust-rect-packer-0.2.1)
        ("msdfgen-rs-9999" ,msdfgen-rs-9999)) ; custom depend
       #:cargo-development-inputs
       (
        ("rust-png" ,rust-png-0.17))))
    (home-page "https://git.disroot.org/hearth/font-mud")
    (synopsis "A library for dynamically generating and packing MSDFs of text glyphs")
    (description "A library for dynamically generating and packing MSDFs of text glyphs")
    (license license:asl2.0)))

(define rust-rect-packer-0.2.1
  (package
    (name "rust-rect-packer")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rect_packer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i8wrfdgwcpdcqm7gwl6w5n5j3bnh325srj7111cq0abvbgv9zyq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/kryptan/rect_packer")
    (synopsis "A library for rectangle packing.")
    (description "A library for rectangle packing.")
    (license license:expat)))

(define msdfgen-rs-9999
  (package
    (name "msdfgen-rs")
    (version "9999")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hearth-rs/msdfgen-rs/tree/fix-mac-builds")
             (commit "ac254fb7a3ffc9a8e4ca9acf7929b5128b8281e6")))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16lcvn1fmqajci1k3j3psc008v7a3nw0j25247970nsr2z7g66x4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ttf-parser" ,rust-ttf-parser-0.19)
        ("rust-bytemuck" ,rust-bytemuck-1) ; features extern_crate_alloc
        ("rust-typeface" ,rust-typeface-0.3.3)
        ("rust-bindgen" ,rust-bindgen-0.63) ; msdfgen-sys depend
        ("rust-font" ,rust-font-0.15)
        ("rust-freetype-rs" ,rust-freetype-rs-0.32)
        ("rust-png" ,rust-png-0.17)
        )
       #:cargo-development-inputs
       (("rust-all-asserts" ,rust-all-asserts-2)
        ("rust-ttf-parser" ,rust-ttf-parser-0.19)
        ("rust-notosans" ,rust-notosans-0.1)
        ("rust-material-icons" ,rust-material-icons-0.2)
        )
       #:features '("ttf-parser" "png")))
    (home-page "https://github.com/hearth-rs/msdfgen-rs")
    (synopsis "Rust bindings to msdfgen library.")
    (description "Rust bindings to msdfgen library.")
    (license license:asl2.0)
    ))

(define-public rust-typeface-0.3.3
  (package
    (name "rust-typeface")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typeface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06h3201c1l5iqv1a74zcbd6j5j6538w44dlfd29j8h5c6n3dqjjd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bodoni/typeface")
    (synopsis "The package provides a foundation for parsing fonts.")
    (description "The package provides a foundation for parsing fonts.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-font-0.15
  (package
    (name "rust-font")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "font" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1al23fkbb845a8mzc0g4gpr3xkpddx8sdjk90l3s02h556n7mbjx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-opentype" ,rust-opentype-0.24)
                       ("rust-typeface" ,rust-typeface-0.4)
                       ("rust-webtype" ,rust-webtype-0.2))))
    (home-page "https://github.com/bodoni/font")
    (synopsis "The package provides a builder and parser of fonts.")
    (description "The package provides a builder and parser of fonts.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-opentype-0.24
  (package
    (name "rust-opentype")
    (version "0.24.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentype" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qc9cb0g9kabxnhqgmcnm3a84x454q2a6nbxl78w54arw9r6sj56"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-postscript" ,rust-postscript-0.17)
                       ("rust-truetype" ,rust-truetype-0.42)
                       ("rust-typeface" ,rust-typeface-0.4))))
    (home-page "https://github.com/bodoni/opentype")
    (synopsis "The package provides a builder and parser of OpenType fonts.")
    (description
     "The package provides a builder and parser of @code{OpenType} fonts.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-all-asserts-2
  (package
    (name "rust-all-asserts")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "all_asserts" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0051psbhdpz4zmm68bjzmmy2p29x6shws0x1rmsc4mqhrbqclxya"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ohsayan/all_asserts")
    (synopsis
     "A crate for multiple types of asserts that don't exist in the standard library")
    (description
     "This package provides a crate for multiple types of asserts that don't exist in
the standard library")
    (license license:asl2.0)))

(define-public rust-notosans-0.1
  (package
    (name "rust-notosans")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notosans" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p10x0hlhy710j33bk6l3l2yyd7j38wahxh5d7abv9n8py5mfk80"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nannou-org/notosans")
    (synopsis
     "A simple crate providing several of Google's Noto Sans true type fonts as slices of bytes.")
    (description
     "This package provides a simple crate providing several of Google's Noto Sans
true type fonts as slices of bytes.")
    (license (list license:expat license:asl2.0))))

(define-public rust-material-icons-0.2
  (package
    (name "rust-material-icons")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "material-icons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cx4j02fnc8z3plw3chqxyyz0pdq5kbdi5bfm3hf504whnjnpink"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-enum-iterator" ,rust-enum-iterator-0.8))))
    (home-page "https://github.com/fschutt/material-icons")
    (synopsis
     "Strongly-typed codepoint mappings for the Google Material Icons font")
    (description
     "Strongly-typed codepoint mappings for the Google Material Icons font")
    (license license:asl2.0)))



;; hearth-9999

;; rust-font-mud-9999

msdfgen-rs-9999 ; font-mud depend
