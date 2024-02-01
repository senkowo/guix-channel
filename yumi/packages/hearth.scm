(define-module (yumi packages hearth)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics) ; rust-image?
  ;; #:use-module (gnu packages pkg-config) ; pkg-config native inputs?

  #:use-module (yumi packages hearth-dep)
  )

(define-public hearth-9999
  (let ((commit "9a392fa80b8d4b5a6039ddc53b593accdd7ca7d8"))
    (package
      (name "hearth")
      (version "9999")                  ; TOFIX
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hearth-rs/hearth/")
               (commit commit)))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 "03d0x3ifcdqqz20d09pv95116q8cvcp909jnisf5f8z2iqwhl4fp"))))
      (build-system cargo-build-system)
      (arguments
       `(;; #:tests? #f
         #:cargo-inputs
         (
          ("rust-bytemuck" ,rust-bytemuck-1) ; 1.13 features: derive
          ("rust-flume" ,rust-flume-0.11)
          ("rust-glam" ,rust-glam-0.20)         ; features: bytemuck,serde
          
          ;; TOFIX (how to fix?)
          ;; - define a package 
          ;; ("hearth-canvas.path" "plugins/canvas")
          ;; ("hearth-canvas.path" "plugins/canvas")
          ;; ("hearth-daemon.path" "plugins/daemon")
          ;; ("hearth-debug-draw.path" "plugins/debug-draw")
          ;; ("hearth-init.path" "plugins/init")
          ;; ("hearth-ipc.path" "core/ipc")
          ;; ("hearth-fs.path" "plugins/fs")
          ;; ("hearth-macros.path" "core/macros")
          ;; ("hearth-network.path" "plugins/network")
          ;; ("hearth-rend3.path" "plugins/rend3")
          ;; ("hearth-renderer.path" "plugins/renderer")
          ;; ("hearth-runtime.path" "core/runtime")
          ;; ("hearth-schema.path" "core/schema")
          ;; ("hearth-terminal.path" "plugins/terminal")
          ;; ("hearth-time.path" "plugins/time")
          ;; ("hearth-wasm.path" "plugins/wasm")
          ;; ("kindling-build.path" "kindling/build")
          
          ("rust-ouroboros" ,rust-ouroboros-0.18)
          ("rust-parking_lot" ,rust-parking-lot-0.12)
          ("rust-serde_json" ,rust-serde-json-1) ; TOFIX default-features=f, features:derive
          ("rust-tracing" ,rust-tracing-0.1) ; 0.1.37
          ("rust-wasmtime" ,rust-wasmtime-11)

          ;; ("rust-font-mud" ,rust-font-mud-9999) ; defined below
          )))
      ;; (native-inputs (list pkg-config))
      (home-page "https://github.com/hearth-rs/hearth")
      (synopsis "Shared execution environment for constructing 3D virtual spaces from the inside.")
      (description "Hearth is a shared, always-on execution environment for constructing 3D virtual spaces from the inside.")
      (license license:gpl3+))))

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

hearth-9999
