(define-module (yumi packages cargo)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io))

;; TODO: figure out a way to not have to specify version when
;;  inheriting packages from crates-io and etc.
;; TODO: use paths in repo for #:cargo-inputs

(define-public hearth-0.0
  (let ((commit "90eb95f62a32981cb662b0f750027231d8a2586b"))
    (package
      (name "hearth")
      (version "0.0")                   ; TOFIX
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hearth-rs/hearth")
               (commit commit)))
         ;; file-name?
         ;; https://guix.gnu.org/cookbook/en/html_node/Extended-example.html
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 "1w503n9rxja3h8ls6p5xsly8aclbp30dm4hd0525bvpbippi161v"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-test-flags
         (list "--release")
         #:cargo-inputs
         (("rust-glam" ,rust-glam-0.20)         ; features: bytemuck,serde
          ("rust-bytemuck" ,rust-bytemuck-1.13) ;features: derive
          ("rust-flume" ,rust-flume-0.11)
          ;;; TOFIX
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
          ("rust-tracing" ,rust-tracing-0.1.37)
          ("rust-wasmtime" ,rust-wasmtime-11))))
      (home-page "https://github.com/hearth-rs/hearth")
      (synopsis "Shared execution environment for constructing 3D virtual spaces from the inside.")
      (description "Hearth is a shared, always-on execution environment for constructing 3D virtual spaces from the inside.")
      (license license:gpl3+))))

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

hearth-0.0
