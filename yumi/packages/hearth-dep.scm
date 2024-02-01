(define-module (yumi packages hearth-dep)
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

;;; Commentary:

;; These are almost all the dependencies that are needed by Hearth.
;; Because of the limited number and variety of guix cargo packages
;; in the main repos, a lot of them had to be defined here.
;; There are a lot of dependencies here.
;; Most of these were created using "guix import cargo <pkg>@<ver>".

;;; Hearth dependency
(define-public rust-bytemuck-1.13
  (package
    (name "rust-bytemuck")
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytemuck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sifp93886b552fwbywmp5f4gysar7z62mhh4y8dh5gxhkkbrzhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytemuck-derive" ,rust-bytemuck-derive-1))))
    (home-page "https://github.com/Lokathor/bytemuck")
    (synopsis "A crate for mucking around with piles of bytes.")
    (description
     "This package provides a crate for mucking around with piles of bytes.")
    (license (list license:zlib license:asl2.0 license:expat))))

;;; Hearth dependency
(define-public rust-flume-0.11
  (package
    (name "rust-flume")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flume" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10girdbqn77wi802pdh55lwbmymy437k7kklnvj12aaiwaflbb2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-spin" ,rust-spin-0.9))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                                   ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-waker-fn" ,rust-waker-fn-1))))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "A blazingly fast multi-producer channel")
    (description
     "This package provides a blazingly fast multi-producer channel")
    (license (list license:asl2.0 license:expat))))

;;; Hearth dependency
(define-public rust-ouroboros-0.18
  (package
    (name "rust-ouroboros")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ouroboros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g2pn23qpmm4w9my80j9hc01szrgpyrg4gxyyi9bfqili9dbxdwp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aliasable" ,rust-aliasable-0.1)
                       ("rust-ouroboros-macro" ,rust-ouroboros-macro-0.18)
                       ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/someguynamedjosh/ouroboros")
    (synopsis "Easy, safe self-referential struct generation.")
    (description "Easy, safe self-referential struct generation.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-ouroboros-0.18
(define-public rust-ouroboros-macro-0.18
  (package
    (name "rust-ouroboros-macro")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ouroboros_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cqzvszvvm7ad0m2kr4jv82v58x2f6idzl4j992jr70ibzgdqidn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.10)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/someguynamedjosh/ouroboros")
    (synopsis "Proc macro for ouroboros crate.")
    (description "Proc macro for ouroboros crate.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-ouroboros-macro-0.18
(define-public rust-itertools-0.12
  (package
    (name "rust-itertools")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-permutohedron" ,rust-permutohedron-0.2)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-ouroboros-macro-0.18
(define-public rust-proc-macro2-diagnostics-0.10
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2-diagnostics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-1))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/SergioBenitez/proc-macro2-diagnostics")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-proc-macro2-diagnostics-0.10
(define-public rust-yansi-1
  (package
    (name "rust-yansi")
    (version "1.0.0-rc.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yansi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xr3n41j5v00scfkac2d6vhkxiq9nz3l5j6vw8f3g3bqixdjjrqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-is-terminal" ,rust-is-terminal-0.4))))
    (home-page "https://github.com/SergioBenitez/yansi")
    (synopsis "A dead simple ANSI terminal color painting library.")
    (description
     "This package provides a dead simple ANSI terminal color painting library.")
    (license (list license:expat license:asl2.0))))

;;; Hearth dependency
(define-public rust-wasmtime-11
  (package
    (name "rust-wasmtime")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11f6rxh7s2bxvd8ia9k647sbgqkx260a6i8q90ihhvnzyjx570h2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fxprof-processed-profile" ,rust-fxprof-processed-profile-0.6)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-psm" ,rust-psm-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmtime-cache" ,rust-wasmtime-cache-11)
                       ("rust-wasmtime-component-macro" ,rust-wasmtime-component-macro-11)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-11)
                       ("rust-wasmtime-cranelift" ,rust-wasmtime-cranelift-11)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11)
                       ("rust-wasmtime-fiber" ,rust-wasmtime-fiber-11)
                       ("rust-wasmtime-jit" ,rust-wasmtime-jit-11)
                       ("rust-wasmtime-runtime" ,rust-wasmtime-runtime-11)
                       ("rust-wasmtime-winch" ,rust-wasmtime-winch-11)
                       ("rust-wat" ,rust-wat-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "High-level API to expose the Wasmtime runtime")
    (description "High-level API to expose the Wasmtime runtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-fxprof-processed-profile-0.6
  (package
    (name "rust-fxprof-processed-profile")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fxprof-processed-profile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ggsn3im2bfcnxic0jzk00qgiacfrg2as6i4d8kj87kzxl52rl97"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-debugid" ,rust-debugid-0.8)
                       ("rust-fxhash" ,rust-fxhash-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-assert-json-diff" ,rust-assert-json-diff-2))))
    (home-page "https://github.com/mstange/samply/")
    (synopsis
     "Create profiles in the Firefox Profiler's processed profile JSON format.")
    (description
     "Create profiles in the Firefox Profiler's processed profile JSON format.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-fxprof-processed-profile-0.6
(define-public rust-assert-json-diff-2
  (package
    (name "rust-assert-json-diff")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "assert-json-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04mg3w0rh3schpla51l18362hsirl23q93aisws2irrj32wg5r27"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-version-sync" ,rust-version-sync-0.8))))
    (home-page "https://github.com/davidpdrsn/assert-json-diff")
    (synopsis "Easily compare two JSON values and get great output")
    (description "Easily compare two JSON values and get great output")
    (license license:expat)))

;; dependency for rust-wasmtime-11
(define-public rust-wasmparser-0.107
  (package
    (name "rust-wasmparser")
    (version "0.107.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jvs1ps5iixn16r9s725wg37qw6nsbbaalksmh6dlz8cg2dsrqr9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-semver" ,rust-semver-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-wasm-encoder" ,rust-wasm-encoder-0.29))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "A simple event-driven library for parsing WebAssembly binary files.
")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-cache-11
  (package
    (name "rust-wasmtime-cache")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-cache" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02mk0x7fr5yg392i774yq5nwph723lnnyvz0ip0k1ddd6d88wc1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-directories-next" ,rust-directories-next-2)
                       ("rust-file-per-thread-logger" ,rust-file-per-thread-logger-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-windows-sys" ,rust-windows-sys-0.48)
                       ("rust-zstd" ,rust-zstd-0.11))
       #:cargo-development-inputs (("rust-filetime" ,rust-filetime-0.2)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Support for automatic module caching with Wasmtime")
    (description "Support for automatic module caching with Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-cache-11
(define-public rust-file-per-thread-logger-0.2
  (package
    (name "rust-file-per-thread-logger")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "file-per-thread-logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ww3yqf7mdpdwb9b75rcbmxw3zg6wnny9jn8604sz2dg6cfc4g4a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/bnjbvr/file-per-thread-logger")
    (synopsis
     "A logging implementation that writes logs in one file per thread")
    (description
     "This package provides a logging implementation that writes logs in one file per
thread")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-component-macro-11
  (package
    (name "rust-wasmtime-component-macro")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-component-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yc41wfq6h7m4001l3lpk5qg20q92pk4aj30w3vysqp1skzaac8n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-11)
                       ("rust-wasmtime-wit-bindgen" ,rust-wasmtime-wit-bindgen-11)
                       ("rust-wit-parser" ,rust-wit-parser-0.8))
       #:cargo-development-inputs (("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Macros for deriving component interface types from Rust types")
    (description
     "Macros for deriving component interface types from Rust types")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-component-macro-11
(define-public rust-wasmtime-wit-bindgen-11
  (package
    (name "rust-wasmtime-wit-bindgen")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-wit-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fqv66rwma17lw0danclldq7sxabszn5kp7hc2z1m2z6ngj8vndf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-wit-parser" ,rust-wit-parser-0.8))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Internal `*.wit` support for the `wasmtime` crate's macros")
    (description "Internal `*.wit` support for the `wasmtime` crate's macros")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-component-macro-11
(define-public rust-wit-parser-0.8
  (package
    (name "rust-wit-parser")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wit-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w2hy3byzgwvr8dg9cc61kkvnyrjrqpbkvj383ls1bnvjgqckbkd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-id-arena" ,rust-id-arena-2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-rayon" ,rust-rayon-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wit-parser")
    (synopsis
     "Tooling for parsing `*.wit` files and working with their contents.
")
    (description
     "Tooling for parsing `*.wit` files and working with their contents.")
    (license (list license:asl2.0))))

;; dependency for rust-wit-parser-0.8
(define-public rust-id-arena-2
  (package
    (name "rust-id-arena")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "id-arena" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01ch8jhpgnih8sawqs44fqsqpc7bzwgy0xpi6j0f4j0i5mkvr8i5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/fitzgen/id-arena")
    (synopsis "A simple, id-based arena.")
    (description "This package provides a simple, id-based arena.")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-component-util-11
  (package
    (name "rust-wasmtime-component-util")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-component-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w6vb900b2r34k7pn7apqy0kxrhvm47aspdzaal2mklfdlf6pg9i"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "Utility types and functions to support the component model in Wasmtime")
    (description
     "Utility types and functions to support the component model in Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-cranelift-11
  (package
    (name "rust-wasmtime-cranelift")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-cranelift" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0001nk5nya2sjc2dzls6jhcjvlpwflnfhpladm4bpynrvgzk6qv9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-cranelift-control" ,rust-cranelift-control-0.98)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.98)
                       ("rust-cranelift-frontend" ,rust-cranelift-frontend-0.98)
                       ("rust-cranelift-native" ,rust-cranelift-native-0.98)
                       ("rust-cranelift-wasm" ,rust-cranelift-wasm-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmtime-cranelift-shared" ,rust-wasmtime-cranelift-shared-11)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Integration between Cranelift and Wasmtime")
    (description "Integration between Cranelift and Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-cranelift-11
(define-public rust-cranelift-control-0.98
  (package
    (name "rust-cranelift-control")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-control" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ylxf9fjf2j9jmsvqj50j31c3blipb11w69a1v7x7mi9w78m9fr6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "White-box fuzz testing framework")
    (description "White-box fuzz testing framework")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-cranelift-11
(define-public rust-cranelift-frontend-0.98
  (package
    (name "rust-cranelift-frontend")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-frontend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wndzw8653m4p7410nfcf58jbz7dccy58s4fbpw1jkhg9z2iscpm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))
       #:cargo-development-inputs (("rust-similar" ,rust-similar-2))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Cranelift IR builder helper")
    (description "Cranelift IR builder helper")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-cranelift-11 and 1 other
(define-public rust-cranelift-native-0.98
  (package
    (name "rust-cranelift-native")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-native" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vyy5b9bsrz5i719aq7i9mk5kfdsnqm01h68ddwv30mrn0fzm6w0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Support for targeting the host with Cranelift")
    (description "Support for targeting the host with Cranelift")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-cranelift-11
(define-public rust-cranelift-wasm-0.98
  (package
    (name "rust-cranelift-wasm")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-wasm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hfxqf7dxq44km1gmgaxps4hfqxikngpdvpw77l0jxqcxrrr4bw9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.98)
                       ("rust-cranelift-frontend" ,rust-cranelift-frontend-0.98)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmtime-types" ,rust-wasmtime-types-11))
       #:cargo-development-inputs (("rust-target-lexicon" ,rust-target-lexicon-0.12)
                                   ("rust-wat" ,rust-wat-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Translator from WebAssembly to Cranelift IR")
    (description "Translator from @code{WebAssembly} to Cranelift IR")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-environ-11
  (package
    (name "rust-wasmtime-environ")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-environ" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sgz9q12w1gidxnr4ra39s7my27hz3krm43wdjh73gvz1vxb528i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-encoder" ,rust-wasm-encoder-0.29)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmprinter" ,rust-wasmprinter-0.2)
                       ("rust-wasmtime-component-util" ,rust-wasmtime-component-util-11)
                       ("rust-wasmtime-types" ,rust-wasmtime-types-11))
       #:cargo-development-inputs (("rust-atty" ,rust-atty-0.2)
                                   ("rust-clap" ,rust-clap-3)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-wat" ,rust-wat-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "Standalone environment support for WebAsssembly code in Cranelift")
    (description
     "Standalone environment support for @code{WebAsssembly} code in Cranelift")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-environ-11 and 2 others
(define-public rust-cranelift-entity-0.98
  (package
    (name "rust-cranelift-entity")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-entity" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "133h1dyg8samzz6gbxy5nx9kblnq488l68mxn4d52si4935mcmjd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Data structures using entity references as mapping keys")
    (description "Data structures using entity references as mapping keys")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-environ-11 and 1 other
(define-public rust-wasm-encoder-0.29
  (package
    (name "rust-wasm-encoder")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-encoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1098j622k5mgcjqfdgdr3j3pqdxq81jk3gir59hz7szajayivi0q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-leb128" ,rust-leb128-0.2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-encoder")
    (synopsis "A low-level WebAssembly encoder.
")
    (description
     "This package provides a low-level @code{WebAssembly} encoder.")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-environ-11
(define-public rust-wasmprinter-0.2
  (package
    (name "rust-wasmprinter")
    (version "0.2.78")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmprinter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yjpmmhmdf6cj10pb2x80iljndhxxd91v2lryv9n9p4zql9jrqq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.121))
       #:cargo-development-inputs (("rust-diff" ,rust-diff-0.1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmprinter")
    (synopsis
     "Rust converter from the WebAssembly binary format to the text format.
")
    (description
     "Rust converter from the @code{WebAssembly} binary format to the text format.")
    (license (list license:asl2.0))))

;; dependency for rust-wasmprinter-0.2 and 1 other
(define-public rust-wasmparser-0.121
  (package
    (name "rust-wasmparser")
    (version "0.121.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a7x5ga40w3y2vdfw4lfgw6afyz70gj5mahwrf117cvac2kzcg4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-semver" ,rust-semver-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-wasm-encoder" ,rust-wasm-encoder-0.41))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "A simple event-driven library for parsing WebAssembly binary files.
")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-environ-11
(define-public rust-wasmtime-types-11
  (package
    (name "rust-wasmtime-types")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17dnb3vbfscxhl842kb2k5d943bhh8a141q3pcy37ba1z6nrmh9a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-entity" ,rust-cranelift-entity-0.98)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.107))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "WebAssembly type definitions for Cranelift")
    (description "@code{WebAssembly} type definitions for Cranelift")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-fiber-11
  (package
    (name "rust-wasmtime-fiber")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-fiber" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ib5c2h4f7v9cvl86w1z61xpb36qkxirczs7wgz8249s2512vqp0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-wasmtime-asm-macros" ,rust-wasmtime-asm-macros-11)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Fiber support for Wasmtime")
    (description "Fiber support for Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-jit-11
  (package
    (name "rust-wasmtime-jit")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-jit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hk896lxf622gyyj39fkx86isbq94dcmqi6jwpd2amqcjbj09fdk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-addr2line" ,rust-addr2line-0.19)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.3)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-ittapi" ,rust-ittapi-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11)
                       ("rust-wasmtime-jit-debug" ,rust-wasmtime-jit-debug-11)
                       ("rust-wasmtime-jit-icache-coherence" ,rust-wasmtime-jit-icache-coherence-11)
                       ("rust-wasmtime-runtime" ,rust-wasmtime-runtime-11)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "JIT-style execution for WebAsssembly code in Cranelift")
    (description
     "JIT-style execution for @code{WebAsssembly} code in Cranelift")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-jit-11
(define-public rust-wasmtime-jit-debug-11
  (package
    (name "rust-wasmtime-jit-debug")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-jit-debug" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ksdsijmq1xjiq0pqmvvvgznqqpl89z8b3r9ppb7bv7dmbbqw8n7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-object" ,rust-object-0.30)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-0.37))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "JIT debug interfaces support for Wasmtime")
    (description "JIT debug interfaces support for Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-jit-11
(define-public rust-ittapi-0.3
  (package
    (name "rust-ittapi")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ittapi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wbjdpq28dvm5iigi4jmscvz7nf5cmjhgsi2c9wss730jfww1995"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-ittapi-sys" ,rust-ittapi-sys-0.3)
                       ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs (("rust-scoped-env" ,rust-scoped-env-2))))
    (home-page "https://github.com/intel/ittapi/tree/master/rust/ittapi")
    (synopsis "High-level Rust bindings for ittapi")
    (description "High-level Rust bindings for ittapi")
    (license (list license:gpl2 license:bsd-3))))

;; dependency for rust-ittapi-0.3
(define-public rust-ittapi-sys-0.3
  (package
    (name "rust-ittapi-sys")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ittapi-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n972w6wcna94rir1r3k1j0imksqywkx3vk0lqv0a1k56x3mwyyb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))
       #:cargo-development-inputs (("rust-bindgen" ,rust-bindgen-0.68)
                                   ("rust-diff" ,rust-diff-0.1))))
    (home-page "https://github.com/intel/ittapi/tree/master/rust/ittapi-sys")
    (synopsis "Rust bindings for ittapi")
    (description "Rust bindings for ittapi")
    (license (list license:gpl2 license:bsd-3))))

;; dependency for rust-ittapi-0.3
(define-public rust-scoped-env-2
  (package
    (name "rust-scoped-env")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scoped-env" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09n0q6v9plj1s3vmd8s3nh8wclcwirayrx0bnwdswn4hinkkhqx8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Nokel81/scoped-env")
    (synopsis "A lifetime scoped env variable setting")
    (description
     "This package provides a lifetime scoped env variable setting")
    (license (list license:expat))))

;; dependency for rust-scoped-env-2
(define-public rust-bindgen-0.68
  (package
    (name "rust-bindgen")
    (version "0.68.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y40gndyay1fj8d3d8gsd9fyfzjlbghx92i560kmvhvfxc9l6vkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-annotate-snippets" ,rust-annotate-snippets-0.9)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

;; dependency for rust-wasmtime-jit-11
(define-public rust-wasmtime-jit-icache-coherence-11
  (package
    (name "rust-wasmtime-jit-icache-coherence")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-jit-icache-coherence" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1imd4rs33d17lbcmkcmvrvj4iap6qw9f9ki64b1v5gbf9gc50xsi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Utilities for JIT icache maintenance")
    (description "Utilities for JIT icache maintenance")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-runtime-11
  (package
    (name "rust-wasmtime-runtime")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-runtime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fp8qh2lkcm7wdnvf6l8ngw3ndl1bfv62nx1zrfzqc0gszwzg7n8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mach" ,rust-mach-0.3)
                       ("rust-memfd" ,rust-memfd-0.6)
                       ("rust-memoffset" ,rust-memoffset-0.8)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-sptr" ,rust-sptr-0.3)
                       ("rust-wasmtime-asm-macros" ,rust-wasmtime-asm-macros-11)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11)
                       ("rust-wasmtime-fiber" ,rust-wasmtime-fiber-11)
                       ("rust-wasmtime-jit-debug" ,rust-wasmtime-jit-debug-11)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Runtime library support for Wasmtime")
    (description "Runtime library support for Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-runtime-11
(define-public rust-memfd-0.6
  (package
    (name "rust-memfd")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memfd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r5cm3wzyr1x7768h3hns77b494qbz0g05cb9wgpjvrcsm5gmkxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.38))))
    (home-page "https://github.com/lucab/memfd-rs")
    (synopsis "A pure-Rust library to work with Linux memfd and sealing")
    (description
     "This package provides a pure-Rust library to work with Linux memfd and sealing")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-wasmtime-runtime-11
(define-public rust-wasmtime-asm-macros-11
  (package
    (name "rust-wasmtime-asm-macros")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-asm-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n0w0gqqw7l1lnhazfx3acgxl5rvd419xfs0h4x2gpxkiyb6wv77"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Macros for defining asm functions in Wasmtime")
    (description "Macros for defining asm functions in Wasmtime")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-11
(define-public rust-wasmtime-winch-11
  (package
    (name "rust-wasmtime-winch")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-winch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092bdili39rzk78dfcv0asr672kllcvpql61xyk735pci47gi82s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmtime-cranelift-shared" ,rust-wasmtime-cranelift-shared-11)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11)
                       ("rust-winch-codegen" ,rust-winch-codegen-0.9))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Integration between Wasmtime and Winch")
    (description "Integration between Wasmtime and Winch")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-winch-11
(define-public rust-cranelift-codegen-0.98
  (package
    (name "rust-cranelift-codegen")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w0lr6rbxk0ix3c0s5k49nykcd06xw1yingfandwxsvhv3fi9m26"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-capstone" ,rust-capstone-0.9)
                       ("rust-cranelift-bforest" ,rust-cranelift-bforest-0.98)
                       ("rust-cranelift-codegen-meta" ,rust-cranelift-codegen-meta-0.98)
                       ("rust-cranelift-codegen-shared" ,rust-cranelift-codegen-shared-0.98)
                       ("rust-cranelift-control" ,rust-cranelift-control-0.98)
                       ("rust-cranelift-entity" ,rust-cranelift-entity-0.98)
                       ("rust-cranelift-isle" ,rust-cranelift-isle-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regalloc2" ,rust-regalloc2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-souper-ir" ,rust-souper-ir-2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-similar" ,rust-similar-2))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Low-level code generator library")
    (description "Low-level code generator library")
    (license (list license:asl2.0))))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-capstone-0.9
  (package
    (name "rust-capstone")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "capstone" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v19wszrrhidh6z1ms0hda0dl4p0fl2n1mhx5mwkjfffnj03r2qp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-capstone-sys" ,rust-capstone-sys-0.13)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.2)
                                   ("rust-macho" ,rust-macho-0.4)
                                   ("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/capstone-rust/capstone-rs")
    (synopsis
     "High level bindings to capstone disassembly engine (https://capstone-engine.org/)")
    (description "High level bindings to capstone disassembly engine
(https://capstone-engine.org/)")
    (license license:expat)))

;; dependency for rust-capstone-0.9
(define-public rust-macho-0.4
  (package
    (name "rust-macho")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "macho" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "011p1j9jlndbanmqy5y7lk1vn3mh4jk3chgjiw3wwnbfzkicdsvg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nom" ,rust-nom-1)
                       ("rust-uuid" ,rust-uuid-0.3))))
    (home-page "https://github.com/richo/macho-rs")
    (synopsis "Mach-O parser in rust")
    (description "Mach-O parser in rust")
    (license license:expat)))

;; dependency for rust-capstone-0.9
(define-public rust-capstone-sys-0.13
  (package
    (name "rust-capstone-sys")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "capstone-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xpia1gs2b0zl7n521ldq6lax2jqqjw0hz2c8skak94gp2bksbyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.53)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-regex" ,rust-regex-1))))
    (home-page
     "https://github.com/capstone-rust/capstone-rs/tree/master/capstone-sys")
    (synopsis "System bindings to the capstone disassembly library")
    (description "System bindings to the capstone disassembly library")
    (license license:expat)))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-cranelift-bforest-0.98
  (package
    (name "rust-cranelift-bforest")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-bforest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yzd41if59zizq5n5hq0vv86mswsdgjvl80blz6k2fmw18n2zgqf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-entity" ,rust-cranelift-entity-0.98))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "A forest of B+-trees")
    (description "This package provides a forest of B+-trees")
    (license (list license:asl2.0))))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-cranelift-codegen-meta-0.98
  (package
    (name "rust-cranelift-codegen-meta")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen-meta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "164596lfqd6jdpbqc2358a2mhpkj0dhdraj8jzlzmhf0a190dc6i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cranelift-codegen-shared" ,rust-cranelift-codegen-shared-0.98))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Metaprogram for cranelift-codegen code generator library")
    (description "Metaprogram for cranelift-codegen code generator library")
    (license (list license:asl2.0))))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-cranelift-codegen-shared-0.98
  (package
    (name "rust-cranelift-codegen-shared")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-codegen-shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06q84iwzfdln1dalh6d3nxfamh25q5madvdwpvffm9fgb58hnci7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis
     "For code shared between cranelift-codegen-meta and cranelift-codegen")
    (description
     "For code shared between cranelift-codegen-meta and cranelift-codegen")
    (license (list license:asl2.0))))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-cranelift-isle-0.98
  (package
    (name "rust-cranelift-isle")
    (version "0.98.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cranelift-isle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q7yafyyp6p1r56qqlzq35fsg2803d70skn1kjnp16gxaa8izzxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
                       ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page
     "https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/isle")
    (synopsis
     "ISLE: Instruction Selection and Lowering Expressions. A domain-specific language for instruction selection in Cranelift.")
    (description
     "ISLE: Instruction Selection and Lowering Expressions.  A domain-specific
language for instruction selection in Cranelift.")
    (license (list license:asl2.0))))

;; dependency for rust-cranelift-codegen-0.98
(define-public rust-souper-ir-2
  (package
    (name "rust-souper-ir")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "souper-ir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i60q84w5k3rd0j3zhsdc5xasrd4wrkamyrs01rik3lq6g71h355"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-id-arena" ,rust-id-arena-2))))
    (home-page "https://github.com/fitzgen/souper-ir")
    (synopsis "A library for manipulating Souper IR")
    (description "This package provides a library for manipulating Souper IR")
    (license (list license:expat license:asl2.0))))

;; dependency for rust-wasmtime-winch-11
(define-public rust-wasmtime-cranelift-shared-11
  (package
    (name "rust-wasmtime-cranelift-shared")
    (version "11.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmtime-cranelift-shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mix4938syq3id78z2k1rc3yg1iv128vlvqgf7krlcv55wb9jj23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-cranelift-control" ,rust-cranelift-control-0.98)
                       ("rust-cranelift-native" ,rust-cranelift-native-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Base-level integration with Wasmtime and Cranelift")
    (description "Base-level integration with Wasmtime and Cranelift")
    (license (list license:asl2.0))))

;; dependency for rust-wasmtime-winch-11
(define-public rust-winch-codegen-0.9
  (package
    (name "rust-winch-codegen")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winch-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g8b1yznn3rx94yrm5a9hynyy5qpkbhyb61y7xyc4zqm8hsab8g1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cranelift-codegen" ,rust-cranelift-codegen-0.98)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-regalloc2" ,rust-regalloc2-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-wasmparser" ,rust-wasmparser-0.107)
                       ("rust-wasmtime-environ" ,rust-wasmtime-environ-11))))
    (home-page "https://github.com/bytecodealliance/wasmtime")
    (synopsis "Winch code generation library")
    (description "Winch code generation library")
    (license (list license:asl2.0))))

;; dependency for rust-winch-codegen-0.9 and 1 other
(define-public rust-regalloc2-0.9
  (package
    (name "rust-regalloc2")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regalloc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19i94jyjma82hgyf5wj83zkqc5wnfxnh38k3lcj7m6w7ki9ns5dd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-libfuzzer-sys" ,rust-libfuzzer-sys-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-slice-group-by" ,rust-slice-group-by-0.3)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/bytecodealliance/regalloc2")
    (synopsis "Backtracking register allocator inspired from IonMonkey")
    (description
     "Backtracking register allocator inspired from @code{IonMonkey}")
    (license (list license:asl2.0))))

;; dependency for rust-regalloc2-0.9
(define-public rust-slice-group-by-0.3
  (package
    (name "rust-slice-group-by")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slice-group-by" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19vbyyxqvc25fv2dmhlxijlk5sa9j34yb6hyydb9vf89kh36fqc2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rand" ,rust-rand-0.6))))
    (home-page "https://github.com/Kerollmops/slice-group-by")
    (synopsis "Iterators over groups in slices and strs")
    (description "Iterators over groups in slices and strs")
    (license license:expat)))

;; dependency for rust-regalloc2-0.9
(define-public rust-libfuzzer-sys-0.4
  (package
    (name "rust-libfuzzer-sys")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libfuzzer-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xz70z8q85f80wdrc0m0flck73cqdhj5ajgd7ywg50pbaxazsv59"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-fuzz/libfuzzer")
    (synopsis "A wrapper around LLVM's libFuzzer runtime.")
    (description
     "This package provides a wrapper around LLVM's @code{libFuzzer} runtime.")
    (license (list license:expat license:asl2.0 license:ncsa))))

;; dependency for rust-wasmtime-11
(define-public rust-wat-1
  (package
    (name "rust-wat")
    (version "1.0.85")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mnjz9gs1d8pf3c64mg7ykxxlca1vnqxqhwwa2i4cvf4dixkbmxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wast" ,rust-wast-70))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wat")
    (synopsis "Rust parser for the WebAssembly Text format, WAT
")
    (description "Rust parser for the @code{WebAssembly} Text format, WAT")
    (license (list license:asl2.0))))

;; dependency for rust-wat-1
(define-public rust-wast-70
  (package
    (name "rust-wast")
    (version "70.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wast" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p0nl94529bjhiy978ilgmb3zh00gpif3ni3a43gabq4009hdmd3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-leb128" ,rust-leb128-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-wasm-encoder" ,rust-wasm-encoder-0.41))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-rayon" ,rust-rayon-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wast")
    (synopsis
     "Customizable Rust parsers for the WebAssembly Text formats WAT and WAST
")
    (description
     "Customizable Rust parsers for the @code{WebAssembly} Text formats WAT and WAST")
    (license (list license:asl2.0))))

;; dependency for rust-wast-70
(define-public rust-leb128-0.2
  (package
    (name "rust-leb128")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "leb128" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rxxjdn76sjbrb08s4bi7m4x47zg68f71jzgx8ww7j0cnivjckl8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/gimli-rs/leb128")
    (synopsis
     "Read and write DWARF's \"Little Endian Base 128\" (LEB128) variable length integer encoding.")
    (description
     "Read and write DWARF's \"Little Endian Base 128\" (LEB128) variable length integer
encoding.")
    (license (list license:asl2.0 license:expat))))

;; dependency for rust-wast-70
(define-public rust-wasm-encoder-0.41
  (package
    (name "rust-wasm-encoder")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-encoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd06181w3ip6jp5ag3lz37apgsn3yqymnsyzckpsqw8cdywm6z0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-leb128" ,rust-leb128-0.2)
                       ("rust-wasmparser" ,rust-wasmparser-0.121))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-encoder")
    (synopsis "A low-level WebAssembly encoder.
")
    (description
     "This package provides a low-level @code{WebAssembly} encoder.")
    (license (list license:asl2.0))))

;;; Hearth dependency: plugins/terminal


