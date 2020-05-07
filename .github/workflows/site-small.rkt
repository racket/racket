#lang distro-build/config
(require racket/format)

(define (build-dir-name)
  (case (current-mode)
    [("release") "release-build"]
    [else "build"]))
(define (dest-dir-name)
  (case (current-mode)
    [("release") "ci-release"]
    [else (~a "ci-snapshots/" (current-stamp))]))

(define server-base-url (~a "https://snapshots.racket-lang.org/" (dest-dir-name) "/"))

(define distro-content
  '("compiler-lib" "racket-lib"))

(define (prebuilt-racket)
  (or (and (getenv "PLTHOME") (~a (getenv "PLTHOME") "/racket/" "bin/" "racket"))
      "/usr/bin/racket"))

;; The overall configuration:
(sequential
 #:pkgs distro-content
 #:dist-base-url server-base-url
 #:site-dest (build-path (or (getenv "DISTRO_BUILD_SITE_DEST") "/tmp/racket-snapshots/") (dest-dir-name))
 #:plt-web-style? #t
 #:site-title (format "Snapshot: ~a" (current-stamp))
 #:fail-on-client-failures #f
 (machine #:name "Racket BC (Ubuntu 18.04, x86_64)"
          #:versionless? #true
          #:log-file "racket_bc_ubuntu18.04.txt"
          #:j 2)
 (machine #:name "Racket CS (Ubuntu 18.04, x86_64)"
          ;; can't use the pre-built Racket with Racket CS
          #:versionless? #true
          #:log-file "racket_cs_ubuntu18.04.txt"
          #:dir "cs_build"
          #:j 2
          #:repo (or (getenv "HERE") ".")
          #:pull? #f
          #:variant 'cs
          #:dist-suffix "cs"))
