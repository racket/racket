#lang racket/base
(require ffi/unsafe/global)

(provide scheme-dir
         target-machine
         optimize-level-init)

(define ht (get-place-table))

(define scheme-dir (or (hash-ref ht 'make-boot-scheme-dir #f)
                       (let ([scheme-dir
                              (getenv "SCHEME_SRC")])
                         (and scheme-dir
                              (simplify-path
                               (path->complete-path scheme-dir))))))
(hash-set! ht 'make-boot-scheme-dir scheme-dir)

(define (infer-target-machine)
  ;; Compute a native or pbarch machine string for the current platform.
  ;; Some caveats:
  ;;  1. A pbarch Racket will always infer a pbarch machine,
  ;;     even if a native machine exists. Hopefully this is an
  ;;     unlikely scenario: if you're running Racket CS, you've
  ;;     bootstrapped Chez somehow, so you could use `re.boot`.
  ;;  2. A `tpb` or `pb` Racket on a 32-bit platform would still return
  ;;     64 from `(system-type 'word)`, but, in addition to the above,
  ;;     it is not currently possible or desired to build Racket as
  ;;     `tpb` or `pb` (as opposed to pbarch variants):
  ;;     see <https://github.com/racket/racket/issues/4692>.
  ;;  3. On a hypothetical platform where Chez supported both the
  ;;     architecture and the OS, but not the combination of the two,
  ;;     (e.g. riscv64 Windows) this code would currently return a
  ;;     non-existent native machine (e.g. trv64nt) instead of a
  ;;     working pbarch machine. Presumably this could be fixed if
  ;;     such a platform came into existence.
  (define s (path->string (system-library-subpath #f)))
  (define arch+os
    (cond
      [(regexp-match #rx"^([^\\]+)\\\\([^\\]+)$" s)
       => (λ (m)
            (reverse (cdr m)))]
      [(regexp-match #rx"^([^-]+)-(.+)$" s)
       => cdr]
      [else
       (error 'infer-target-machine "unknown format for system library subpath"
              "produced" (system-library-subpath #f))]))
  (define arch
    (case (car arch+os)
      [("x86_64" "amd64") "a6"] ; https://github.com/racket/racket/issues/4691
      [("i386") "i3"]
      [("aarch64") "arm64"]
      [("arm") "arm32"]
      [("ppc") "ppc32"]
      [("riscv64") "rv64"]
      [("unknown") #f] ; pb machine types
      [else #f]))
  (define os
    (case (cadr arch+os)
      [("macosx" "darwin" "ios") "osx"]
      [("win32" "cygwin") "nt"]
      [("linux" "android") "le"]
      [("gnu-hurd") "gnu"]
      [("freebsd") "fb"]
      [("openbsd") "ob"]
      [("netbsd") "nb"]
      [("solaris") "s2"] ; NOT "sunos4" (I think)
      [("qnx") "qnx"]
      [("unknown") #f] ; pb machine types
      [else #f]))
  (if (and arch os)
      (string-append "t" arch os)
      (format "tpb~a~a"
              (system-type 'word)
              (if (system-big-endian?)
                  "b"
                  "l"))))

(define target-machine (or (hash-ref ht 'make-boot-targate-machine #f)
                           (getenv "MACH")
                           (infer-target-machine)))
(hash-set! ht 'make-boot-targate-machine target-machine)

(define optimize-level-init 3)
