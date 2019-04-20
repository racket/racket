#lang racket/base
(require ffi/unsafe/global)

(provide scheme-dir
         target-machine
         optimize-level-init)

(define ht (get-place-table))

(define scheme-dir (or (hash-ref ht 'make-boot-scheme-dir #f)
                       (simplify-path
                        (path->complete-path
                         (or (getenv "SCHEME_SRC")
                             (error "set `SCHEME_SRC` environment variable"))))))
(hash-set! ht 'make-boot-scheme-dir scheme-dir)

(define target-machine (or (hash-ref ht 'make-boot-targate-machine #f)
                           (getenv "MACH")
                           (case (system-type)
                             [(macosx) (if (eqv? 64 (system-type 'word))
                                           "ta6osx"
                                           "ti3osx")]
                             [(windows) (if (eqv? 64 (system-type 'word))
                                           "ta6nt"
                                           "ti3nt")]
                             [else
                              (error "set `MACH` environment variable")])))

(hash-set! ht 'make-boot-targate-machine target-machine)

(define optimize-level-init 3)
