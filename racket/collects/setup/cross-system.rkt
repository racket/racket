#lang racket/base
(require "private/dirs.rkt")

(provide cross-system-type
         cross-system-library-subpath
         cross-installation?)

(define cross-system-table #f)

(define system-type-symbols '(os word gc link machine so-suffix so-mode fs-change))

(define (compute-cross!)
  (unless cross-system-table
    (define lib-dir (find-lib-dir))    
    (define ht (and lib-dir
                    (let ([f (build-path lib-dir "system.rktd")])
                      (and (file-exists? f)
                           (let ([ht (call-with-input-file*
                                      f
                                      read)])
                             (and (hash? ht)
                                  (for/and ([sym (in-list (list*
                                                           'library-subpath
                                                           'library-subpath-convention
                                                           system-type-symbols))])
                                    (hash-ref ht sym #f))
                                  (not
                                   (and (for/and ([sym (in-list system-type-symbols)]
                                                  #:unless (or (eq? sym 'machine)
                                                               (eq? sym 'gc)))
                                          (equal? (hash-ref ht sym) (system-type sym)))
                                        (equal? (bytes->path (hash-ref ht 'library-subpath)
                                                             (hash-ref ht 'library-subpath-convention))
                                                (system-library-subpath #f))))
                                  ht))))))
    (if ht
        (set! cross-system-table ht)
        (set! cross-system-table #hasheq()))))

(define cross-system-type
  (case-lambda
    [()
     (compute-cross!)
     (or (hash-ref cross-system-table 'os #f)
         (system-type 'os))]
    [(mode)
     (unless (memq mode system-type-symbols)
       (raise-argument-error
        'cross-system-type
        "(or/c 'os 'word 'gc 'link 'machine 'so-suffix 'so-mode 'fs-change)"
        mode))
     (compute-cross!)
     (or (hash-ref cross-system-table mode #f)
         (system-type mode))]))

(define (cross-system-library-subpath [mode (begin
                                              (compute-cross!)
                                              (cross-system-type 'gc))])
  (unless (memq mode '(#f 3m cgc))
    (raise-argument-error
     'cross-system-library-subtype
     "(or/c #f '3m 'cgc)"
     mode))
  (compute-cross!)
  (define bstr (hash-ref cross-system-table 'library-subpath #f))
  (cond
   [bstr
    (define conv (hash-ref cross-system-table 'library-subpath-convention))
    (define path (bytes->path bstr conv))
    (case mode
      [(#f cgc) path]
      [(3m) (build-path path (bytes->path #"3m" conv))])]
   [else (system-library-subpath mode)]))

(define (cross-installation?)
  (compute-cross!)
  (positive? (hash-count cross-system-table)))
