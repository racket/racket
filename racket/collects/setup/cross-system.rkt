#lang racket/base
(require "private/dirs.rkt")

(provide cross-system-type
         cross-system-library-subpath
         cross-installation?)

(define cross-system-table #f)

(define system-type-symbols '(os word gc vm link machine so-suffix so-mode fs-change target-machine))

(define (compute-cross!)
  (unless cross-system-table
    (define lib-dir (find-lib-dir))    
    (define ht (and lib-dir
                    (let ([f (build-path lib-dir "system.rktd")])
                      (and (file-exists? f)
                           (let ([ht (call-with-default-reading-parameterization
                                      (lambda ()
                                        (call-with-input-file*
                                         f
                                         read)))])
                             (and (hash? ht)
                                  ;; If 'vm doesn't match, then assuming that we're looking
                                  ;; at a multi-vm overlay, instead of cross-compiling,
                                  ;; because cross-compiling requires the same VM.
                                  (eq? (system-type 'vm)
                                       (hash-ref ht 'vm #f))
                                  (for/and ([sym (in-list (append
                                                           '(library-subpath
                                                             library-subpath-convention)
                                                           system-type-symbols))])
                                    (not (void? (hash-ref ht sym (void)))))
                                  (not
                                   (and (for/and ([sym (in-list system-type-symbols)]
                                                  #:unless (or (eq? sym 'machine)
                                                               (eq? sym 'gc)))
                                          (define v (hash-ref ht sym))
                                          (or (equal? v (system-type sym))
                                              ;; If 'target-machine is set to #f, that's
                                              ;; for SERVER_COMPILE_MACHINE=any mode
                                              (and (not v)
                                                   (eq? sym 'target-machine)
                                                   (eq? (system-type 'cross) 'infer))))
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
        "(or/c 'os 'word 'gc 'vm 'link 'machine 'target-machine 'so-suffix 'so-mode 'fs-change)"
        mode))
     (compute-cross!)
     (define v (hash-ref cross-system-table mode (void)))
     (if (eq? v (void))
         (system-type mode)
         v)]))

(define (cross-system-library-subpath [mode (begin
                                              (compute-cross!)
                                              (cross-system-type 'gc))])
  (unless (memq mode '(#f 3m cgc cs))
    (raise-argument-error
     'cross-system-library-subtype
     "(or/c #f '3m 'cgc 'cs)"
     mode))
  (compute-cross!)
  (define bstr (hash-ref cross-system-table 'library-subpath #f))
  (cond
   [bstr
    (define conv (hash-ref cross-system-table 'library-subpath-convention))
    (define path (bytes->path bstr conv))
    (case mode
      [(#f cgc) path]
      [(3m) (build-path path (bytes->path #"3m" conv))]
      [(cs) (build-path path (bytes->path #"cs" conv))])]
   [else (system-library-subpath mode)]))

(define (cross-installation?)
  (compute-cross!)
  (or (eq? (system-type 'cross) 'force)
      (positive? (hash-count cross-system-table))))
