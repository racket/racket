#lang scheme/base

(require (for-syntax scheme/base scheme/provide-transform))

;; This is a utility for many srfi/N.rkt files that simply reprovide stuff from
;; some other file.  It is used as a module, for example, the "srfi/1.rkt"
;; loader has:
;;   #lang s-exp srfi/provider srfi/1/list #:unprefix s:
;; which makes it require `srfi/1/list', then reprovide everything from there,
;; removing any `s:' prefixes that it uses (since `srfi/1/list' does not
;; collide with `scheme/base').  It is used in most files here, and the
;; unprefix facility is used in a few similar situations.  You can add a
;; `#:debug' flag to have the unprefixer print its renamings, to check that you
;; get the right bindings.

(provide (rename-out [module-begin #%module-begin]))
(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ srfi-req . more)
     (let ([pfx #f] [debug #f])
       (let loop ([more #'more])
         (syntax-case more ()
           [(#:unprefix pfx-id . more) (set! pfx #'pfx-id) (loop #'more)]
           [(#:debug . more) (set! debug #t) (loop #'more)]
           [() (void)]))
       #`(#%module-begin
          (require srfi-req)
          (provide (all-from-unprefix-out #,pfx srfi-req #,debug))))]))

(define-syntax all-from-unprefix-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ pfx spec debug?)
        (map (if (identifier? #'pfx)
               (let ([rx (string-append
                          "^"
                          (regexp-quote (symbol->string (syntax-e #'pfx))))]
                     [debug? (syntax-e #'debug?)])
                 (lambda (e)
                   (let* ([s (symbol->string (export-out-sym e))]
                          [m (regexp-match-positions rx s)])
                     (when (and m debug?)
                       (printf "Renaming: ~a -> ~a\n" s (substring s (cdar m))))
                     (if m
                       (make-export (export-local-id e)
                                    (string->symbol  (substring s (cdar m)))
                                    (export-mode     e)
                                    (export-protect? e)
                                    (export-orig-stx e))
                       e))))
               values)
             (expand-export #'(all-from-out spec) modes))]))))
