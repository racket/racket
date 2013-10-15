#lang racket/base

(require racket/list
         racket/match
         racket/contract
         compiler/zo-parse
         "util.rkt")

(define (->module-path-index s)
  (if (module-path-index? s)
      s
      (module-path-index-join `(quote ,s) #f)))

(define (wrap-in-kernel-module name srcname lang-info self-modidx top)
  (match top
    [(struct compilation-top (max-let-depth prefix form))
     (define-values (reqs new-forms)
       (partition req? (splice-forms form)))
     (define requires
       (map (compose ->module-path-index wrapped-datum stx-encoded req-reqs) reqs))
     (make-compilation-top 
      0
      (make-prefix 0 (list #f) empty)
      (make-mod name srcname
                self-modidx
                prefix
                empty ; provides
                (list (cons 0 requires))
                new-forms
                empty ; syntax-body
                (list) ; unexported
                max-let-depth
                (make-toplevel 0 0 #f #f) ; dummy
                lang-info
                #t
                empty
                empty
                empty))]))

(provide/contract
 [wrap-in-kernel-module (symbol? symbol? lang-info/c module-path-index? compilation-top? . -> . compilation-top?)])
