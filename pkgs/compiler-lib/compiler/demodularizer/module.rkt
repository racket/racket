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
    [(struct compilation-top (max-let-depth binding-namess prefix form))
     (define-values (reqs new-forms)
       (partition req? (splice-forms form)))
     (define requires
       (map (compose ->module-path-index stx-obj-datum stx-content req-reqs) reqs))
     (make-compilation-top 
      0
      #hash()
      (make-prefix 0 (list #f) empty (prefix-src-inspector-desc prefix))
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
                (hash) ; no names visible via `module->namespace`
                empty
                empty
                empty))]))

(provide/contract
 [wrap-in-kernel-module (symbol? symbol? lang-info/c module-path-index? compilation-top? . -> . compilation-top?)])
