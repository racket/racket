#lang racket/base
(provide wrap-tool-inputs)
(require racket/contract)
(require (for-syntax racket/base
                     racket/match
                     syntax/modread
                     compiler/cm-accomplice))

(define-syntax (wrap-tool-inputs stx)
  (syntax-case stx ()
    [(_ body tool-name replace?)
     (let ()
       (define do-regexp-replace? (syntax-e #'replace?))
       (define tool-lib-src (collection-file-path "tool-lib.rkt" "drracket"))
       (define full-sexp
         (call-with-input-file tool-lib-src
           (λ (port)
              (with-module-reading-parameterization
               (lambda ()
                 (read port))))))
       
       (register-external-file tool-lib-src)

       (let loop ([sexp full-sexp])
         (match sexp
           [`((#%module-begin ,body ...))
            (loop body)]
           [`((provide/dr/doc ,clauses ...) ,rest ...)
            (let ()
              (define (rewrite obj)
                (if do-regexp-replace?
                    (let loop ([obj obj])
                      (cond
                        [(symbol? obj)
                         (string->symbol (regexp-replace #rx"^drracket:"
                                                         (symbol->string obj)
                                                         "drscheme:"))]
                        [(pair? obj)
                         (cons (loop (car obj)) (loop (cdr obj)))]
                        [else obj]))
                    obj))
              (define let-bindingss
                (for/list ([clause (in-list clauses)])
                  (match clause
                    [`(struct-doc ,struct-name ((,field-name ,field-ctc) ...) ,other ...)
                     '()]
                    [`(,(or 'proc-doc/names 'proc-doc 'thing-doc 'parameter-doc)
                       ,(? symbol? orig-name)
                       ,ctc
                       ,other ...)
                     (with-syntax ([name (datum->syntax #'tool-name (rewrite orig-name))]
                                   [ctc (datum->syntax #'tool-name (rewrite ctc))])
                       (list
                        #`[name
                           (contract
                            (let ([name ctc]) name)  ;; need to replace the names in 'ctc'
                            name 
                            'drracket 
                            tool-name
                            (quote name)
                            (quote-syntax name))]))])))
            #`(let #,(apply append let-bindingss)
                body))]
           [`(,a . ,b) 
            (loop b)]
           [`()
            (error 'tools-drs.rkt "did not find provide/dr/doc: ~a" full-sexp)])))]))
