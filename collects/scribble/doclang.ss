
(module doclang (lib "lang.ss" "big")
  (require "struct.ss"
           "decode.ss"
           (lib "kw.ss"))
  (require-for-syntax (lib "kerncase.ss" "syntax"))

  (provide (all-from-except (lib "lang.ss" "big") #%module-begin)
           (rename *module-begin #%module-begin))

  ;; Module wrapper ----------------------------------------

  (define-syntax (*module-begin stx)
    (syntax-case stx ()
      [(_ id exprs . body)
       #'(#%plain-module-begin
          (doc-begin id exprs . body))]))

  (define-syntax (doc-begin stx)
    (syntax-case stx ()
      [(_ m-id (expr ...))
       #`(begin
           (define m-id (decode (list . #,(reverse (syntax->list #'(expr ...))))))
           (provide m-id))]
      [(_ m-id exprs . body)
       ;; `body' probably starts with lots of string constants;
       ;; it's slow to trampoline on every string, so do them
       ;; in a batch here:
       (let loop ([body #'body]
                  [accum null])
         (syntax-case body ()
           [(s . rest)
            (string? (syntax-e #'s))
            (loop #'rest (cons #'s accum))]
           [()
            (with-syntax ([(accum ...) accum])
              #`(doc-begin m-id (accum ... . exprs)))]
           [(body1 . body)
            (with-syntax ([exprs (append accum #'exprs)])
              (let ([expanded (local-expand #'body1
                                            'module
                                            (append
                                             (kernel-form-identifier-list #'here)
                                             (syntax->list #'(provide
                                                              require
                                                              require-for-syntax
                                                              require-for-label))))])
                (syntax-case expanded  (begin)
                  [(begin body1 ...)
                   #`(doc-begin m-id exprs body1 ... . body)]
                  [(id . rest)
                   (and (identifier? #'id)
                        (ormap (lambda (kw) (module-identifier=? #'id kw))
                               (syntax->list #'(require 
                                                provide 
                                                require-for-syntax
                                                require-for-label
                                                define-values
                                                define-syntaxes
                                                define-for-syntaxes))))
                   #`(begin #,expanded (doc-begin m-id exprs . body))]
                  [_else
                   #`(doc-begin m-id (#,expanded . exprs) . body)])))]))])))
