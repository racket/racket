#lang scheme/base

(require "struct.ss"
         "decode.ss"
         (for-syntax scheme/base
                     syntax/kerncase))

(provide (except-out (all-from-out scheme/base) #%module-begin)
         (rename-out [*module-begin #%module-begin]))

;; Module wrapper ----------------------------------------

(define-syntax (*module-begin stx)
  (syntax-case stx ()
    [(_ id post-process exprs . body)
     #'(#%module-begin
        (doc-begin id post-process exprs . body))]))

(define-syntax (doc-begin stx)
  (syntax-case stx ()
    [(_ m-id post-process (expr ...))
     #`(begin
         (define m-id (post-process (decode (list . #,(reverse (syntax->list #'(expr ...)))))))
         (provide m-id))]
    [(_ m-id post-process exprs . body)
     ;; `body' probably starts with lots of string constants; it's
     ;; slow to trampoline on every string, so do them in a batch
     ;; here:
     (let loop ([body #'body]
                [accum null])
       (syntax-case body ()
         [(s . rest)
          (string? (syntax-e #'s))
          (loop #'rest (cons #'s accum))]
         [()
          (with-syntax ([(accum ...) accum])
            #`(doc-begin m-id post-process (accum ... . exprs)))]
         [(body1 . body)
          (with-syntax ([exprs (append accum #'exprs)])
            (let ([expanded (local-expand
                             #'body1 'module
                             (append (kernel-form-identifier-list)
                                     (syntax->list #'(provide
                                                      require
                                                      #%provide
                                                      #%require))))])
              (syntax-case expanded (begin)
                [(begin body1 ...)
                 #`(doc-begin m-id post-process exprs body1 ... . body)]
                [(id . rest)
                 (and (identifier? #'id)
                      (ormap (lambda (kw) (free-identifier=? #'id kw))
                             (syntax->list #'(require
                                              provide
                                              define-values
                                              define-syntaxes
                                              define-values-for-syntax
                                              #%require
                                              #%provide))))
                 #`(begin #,expanded (doc-begin m-id post-process exprs . body))]
                [_else
                 #`(doc-begin m-id post-process (#,expanded . exprs) . body)])))]))]))
