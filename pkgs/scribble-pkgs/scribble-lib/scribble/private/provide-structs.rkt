#lang scheme/base
(require scheme/serialize
         racket/contract/base
         (for-syntax scheme/base))

(provide provide-structs)

(define-syntax (provide-structs stx)
  (syntax-case stx ()
    [(_ (id ([field ct] ...)) ...)
     #`(begin
         (define-serializable-struct id (field ...) #:transparent) ...
         (provide/contract
          #,@(let ([ids (syntax->list #'(id ...))]
                   [fields+cts (syntax->list #'(([field ct] ...) ...))])
               (define (get-fields super-id)
                 (ormap (lambda (id  fields+cts)
                          (if (identifier? id)
                            (and (free-identifier=? id super-id)
                                 fields+cts)
                            (syntax-case id ()
                              [(my-id next-id)
                               (free-identifier=? #'my-id super-id)
                               #`[#,@(get-fields #'next-id)
                                  #,@fields+cts]]
                              [_else #f])))
                        ids fields+cts))
               (map (lambda (id fields+cts)
                      (if (identifier? id)
                        #`[struct #,id #,fields+cts]
                        (syntax-case id ()
                          [(id super)
                           #`[struct id (#,@(get-fields #'super) 
                                         #,@fields+cts)]])))
                    ids
                    fields+cts))))]))

