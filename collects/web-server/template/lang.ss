#lang scheme

(require (for-syntax scheme)
         web-server/template/lib)

(provide (except-out (all-from-out scheme) #%module-begin)
         (rename-out [*module-begin #%module-begin])
         (all-from-out web-server/template/lib))

(define-for-syntax (ids stx)
  (syntax-case stx ()
    [(e ...)
     (apply append (map ids (syntax->list #'(e ...))))]
    [e (and (identifier? #'e)
            (equal? #\$ (string-ref (symbol->string (syntax->datum #'e)) 0)))
       (list #'e)]
    [_ empty]))

(define-for-syntax (uniq ls)
  (hash-map
   (foldl (lambda (e a) (hash-set a (syntax->datum e) e))
          (make-immutable-hash empty) ls)
   (lambda (k v) v)))

(define-syntax (*module-begin stx)
  (syntax-case stx (require)
    [(_ id (require r ...) body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require r ...)
        (define-template id (#%string-append body ...))
        (provide id)))]
    [(_ id body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (define-template id (#%string-append body ...))
        (provide id)))]))

(define-syntax (define-template stx)
  (syntax-case stx ()
    [(_ id body)
     (let ([fv-stxs (uniq (ids #'body))])
       (with-syntax ([(arg ...)
                      (foldl (lambda (i a)
                               (quasisyntax/loc i
                                 (#,(datum->syntax
                                     i
                                     (string->keyword
                                      (substring
                                       (symbol->string
                                        (syntax->datum i))
                                       1))
                                     i)
                                  #,i
                                  #,@a)))
                             #'()
                             fv-stxs)])
         (quasisyntax/loc stx
           (define (id arg ...)
             body))))]))
