#lang scheme/base

(require scheme/contract
         (for-syntax scheme/base)
         "provide-doc-transform.ss")

(provide require/doc
         provide/doc
         proc-doc)

(define-syntax-rule (require/doc spec ...)
  (void (quote-syntax (require/doc spec ...))))

(define-syntax (provide/doc stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ([forms (syntax->list #'(form ...))])
       (with-syntax ([((for-provide/contract for-docs) ...)
                      (map (lambda (form)
                             (syntax-case form ()
                               [(id . _)
                                (identifier? #'id)
                                (let ([t (syntax-local-value #'id (lambda () #f))])
                                  (unless (provide/doc-transformer? t)
                                    (raise-syntax-error
                                     #f
                                     "not bound as a provide/doc transformer"
                                     stx
                                     #'id))
                                  (let* ([i (make-syntax-introducer)]
                                         [i2 (lambda (x) (syntax-local-introduce (i x)))])
                                    (let-values ([(p/c d req/d) ((provide/doc-transformer-proc t)
                                                                 (i (syntax-local-introduce form)))])
                                      (list (i2 p/c) (list (i2 req/d) (i2 d) (i2 (quote-syntax tag)))))))]
                               [_
                                (raise-syntax-error
                                 #f
                                 "not a provide/doc sub-form"
                                 stx
                                 form)]))
                           forms)])
         (with-syntax ([(p/c ...)
                        (map (lambda (form f)
                               (quasisyntax/loc form
                                 (provide/contract #,f)))
                             forms
                             (syntax->list #'(for-provide/contract ...)))])
           #'(begin
               p/c ...
               (void (quote-syntax (provide/doc for-docs ...)))))))]))

(define-provide/doc-transformer proc-doc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract desc)
       (with-syntax ([(arg ...)
                      (syntax-case #'contract (->d)
                        [(->d (req ...) () result)
                         #'(req ...)]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (arguments)"
                          stx
                          #'contract)])]
                     [result
                      (syntax-case #'contract (->d)
                        [(->d reqs opts (values [name res] ...))
                         #'(values res ...)]
                        [(->d reqs opts [name res])
                         #'res]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (arguments)"
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc (id arg ...) result . desc)
          #'(scribble/manual)))])))

        
