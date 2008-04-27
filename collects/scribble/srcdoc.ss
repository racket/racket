#lang scheme/base

(require scheme/contract
         (for-syntax scheme/base)
         "provide-doc-transform.ss")

(provide require/doc
         provide/doc
         parameter-doc
         proc-doc
         proc-doc/names)

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
       (with-syntax ([((arg ...) result)
                      (syntax-case #'contract (->d -> values)
                        [(->d (req ...) () (values [name res] ...))
                         #'((req ...) (values res ...))]
                        [(->d (req ...) () [name res])
                         #'((req ...) res)]
                        [(-> result)
                         #'(() result)]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (no argument names)"
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc (id arg ...) result . desc)
          #'(scribble/manual)))])))

(define-provide/doc-transformer proc-doc/names
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract names desc)
       (with-syntax ([header                      
                      (syntax-case #'(contract names) (->d -> values)
                        [((-> ctcs ... result) (arg-names ...))
                         (begin
                           (unless (= (length (syntax->list #'(ctcs ...)))
                                      (length (syntax->list #'(arg-names ...))))
                             (raise-syntax-error #f "mismatched argument list and domain contract count" stx))
                           #'([(id (arg-names ctcs) ...) result]))]

                        [((->* (mandatory ...) (optional ...) result) 
                          ((mandatory-names ...)
                           ((optional-names optional-default) ...)))
                         (begin
                           (unless (= (length (syntax->list #'(mandatory-names ...)))
                                      (length (syntax->list #'(mandatory ...))))
                             (raise-syntax-error #f "mismatched mandatory argument list and domain contract count" stx))
                           (unless (= (length (syntax->list #'(optional-names ...)))
                                      (length (syntax->list #'(optional ...))))
                             (raise-syntax-error #f "mismatched mandatory argument list and domain contract count" stx))
                           #'([(id (mandatory-names mandatory) ... (optional-names optional optional-default) ...)
                               result]))]
                        [((case-> (-> doms ... rng) ...)
                          ((args ...) ...))
                         (begin
                           (for-each
                            (λ (doms args)
                              (unless (= (length (syntax->list doms))
                                         (length (syntax->list args)))
                                (raise-syntax-error #f "mismatched case argument list and domain contract" stx)))
                            (syntax->list #'((doms ...) ...))
                            (syntax->list #'((args ...) ...)))
                           #'([(id (args doms) ...) rng] ...))]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (no argument names)"
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc* header . desc)
          #'(scribble/manual)))])))

(define-provide/doc-transformer parameter-doc
  (lambda (stx)
    (syntax-case stx (parameter/c)
      [(_ id (parameter/c contract) arg-id desc)
       (values
        #'[id (parameter/c contract)]
        #'(defparam id arg-id contract . desc)
        #'(scribble/manual))])))
