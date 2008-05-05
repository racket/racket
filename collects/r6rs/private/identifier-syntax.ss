#lang scheme/base
(require (for-syntax scheme/base)
         (for-template (only-in scheme/base set! #%app)))

(provide identifier-syntax)

(define-syntax (identifier-syntax stx)
  (syntax-case* stx (set!) (lambda (a b)
                             (free-template-identifier=? a b))
    [(identifier-syntax template)
     #'(...
        (make-set!-transformer
         (lambda (stx)
           (syntax-case stx (set!)
             [(set! . _) (raise-syntax-error
                          #f
                          "cannot assign to identifier macro"
                          stx)]
             [(_ arg ...) #'(template arg ...)]
             [_ #'template]))))]
    [(identifier-syntax
      [id1 template1]
      [(set! id2 pat) template2])
     (and (identifier? #'id1)
          (identifier? #'id2))
     #'(...
        (make-set!-transformer
         (lambda (stx)
           (syntax-case stx (set!)
             [(set! id2 pat) #'template2]
             [(_ arg ...) #'(template1 arg ...)]
             [_ #'template1]))))]))
