#lang scheme/base
(require (for-syntax scheme/base)
         (for-template "no-set.rkt"
                       (only-in scheme/base #%app set!)))

(provide identifier-syntax)

(define-syntax (identifier-syntax stx)
  (syntax-case* stx (r6rs:set!) (lambda (a b)
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
      [(r6rs:set! id2 pat) template2])
     (and (identifier? #'id1)
          (identifier? #'id2))
     #'(...
        (make-set!-transformer
         (lambda (stx)
           (syntax-case stx (set!)
             [(set! id2 pat) #'template2]
             [(_ arg ...) #'(template1 arg ...)]
             [_ #'template1]))))]))
