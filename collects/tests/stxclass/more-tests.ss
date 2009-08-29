#lang scheme
(require syntax/parse
         schemeunit)
(require (for-syntax syntax/parse))

(define-syntax (convert-syntax-error stx)
  (syntax-case stx ()
    [(_ expr)
     (with-handlers ([exn:fail:syntax?
                      (lambda (e)
                        #`(error '#,(exn-message e)))])
       (local-expand #'expr 'expression null))]))

;; Test #:auto-nested-attributes

(define-syntax-class two
  (pattern (x y)))

(define-syntax-class square0
  (pattern (x:two y:two)))

(define-syntax-class square
  #:auto-nested-attributes
  (pattern (x:two y:two)))

(test-case "nested attributes omitted by default"
  (check-equal? (syntax-class-attributes square0)
                '((x 0) (y 0))))

(test-case "nested attributes work okay"
  (check-equal? (syntax-class-attributes square)
                '((x 0) (x.x 0) (x.y 0) (y 0) (y.x 0) (y.y 0))))

;; Test static-of

(define-syntax zero 0)
(define-syntax (m stx)
  (syntax-parse stx
    [(_ x)
     #:declare x (static number? "identifier bound to number")
     #`(quote #,(attribute x.value))]))

(test-case "static: right error"
           (check-exn (lambda (exn)
                        (regexp-match? #rx"identifier bound to number"
                                       (exn-message exn)))
                      (lambda () (convert-syntax-error (m twelve)))))

(test-case "static: works"
           (check-equal? (convert-syntax-error (m zero))
                         0))

