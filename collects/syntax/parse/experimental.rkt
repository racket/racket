#lang scheme/base
(require (for-syntax scheme/base
                     syntax/parse
                     syntax/private/stxparse/rep-data))
(provide define-primitive-splicing-syntax-class)

(define-syntax (define-primitive-splicing-syntax-class stx)

  (define-syntax-class attr
    (pattern name:id
             #:with depth #'0)
    (pattern [name:id depth:nat]))

  (syntax-parse stx
    [(dssp (name:id param:id ...)
       (~or (~once (~seq #:attrs (a:attr ...))
                   #:name "attributes declaration")
            (~once (~seq #:description description)
                   #:name "description declaration")) ...
       proc:expr)
     #'(begin
         (define (get-description param ...)
           description)
         (define parser
           (lambda (stx param ...)
             (let/ec escape
               ((mk-check-result 'name '(a.name ...) stx)
                (proc stx
                      (lambda ([msg #f])
                        (escape
                         (if msg
                             `#s(expect:message ,msg)
                             `#s(expect:thing
                                 ,(get-description param ...) #f #f)))))))))
         (define-syntax name
           (make-stxclass 'name '(param ...)
                          '(#s(attr a.name a.depth #f) ...)
                          (quote-syntax parser)
                          (quote-syntax get-description)
                          #t
                          #t)))]))


(define (mk-check-result name attr-names stx)
  (lambda (result)
    (unless (list? result)
      (error name "parser returned non-list"))
    (let ([rlength (length result)])
      (unless (= rlength (+ 2 (length attr-names)))
        (error name "parser returned list of wrong length; expected length ~s, got ~e"
               (+ 2 (length attr-names))
               result))
      (unless (exact-nonnegative-integer? (cadr result))
        (error name "expected exact nonnegative integer for second element of result list, got ~e"
               (cadr result)))
      (list* (car result)
             (nat->dfc (cadr result) stx)
             (cddr result)))))

(define (nat->dfc nat stx)
  (if (zero? nat)
      `#s(dfc:empty ,stx)
      `#s(dfc:cdr #s(dfc:empty ,stx) ,nat)))


#|

(define-primitive-splicing-syntax-class (name param ...)
  #:attrs (attr-decl ...)
  #:description description-expr
  proc)

'proc' must take two arguments, 'stx' and 'fail', where 'fail' is an
escaping procedure that indicates failure. 'fail' takes an optional
argument, an error message to attach to the failure. If no message is
given, the syntax class description is used.

'proc' must return a list of 2+|attrs| elements. The first element is
the rest of the input syntax. The second element is the number of
elements consumed from the input. The rest are the attribute values,
in the same order as given in the #:attrs directive.

Example:

(define-primitive-splicing-syntax-class (a-expr)
  #:attrs (x)
  #:description "a-expr"
  (lambda (stx fail)
    (syntax-case stx ()
      [(a b c . rest)
       (list #'rest 3 #'(printf "got an A\n"))]
      [_
       (fail)])))

|#
