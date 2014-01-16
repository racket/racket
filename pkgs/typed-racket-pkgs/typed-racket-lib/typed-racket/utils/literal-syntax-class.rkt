#lang racket/base

(require
  "../utils/utils.rkt"
  syntax/parse
  (utils tc-utils)
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    unstable/sequence))

(provide define-literal-syntax-class)



(define-syntax (define-literal-syntax-class stx)
  (define-splicing-syntax-class spec
    #:attributes (name (literals 1))
    (pattern (~seq name:id (literals:id ...)))
    (pattern literal:id
             #:with (literals ...) #'(literal)
             #:with name (format-id #'literal "~a^" #'literal)))
  (syntax-parse stx
    ((_ phase-spec :spec)
     ;; We need pattern ids that have the all have the same lexical context as the literal-set binding
     (define add-context
       (let ((introducer (make-syntax-introducer)))
         (λ (sym) (introducer (datum->syntax #f sym)))))
     (define/with-syntax literal-set (add-context 'lit-set))
     (define/with-syntax (pattern-literals ...)
        (for/list ([_ (in-syntax #'(literals ...))]
                   [n (in-naturals)])
          (add-context (string->symbol (format "pat~a" n)))))
     #'(begin
         (define-literal-set literal-set phase-spec
           ([pattern-literals literals] ...))

         (define-syntax-class name
           #:attributes ()
           #:commit
           #:literal-sets ([literal-set])
           (pattern (~and op (~or pattern-literals ...))
                    #:do [(add-disappeared-use (syntax-local-introduce #'op))]))))))
