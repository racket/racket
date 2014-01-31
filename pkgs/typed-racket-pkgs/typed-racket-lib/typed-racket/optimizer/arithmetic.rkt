#lang racket/base


(require
  (for-label racket/base)
  (for-syntax racket/base syntax/parse)
  (for-template racket/base racket/flonum racket/unsafe/ops)
  racket/list
  racket/match
  "../utils/utils.rkt"
  (types numeric-tower union subtype abbrev tc-result)
  (typecheck tc-funapp)
  (env lexical-env)
  (for-syntax
    (utils literal-syntax-class)))


(provide
  (struct-out typed-syntax)
  add-r
  sub-r
  div-r
  mult-r
  min-r
  max-r
  expand-multiway)


(struct typed-syntax (stx type) #:transparent)


(begin-for-syntax

  (define-literal-syntax-class #:for-label =>)

  (define-splicing-syntax-class maybe-sym
    #:attributes (list-pat)
    [pattern #:sym #:attr list-pat #'list-no-order]
    [pattern (~seq) #:attr list-pat #'list])


  (define-syntax-class type-expr
    #:attributes (pat)
    [pattern (#:bind id:id type:expr)
      #:with pat #'(and id (typed-syntax _ (? (λ (t) (subtype t type)))))]
    [pattern type:expr
      #:with pat #'(typed-syntax _ (? (λ (t) (subtype t type))))])

  (define-syntax-class unary-clause
    #:attributes (match-clause)
    [pattern (type:type-expr :=>^ body:expr)
      #:with match-clause #'[type.pat (typed-syntax body)]])


  (define-syntax-class (binary-clause flunsafe-id)
    #:attributes (match-clause)
    [pattern (type1:type-expr type2:type-expr sym:maybe-sym :=>^ body:expr)
      #:with match-clause #'[(sym.list-pat type1.pat type2.pat) (typed-syntax-stx body)]]
    [pattern (type1:type-expr type2:type-expr sym:maybe-sym :=>^ #:flunsafe)
      #:with op flunsafe-id
      #:with match-clause #'[(sym.list-pat (and type1.pat e1)
                                           (and type2.pat e2))
                             #`(op #,(fl-coerce e1) #,(fl-coerce e2))]])

  )

(define-syntax (define-real-binop stx)
  (syntax-parse stx
    [(_ name:id #:safe safe-id:id #:flunsafe flunsafe-id:id
        (~var clauses (binary-clause #'flunsafe-id)) ...)
     #'(define (name e1 e2)
         (define stx
           (match (list e1 e2)
             clauses.match-clause ...
             [(list (typed-syntax stx1 _) (typed-syntax stx2 _))
              #`(#,safe-id #,stx1 #,stx2)]))

         (define type
           (match (tc/funapp #'safe-id
                             (list (typed-syntax-stx e1) (typed-syntax-stx e2))
                             (ret (lookup-type/lexical #'safe-id))
                             (list (ret (typed-syntax-type e1)) (ret (typed-syntax-type e2)))
                             #f)
            [(tc-result1: t) t]))
         (typed-syntax stx type))]))



(define-syntax (define-real-unop stx)
  (syntax-parse stx
    [(_ name:id #:safe safe-id:id
        clauses:unary-clause ...)
     #'(define (name e)
         (define stx
             (match e
               clauses.match-clause ...
               [(typed-syntax stx _) #`(#,#'safe-id #,stx)]))
         (define type
           (match (tc/funapp #'safe-id
                             (list (typed-syntax-stx e))
                             (ret (lookup-type/lexical #'safe-id))
                             (list (ret (typed-syntax-type e)))
                             #f)
            [(tc-result1: t) t]))

         (typed-syntax stx type))]))

(define (expand-multiway op args)
  (for/fold ([acc (first args)])
            ([arg (in-list (rest args))])
    (op acc arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zero #'0)
(define -NonExactZeroReal (Un -PosReal -NegReal -InexactRealZero))

(define (fl-coerce e)
  (define stx (typed-syntax-stx e))
  (match (typed-syntax-type e)
    [(? (λ (x) (subtype x -Flonum))) stx]
    [(? (λ (x) (subtype x -Fixnum))) #`(unsafe-fx->fl #,stx)]
    [(? (λ (x) (subtype x -Integer))) #`(->fl #,stx)]
    [else #`(real->double-flonum #,stx)]))

(define-real-unop negate-r
  #:safe -
  [-Zero => zero]
  [(#:bind x -Flonum) => #'(unsafe-fl* -1.0 x)])

(define-real-binop add-r
  #:safe + #:flunsafe unsafe-fl+
  [-Zero (#:bind x -Real) #:sym => x]
  [-Flonum -NonExactZeroReal #:sym => #:flunsafe])

(define-real-binop sub-r
  #:safe - #:flunsafe unsafe-fl-
  [-Zero (#:bind x -Real) => (negate-r x)]
  [(#:bind x -Real) -Zero => x]
  [-Flonum -NonExactZeroReal #:sym => #:flunsafe])

(define-real-binop mult-r
  #:safe * #:flunsafe unsafe-fl*
  [-Zero -Real #:sym => zero]
  [-Flonum -NonExactZeroReal #:sym => #:flunsafe])

(define-real-binop div-r
  #:safe / #:flunsafe unsafe-fl/
  [-Zero -NonExactZeroReal => zero]
  [-Flonum -NonExactZeroReal #:sym => #:flunsafe])

(define-real-binop min-r
  #:safe min #:flunsafe unsafe-flmin
  [-Flonum -Flonum => #:flunsafe])

(define-real-binop max-r
  #:safe max #:flunsafe unsafe-flmax
  [-Flonum -Flonum => #:flunsafe])

