#lang racket/base

(require syntax/parse racket/pretty
         "../utils/utils.rkt"
         (private syntax-properties)
         (types type-table)
         (optimizer utils
                    number fixnum float extflonum float-complex
                    vector string list pair sequence
                    box struct dead-code apply unboxed-let
                    hidden-costs))


(provide optimize-top)



(define-syntax-class opt-expr*
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  ;; Can't optimize this code because it isn't typechecked
  (pattern opt:ignore^)

  ;; Same as above, but if the stx is in the ignore table
  (pattern opt:ignore-table^)

  ;; Can't optimize the body of this code because it isn't typechecked
  (pattern (~and _:kw-lambda^
             ((~and op let-values)
              ([(i:id) e-rhs:opt-expr]) e-body:expr ...))
           #:with opt (quasisyntax/loc/origin this-syntax #'op
                        (op ([(i) e-rhs.opt]) e-body ...)))

  ;; interesting cases, where something is optimized
  (pattern :dead-code-opt-expr)
  (pattern :unboxed-let-opt-expr)
  (pattern :apply-opt-expr)
  (pattern :number-opt-expr)
  (pattern :fixnum-opt-expr)
  (pattern :float-opt-expr)
  (pattern :extflonum-opt-expr)
  (pattern :float-complex-opt-expr)
  (pattern :vector-opt-expr)
  (pattern :string-opt-expr)
  (pattern :list-opt-expr)
  (pattern :pair-opt-expr)
  (pattern :sequence-opt-expr)
  (pattern :box-opt-expr)
  (pattern :struct-opt-expr)
  (pattern :hidden-cost-log-expr)

  ;; boring cases, just recur down
  (pattern ((~and op (~or #%plain-lambda define-values)) formals e:opt-expr ...)
           #:with opt (quasisyntax/loc/origin this-syntax #'op (op formals e.opt ...)))
  (pattern ((~and op case-lambda) [formals e:opt-expr ...] ...)
           ;; optimize all the bodies
           #:with opt (syntax/loc/origin this-syntax #'op (op [formals e.opt ...] ...)))
  (pattern ((~and op (~or let-values letrec-values))
            ([ids e-rhs:opt-expr] ...) e-body:opt-expr ...)
           #:with opt (syntax/loc/origin this-syntax #'op
                        (op ([ids e-rhs.opt] ...)
                          e-body.opt ...)))
  (pattern ((~and op letrec-syntaxes+values)
            stx-bindings
            ([(ids ...) e-rhs:opt-expr] ...)
            e-body:opt-expr ...)
           ;; optimize all the rhss and bodies
           #:with opt (quasisyntax/loc/origin this-syntax #'op
                        (letrec-syntaxes+values
                          stx-bindings
                          ([(ids ...) e-rhs.opt] ...)
                          e-body.opt ...)))
  (pattern ((~and kw (~or if begin begin0 set! #%plain-app #%expression
                          #%variable-reference with-continuation-mark))
            e:opt-expr ...)
           #:with opt (quasisyntax/loc/origin this-syntax #'kw
                        (kw e.opt ...)))
  (pattern (~and ((~or #%provide #%require begin-for-syntax define-syntaxes module module*)
                  . _)
                 opt))
  (pattern (~and (~or (quote _) (quote-syntax _) (#%top . _) :id) opt)))

(define (optimize-top stx)
  (parameterize ([optimize (syntax-parser [e:opt-expr* #'e.opt])])
    (let ((result ((optimize) stx)))
      (when *show-optimized-code*
        (pretty-print (syntax->datum result)))
      result)))
