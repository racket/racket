#lang racket/unit


(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match unstable/sequence
         (typecheck signatures tc-funapp)
         (types abbrev utils union substitute)
         (rep type-rep)
         (env tvar-env)
         (prefix-in i: (infer infer))

         (for-label
          racket/base
          (only-in '#%kernel [reverse k:reverse])))


(import tc-expr^ tc-app^)
(export tc-app-list^)

(define-literal-set list-literals
  #:for-label
  (reverse k:reverse list list* cons map andmap ormap))

(define-syntax-class boolmap
  #:literal-sets (list-literals)
  #:attributes (default)
  (pattern andmap #:attr default #t)
  (pattern ormap #:attr default #f))

(define-tc/app-syntax-class (tc/app-list expected)
  #:literal-sets (list-literals)
  (pattern (~and form (map f arg0 arg ...))
    (match* ((single-value #'arg0) (stx-map single-value #'(arg ...)))
      ;; if the argument is a ListDots
      [((tc-result1: (ListDots: t0 bound0))
        (list (tc-result1: (or (and (ListDots: t bound) (app (λ _ #f) var))
                               ;; a devious hack - just generate #f so the test below succeeds
                               ;; have to explicitly bind `var' since otherwise `var' appears
                               ;; on only one side of the or
                               ;; NOTE: safe to include these, `map' will error if any list is
                               ;; not the same length as all the others
                               (and (Listof: t var) (app (λ _ #f) bound))))
              ...))
       (=> fail)
       (unless (for/and ([b (in-list bound)]) (or (not b) (eq? bound0 b))) (fail))
       (define expected-elem-type
         (match expected
           [(or #f (tc-any-results: _)) #f]
           [(tc-result1: (ListDots: elem-type (== bound0))) (ret elem-type)]
           [(tc-result1: (Listof: elem-type)) (ret elem-type)]
           [else (fail)]))
       ;; Do not check this in an environment where bound0 is a type variable.
       (define f-type (tc-expr/t #'f))
       ;; Check that the function applies successfully to the element type
       ;; We need the bound to be considered a type var here so that inference works
       (match (extend-tvars (list bound0)
                (tc/funapp #'f #'(arg0 arg ...) f-type (cons (ret t0) (map ret t))
                           expected-elem-type))
         [(tc-result1: t) (ret (make-ListDots t bound0))]
         [(tc-results: ts)
          (tc-error/expr "Expected one value, but got ~a" (-values ts))])]
      ;; otherwise, if it's not a ListDots, defer to the regular function typechecking
      ;; TODO fix double typechecking
      [(res0 res) (tc/app-regular #'form expected)]))
  ;; ormap/andmap of ... argument
  (pattern (~and form (m:boolmap f arg))
    (match-let* ([arg-ty (tc-expr/t #'arg)]
                 [ft (tc-expr/t #'f)])
      (match (match arg-ty
               ;; if the argument is a ListDots
               [(ListDots: t bound)
                ;; just check that the function applies successfully to the element type
                (extend-tvars (list bound)
                  (tc/funapp #'f #'(arg) ft (list (ret t)) expected))]
               ;; otherwise ...
               [_ #f])
        [(tc-result1: t) (ret (Un (-val (attribute m.default)) t))]
        ;; if it's not a ListDots, defer to the regular function typechecking
        ;; TODO fix double typechecking
        [_ (tc/app-regular #'form expected)])))
  ;; special case for `list'
  (pattern (list . args)
    (let ()
      (define vs (stx-map (λ (x) (gensym)) #'args))
      (define l-type (-Tuple (map make-F vs)))
      (define subst
        (match expected
          [(tc-result1: t)
           ;; We want to infer the largest vs that are still under the element types
           (i:infer vs null (list l-type) (list t) (-values (list (-> l-type Univ))))]
          [_ #f]))
      (ret (-Tuple
             (for/list ([i (in-syntax #'args)] [v (in-list vs)])
               (or (and subst
                        (tc-expr/check/t? i (ret (subst-all subst (make-F v)))))
                   (tc-expr/t i)))))))
  ;; special case for `list*'
  (pattern (list* (~between args:expr 1 +inf.0) ...)
    (match-let* ([(list tys ... last) (stx-map tc-expr/t #'(args ...))])
      (ret (foldr -pair last tys))))
  ;; special case for `reverse' to propagate expected type info
  (pattern ((~and fun (~or reverse k:reverse)) arg)
    (match expected
      [(tc-result1: (Listof: _))
       (tc-expr/check #'arg expected)]
      [(tc-result1: (List: ts))
       (tc-expr/check #'arg (ret (-Tuple (reverse ts))))
       (ret (-Tuple ts))]
      [_
       (match (single-value #'arg)
         [(tc-result1: (List: ts))
          (ret (-Tuple (reverse ts)))]
         [arg-ty
          (tc/funapp #'fun #'(arg) (tc-expr/t #'fun) (list arg-ty) expected)])])))
