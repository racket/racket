#lang scheme/unit

(require (rename-in "../utils/utils.ss" [infer r:infer]))
(require "signatures.ss"
         (rep type-rep filter-rep object-rep)
         (rename-in (types convenience subtype union utils comparison remove-intersect)
                    [remove *remove])
         (env lexical-env)
         (r:infer infer)
	 (utils tc-utils mutated-vars)
         syntax/kerncase
         mzlib/trace
         mzlib/plt-match)

;; if typechecking   
(import tc-expr^)
(export tc-if^)



;; combinators for typechecking in the context of effects
;; t/f tells us whether this is the true or the false branch of an if
;;      neccessary for handling true/false effects
;; Boolean Expr listof[Effect] option[type] -> TC-Result
(define (tc-expr/eff t/f expr effs expected)
  ;; this flag represents whether the refinement proves that this expression cannot be executed
  (let ([flag (box #f)])
    ;; this does the operation on the old type
    ;; type-op : (Type Type -> Type) Type -> _ Type -> Type
    (define ((type-op f t) _ old)
      (let ([new-t (f old t)])
        ;; if this operation produces an uninhabitable type, then this expression can't be executed
        (when (type-equal? new-t (Un))
          (set-box! flag #t))
        ;; have to return something here, so that we can continue typechecking
        new-t))
    ;; loop : listof[effect] -> tc-result
    (let loop ([effs effs])
      ;; convenience macro for checking the rest of the list
      (define-syntax check-rest
        (syntax-rules ()
          [(check-rest f v)
           (with-update-type/lexical f v (loop (cdr effs)))]
          [(check-rest f t v) 
           (check-rest (type-op f t) v)]))
      (if (null? effs)
          ;; base case
          (let* ([reachable? (not (unbox flag))])
            (unless reachable?
              (warn-unreachable expr))
            (cond
              ;; if flag is true, then we don't want to verify that this branch has the appropriate type
              ;; in particular, it might be (void)
              [(and expected reachable?)
               (tc-expr/check expr expected)]
              ;; this code is reachable, but we have no expected type
              [reachable?
               (tc-expr expr)]
              ;; otherwise, this code is unreachable
              ;; and the resulting type should be the empty type
              [(check-unreachable-code?)
               (tc-expr/check expr Univ)
               (ret (Un))]
              [else
               (ret (Un))]))
          ;; recursive case
          (match (car effs)
            ;; these effects have no consequence for the typechecking
            [(True-Effect:)
             (or t/f (set-box! flag #t)) 
             (loop (cdr effs))]
            [(False-Effect:) 
             (and t/f (set-box! flag #t))
             (loop (cdr effs))]
            ;; restrict v to have a type that's a subtype of t
            [(Restrict-Effect: t v) 
             (check-rest restrict t v)]
            ;; remove t from the type of v
            [(Remove-Effect: t v) (check-rest *remove t v)]
            ;; just replace the type of v with (-val #f)
            [(Var-False-Effect: v) (check-rest (lambda (_ old) (-val #f)) v)]
            ;; v cannot have type (-val #f)
            [(Var-True-Effect: v) 
             (check-rest *remove (-val #f) v)])))))

;; the main function
(define (tc/if-twoarm tst thn els)
  ;; check in the context of the effects, and return
  (match-let* ([(tc-result: tst-ty tst-thn-eff tst-els-eff) (tc-expr tst)]
               [(tc-result: thn-ty thn-thn-eff thn-els-eff) (tc-expr/eff #t thn tst-thn-eff #f)]
               [(tc-result: els-ty els-thn-eff els-els-eff) (tc-expr/eff #f els tst-els-eff #f)])
    (match* (els-ty thn-thn-eff thn-els-eff els-thn-eff els-els-eff)
      ;; this is the case for `or'
      ;; the then branch has to be #t
      ;; the else branch has to be a simple predicate
      ;; FIXME - can something simpler be done by using demorgan's law?
      ;; note that demorgan's law doesn't hold for scheme `and' and `or' because they can produce arbitrary values
      ;; FIXME - mzscheme's or macro doesn't match this!
      [(_ (list (True-Effect:)) (list (True-Effect:)) (list (Restrict-Effect: t v)) (list (Remove-Effect: t v*)))
       (=> unmatch)
       (match (list tst-thn-eff tst-els-eff)
         ;; check that the test was also a simple predicate
         [(list (list (Restrict-Effect: s u)) (list (Remove-Effect: s u*)))
          (if (and 
               ;; check that all the predicates are for the for the same identifier
               (free-identifier=? u u*)
               (free-identifier=? v v*)
               (free-identifier=? v u))
              ;; this is just a very simple or
              (ret (Un (-val #t) els-ty)
                   ;; the then and else effects are just the union of the two types
                   (list (make-Restrict-Effect (Un s t) v))
                   (list (make-Remove-Effect (Un s t) v)))
              ;; otherwise, something complicated is happening and we bail
              (unmatch))]
         ;; similarly, bail here
         [_ (unmatch)])]
      ;; this is the case for `and'
      [(_ _ _ (list (False-Effect:)) (list (False-Effect:)))
       (ret (Un (-val #f) thn-ty) 
            ;; we change variable effects to type effects in the test, 
            ;; because only the boolean result of the test is used
            ;; whereas, the actual value of the then branch is returned, not just the boolean result
            (append (map var->type-eff tst-thn-eff) thn-thn-eff)
            ;; no else effects for and, because any branch could have been false
            (list))]
      ;; if the else branch can never happen, just use the effect of the then branch
      [((Union: (list)) _ _ _ _)
       (ret thn-ty thn-thn-eff thn-els-eff)]
      ;; otherwise this expression has no effects
      [(_ _ _ _ _) 
       (ret (Un thn-ty els-ty))])))

;; checking version
(define (tc/if-twoarm/check tst thn els expected)
  ;; check in the context of the effects, and return
  (match-let* ([(tc-result: tst-ty tst-thn-eff tst-els-eff) (tc-expr tst)]
               [(tc-result: thn-ty thn-thn-eff thn-els-eff) (tc-expr/eff #t thn tst-thn-eff expected)]
               [(tc-result: els-ty els-thn-eff els-els-eff) (tc-expr/eff #f els tst-els-eff expected)])
    (match* (els-ty thn-thn-eff thn-els-eff els-thn-eff els-els-eff) 
      ;; this is the case for `or'
      ;; the then branch has to be #t
      ;; the else branch has to be a simple predicate
      ;; FIXME - can something simpler be done by using demorgan's law?
      ;; note that demorgan's law doesn't hold for scheme `and' and `or' because they can produce arbitrary values
      ;; FIXME - mzscheme's or macro doesn't match this!
      [(_ (list (True-Effect:)) (list (True-Effect:)) (list (Restrict-Effect: t v)) (list (Remove-Effect: t v*)))
       (=> unmatch)
       (match (list tst-thn-eff tst-els-eff)
         ;; check that the test was also a simple predicate
         [(list (list (Restrict-Effect: s u)) (list (Remove-Effect: s u*)))
          (if (and 
               ;; check that all the predicates are for the for the same identifier
               (free-identifier=? u u*)
               (free-identifier=? v v*)
               (free-identifier=? v u))
              ;; this is just a very simple or
              (let ([t (Un (-val #t) els-ty)])
                (check-below t expected)
                (ret t
                     ;; the then and else effects are just the union of the two types
                     (list (make-Restrict-Effect (Un s t) v))
                     (list (make-Remove-Effect (Un s t) v))))
              ;; otherwise, something complicated is happening and we bail
              (unmatch))]
         ;; similarly, bail here
         [_ (unmatch)])]
      ;; this is the case for `and'
      [(_ _ _ (list (False-Effect:)) (list (False-Effect:)))
       (let ([t (Un thn-ty (-val #f))])
         (check-below t expected)              
         (ret t 
              ;; we change variable effects to type effects in the test, 
              ;; because only the boolean result of the test is used
              ;; whereas, the actual value of the then branch is returned, not just the boolean result
              (append (map var->type-eff tst-thn-eff) thn-thn-eff)
              ;; no else effects for and, because any branch could have been false
              (list)))]
      ;; if the else branch can never happen, just use the effect of the then branch
      [((Union: (list)) _ _ _ _)
       (ret thn-ty 
            ;; we change variable effects to type effects in the test, 
            ;; because only the boolean result of the test is used
            ;; whereas, the actual value of the then branch is returned, not just the boolean result
            thn-thn-eff
            ;; no else effects for and, because any branch could have been false
            thn-els-eff)]
      ;; otherwise this expression has no effects
      [(_ _ _ _ _)
       (let ([t (Un thn-ty els-ty)])
         (check-below t expected)
         (ret t))])))
