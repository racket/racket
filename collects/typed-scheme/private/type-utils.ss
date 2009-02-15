#lang scheme/base

(require "../utils/utils.ss")

(require (rep type-rep effect-rep rep-utils)
         (utils tc-utils)
         (only-in (rep free-variance) combine-frees)
         scheme/match
         scheme/list
         mzlib/trace
         (for-syntax scheme/base))

(provide fv fv/list
         substitute
         substitute-dots
         substitute-dotted
         subst-all
         subst 
         ret
         instantiate-poly
         instantiate-poly-dotted
         tc-result:
         tc-result?
         tc-result-equal? 
         effects-equal?
         tc-result-t
         unfold
         (struct-out Dotted)
         (struct-out DottedBoth)
         just-Dotted?
         tc-error/expr
         lookup-fail)


;; substitute : Type Name Type -> Type
(define (substitute image name target #:Un [Un (get-union-maker)])
  (define (sb t) (substitute image name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
		 [#:Union tys (Un (map sb tys))]
                 [#:F name* (if (eq? name* name) image target)]
                 [#:arr dom rng rest drest kws thn-eff els-eff
                        (begin
                          (when (and (pair? drest)
                                     (eq? name (cdr drest))
                                     (just-Dotted? name))
                            (int-err "substitute used on ... variable ~a in type ~a" name target))
                          (make-arr (map sb dom)
                                    (sb rng)
                                    (and rest (sb rest))
                                    (and drest (cons (sb (car drest)) (cdr drest)))
                                    (for/list ([kw kws])
                                      (cons (car kw) (sb (cdr kw))))
                                    (map (lambda (e) (sub-eff sb e)) thn-eff)
                                    (map (lambda (e) (sub-eff sb e)) els-eff)))]
                 [#:ValuesDots types dty dbound
                               (begin
                                 (when (eq? name dbound)
                                   (int-err "substitute used on ... variable ~a in type ~a" name target))
                                 (make-ValuesDots (map sb types) (sb dty) dbound))])
      target))

;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(define (substitute-dots images rimage name target)    
  (define (sb t) (substitute-dots images rimage name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
                 [#:ValuesDots types dty dbound
                               (if (eq? name dbound)
                                   (make-Values
                                    (append 
                                     (map sb types)
                                     ;; We need to recur first, just to expand out any dotted usages of this.
                                     (let ([expanded (sb dty)])
                                       (map (lambda (img) (substitute img name expanded)) images))))
                                   (make-ValuesDots (map sb types) (sb dty) dbound))]
                 [#:arr dom rng rest drest kws thn-eff els-eff
                        (if (and (pair? drest)
                                 (eq? name (cdr drest)))                            
                            (make-arr (append 
                                       (map sb dom)
                                       ;; We need to recur first, just to expand out any dotted usages of this.
                                       (let ([expanded (sb (car drest))])
					 (map (lambda (img) (substitute img name expanded)) images)))
                                      (sb rng)
                                      rimage
                                      #f
                                      (for/list ([kw kws])
                                        (cons (car kw) (sb (cdr kw))))
                                      (map (lambda (e) (sub-eff sb e)) thn-eff)
                                      (map (lambda (e) (sub-eff sb e)) els-eff))
                            (make-arr (map sb dom)
                                      (sb rng)
                                      (and rest (sb rest))
                                      (and drest (cons (sb (car drest)) (cdr drest)))
                                      (for/list ([kw kws])
                                        (cons (car kw) (sb (cdr kw))))
                                      (map (lambda (e) (sub-eff sb e)) thn-eff)
                                      (map (lambda (e) (sub-eff sb e)) els-eff)))])
      target))

;; implements sd from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(define (substitute-dotted image image-bound name target)
  (define (sb t) (substitute-dotted image image-bound name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
                 [#:ValuesDots types dty dbound
                               (make-ValuesDots (map sb types)
                                                (sb dty)
                                                (if (eq? name dbound) image-bound dbound))]
                 [#:F name*
                      (if (eq? name* name)
                          image
                          target)]
                 [#:arr dom rng rest drest kws thn-eff els-eff
                        (make-arr (map sb dom)
                                  (sb rng)
                                  (and rest (sb rest))
                                  (and drest
                                       (cons (sb (car drest))
                                             (if (eq? name (cdr drest)) image-bound (cdr drest))))
                                  (for/list ([kw kws])
                                    (cons (car kw) (sb (cdr kw))))
                                  (map (lambda (e) (sub-eff sb e)) thn-eff)
                                  (map (lambda (e) (sub-eff sb e)) els-eff))])
       target))

;; substitute many variables
;; substitution = Listof[U List[Name,Type] List[Name,Listof[Type]]]
;; subst-all : substition Type -> Type
(define (subst-all s t)
  (for/fold ([t t]) ([e s])
    (match e
      [(list v (list imgs ...) starred)
       (substitute-dots imgs starred v t)]
      [(list v img)
       (substitute img v t)])))


;; unfold : Type -> Type
;; must be applied to a Mu
(define (unfold t)
  (match t
    [(Mu: name b) (substitute t name b #:Un (lambda (tys) (make-Union (sort tys < #:key Type-seq))))]
    [_ (int-err "unfold: requires Mu type, got ~a" t)]))

(define (instantiate-poly t types)
  (match t
    [(Poly: ns body) 
     (unless (= (length types) (length ns))
       (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a" (length ns) (length types)))
     (subst-all (map list ns types) body)]
    [(PolyDots: (list fixed ... dotted) body)
     (unless (>= (length types) (length fixed))
       (int-err "instantiate-poly: wrong number of types: expected at least ~a, got ~a" (length fixed) (length types)))
     (let* ([fixed-tys (take types (length fixed))]
            [rest-tys (drop types (length fixed))]
            [body* (subst-all (map list fixed fixed-tys) body)])
       (substitute-dots rest-tys #f dotted body*))]
    [_ (int-err "instantiate-poly: requires Poly type, got ~a" t)]))

(define (instantiate-poly-dotted t types image var)
  (match t
    [(PolyDots: (list fixed ... dotted) body)
     (unless (= (length fixed) (length types))
       (int-err "instantiate-poly-dotted: wrong number of types: expected ~a, got ~a, types were ~a" 
                (length fixed) (length types) types))
     (let ([body* (subst-all (map list fixed types) body)])
       (substitute-dotted image var dotted body*))]
    [_ (int-err "instantiate-poly-dotted: requires PolyDots type, got ~a" t)]))


;; this structure represents the result of typechecking an expression
(define-struct tc-result (t thn els) #:inspector #f)

(define-match-expander tc-result:
  (lambda (stx)
    (syntax-case stx ()
      [(form pt) #'(struct tc-result (pt _ _))]
      [(form pt pe1 pe2) #'(struct tc-result (pt pe1 pe2))])))

;; convenience function for returning the result of typechecking an expression
(define ret
  (case-lambda [(t) (make-tc-result t (list) (list))]
               [(t thn els) (make-tc-result t thn els)]))

(define (subst v t e) (substitute t v e))


;; type comparison

;; equality - good

(define tc-result-equal? equal?)
(define (effects-equal? fs1 fs2) 
  (and 
   (= (length fs1) (length fs2))
   (andmap eq? fs1 fs2)))


;; fv : Type -> Listof[Name]
(define (fv t) (hash-map (free-vars* t) (lambda (k v) k)))

;; fv/list : Listof[Type] -> Listof[Name]
(define (fv/list ts) (hash-map (combine-frees (map free-vars* ts)) (lambda (k v) k)))

;; t is (make-F v)
(define-struct Dotted (t))
(define-struct (DottedBoth Dotted) ())

(define (just-Dotted? S)
    (and (Dotted? S)
         (not (DottedBoth? S))))

(define (tc-error/expr msg #:return [return (make-Union null)] #:stx [stx (current-orig-stx)] . rest)
  (tc-error/delayed #:stx stx (apply format msg rest))
  return)

;; error for unbound variables
(define (lookup-fail e) (tc-error/expr "unbound identifier ~a" e))
