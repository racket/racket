#lang scheme/base

(require "type-rep.ss"
         "effect-rep.ss"
         "tc-utils.ss"
         "rep-utils.ss"
         (only-in "free-variance.ss" combine-frees)
         mzlib/plt-match
         scheme/list
         (for-syntax scheme/base))

(provide fv fv/list
         substitute
         substitute-dots
         subst-all
         subst 
         ret
         instantiate-poly
         tc-result: 
         tc-result-equal? 
         effects-equal?
         tc-result-t
         unfold
         (struct-out Dotted)
         (struct-out DottedBoth))


;; substitute : Type Name Type -> Type
(define (substitute image name target)    
  (define (sb t) (substitute image name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
                 [#:F name* (if (eq? name* name) image target)]
                 [#:arr dom rng rest drest thn-eff els-eff
                        (begin
                          (when (and (pair? drest)
                                     (eq? name (cdr drest)))
                            (int-err "substitute used on ... variable ~a in type ~a" name target))
                          (make-arr (map sb dom)
                                    (sb rng)
                                    (and rest (sb rest))
                                    (and drest (cons (sb (car drest)) (cdr drest)))
                                    (map (lambda (e) (sub-eff sb e)) thn-eff)
                                    (map (lambda (e) (sub-eff sb e)) els-eff)))])
      target))

;; substitute-dots : Listof[Type] Name Type -> Type
(define (substitute-dots images name target)    
  (define (sb t) (substitute-dots images name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
                 [#:F name* (if (eq? name* name)
                                (int-err "substitute-dots: got single variable ~a" name*)
                                target)]
                 [#:arr dom rng rest drest thn-eff els-eff
                        (if (and (pair? drest)
                                 (eq? name (cdr drest)))                            
                            (make-arr (append 
                                       (map sb dom)
                                       (map (lambda (img) (substitute img name (car drest))) images))
                                      (sb rng)
                                      #f
                                      #f
                                      (map (lambda (e) (sub-eff sb e)) thn-eff)
                                      (map (lambda (e) (sub-eff sb e)) els-eff))
                            (make-arr (map sb dom)
                                      (sb rng)
                                      (and rest (sb rest))
                                      (and drest (cons (sb (car drest)) (cdr drest)))
                                      (map (lambda (e) (sub-eff sb e)) thn-eff)
                                      (map (lambda (e) (sub-eff sb e)) els-eff)))])
      target))

;; substitute many variables
;; substitution = Listof[List[Name,Type]]
;; subst-all : substition Type -> Type
(define (subst-all s t)
  (foldr (lambda (e acc) (substitute (cadr e) (car e) acc)) t s))


;; unfold : Type -> Type
;; must be applied to a Mu
(define (unfold t)
  (match t
    [(Mu: name b) (substitute t name b)]
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
       (substitute-dots rest-tys dotted body*))]
    [_ (int-err "instantiate-many: requires Poly type, got ~a" t)]))


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
(define (effects-equal? fs1 fs2) (andmap eq? fs1 fs2))


;; fv : Type -> Listof[Name]
(define (fv t) (hash-map (free-vars* t) (lambda (k v) k)))

;; fv/list : Listof[Type] -> Listof[Name]
(define (fv/list ts) (hash-map (combine-frees (map free-vars* ts)) (lambda (k v) k)))

;; t is (make-F v)
(define-struct Dotted (t))
(define-struct (DottedBoth Dotted) ())