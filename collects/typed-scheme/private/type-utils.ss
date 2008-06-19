#lang scheme/base

(require "type-rep.ss"
         "effect-rep.ss"
         "tc-utils.ss"
         "rep-utils.ss"
         "free-variance.ss"
         mzlib/plt-match
         (for-syntax scheme/base))

(provide fv fv/list
         substitute
         subst-all
         subst 
         ret
         instantiate-poly
         tc-result: 
         tc-result-equal? 
         effects-equal?
         tc-result-t)


;; substitute : Type Name Type -> Type
(define (substitute image name target)    
  (define (sb t) (substitute image name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case sb target
                 [#:F name* (if (eq? name* name) image target)])
      target))

;; substitute many variables
;; substitution = Listof[List[Name,Type]]
;; subst-all : substition Type -> Type
(define (subst-all s t)
  (foldr (lambda (e acc) (substitute (cadr e) (car e) acc)) t s))




(define (instantiate-poly t types)
  (match t
    [(Poly: ns body) 
     (unless (= (length types) (length ns))
       (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a" (length ns) (length types)))
     (subst-all (map list ns types) body)]
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
(define (effects-equal? fs1 fs2) 
  (and 
   (= (length fs1) (length fs2))
   (andmap eq? fs1 fs2)))


;; fv : Type -> Listof[Name]
(define (fv t) (hash-map (free-vars* t) (lambda (k v) k)))

;; fv/list : Listof[Type] -> Listof[Name]
(define (fv/list ts) (hash-map (combine-frees (map free-vars* ts)) (lambda (k v) k)))
