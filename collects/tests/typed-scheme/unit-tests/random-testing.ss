#lang scheme/base

(require (planet cce/fasttest/random)
         "test-utils.ss")
(require (private type-effect-convenience type-rep)
         scheme/match)



(define base (random-uniform (random-apply (lambda (n) #`(quote #,n)) (random-int-between 1 100))))

(define (N? t)
  (match t
    [(Base: 'Number) #t]
    [_ #f]))

(define (make-lam formals body)
  #`(#%plain-lambda #,formals #,body))


(define random-id
  (random-apply datum->syntax #f (random-symbol)))

(define (make-app f . args)
  #`(#%plain-app #,f #,@args))

;; ty-gen : size -> generator[type]
(define-generator (ty-gen max-depth)
  [3 N]
  [(if (< max-depth 1) 0 1)
   (random-apply (lambda (args ret) (args . ->* . ret))
                 (random-list-of (ty-gen (sub1 max-depth)) (random-int-between 0 3))
                 (ty-gen (sub1 max-depth)))])

;; base-gen : number -> generator[syntax]
(define-generator (base-gen max-depth)
  [10 base]
  [(if (< max-depth 1) 0 1)
   (let*-random ([arg-tys (random-list-of (ty-gen (sub1 max-depth)) (random-int-between 0 (max 0 3 max-depth)))])
     (let* ([args (map (lambda (t) (term-gen t (sub1 max-depth))) arg-tys)])
       (random-apply
        apply
        make-app
        (term-gen (arg-tys . ->* . N) (sub1 max-depth))
        (map generate args))))])

    

;; term-gen : type size -> generator[syntax]
(define-generator (term-gen ty max-depth)
  [1
   (match ty
     [(? N?) (base-gen (sub1 max-depth))]
     [(Function: (list (arr: args ret _ _ _ _)))
      (cond [(and (> (length args) 0) (andmap N? args))
             (random-uniform #'+ #'- #'* #'-)]
            [(andmap N? args)
             (random-uniform #'+ #'*)]
            [else
             (random-apply make-lam
                           (random-list-of random-id (length args))
                           (term-gen ret (sub1 max-depth)))])]
     [_ (error "epic fail")])])

(define (go [n 3])
  (generate (random-apply term-gen (ty-gen n) n)))

(go 0)

;(generate (base-gen 1))