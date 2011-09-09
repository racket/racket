#lang typed/scheme

;; variables
(define-type Var Symbol)
(define-type UserVar Var)
(define-struct: ContVar ([v : Var]))
(define-type AnyVar (U UserVar ContVar))

;; constructors
(define-struct: (V A) lamV ([x : V] [b : A]))
(define-struct: (A B) app ([rator : A] [rand : B]))

(define-type (lam A) (lamV Var A))
(define-type (lamC A) (lamV ContVar A))

;; non-partitioned CPS (from Dimitrios Vardoulakis)
(define-type Λ (Rec L (U Var (lam L) (app L L))))
(define-type Uv (Rec Uv
                     (U Var (lam (lam (U (app (app Uv Uv) (Rec Cv (U Var (lam (U (app (app Uv Uv) Cv)
                                                                                 (app Cv Uv))))))
                                         (app (Rec Cv (U Var (lam (U (app (app Uv Uv) Cv)
                                                                     (app Cv Uv)))))
                                              Uv)))))))

(define-type Cv (Rec Cv (U Var (lam (U (app (app Uv Uv) Cv)
                                       (app Cv Uv))))))
(define-type Call (U (app (app Uv Uv) Cv)
                     (app Cv Uv)))
(define-type CPS (U Cv Uv Call))

(: f (CPS -> Λ))
(define (f x) x)

;; partitioned CPS (from Sabry and Felleisen LaSC 93)
(define-type Λ2 (Rec L (U AnyVar (lamV AnyVar L) (app L L))))

(define-type K (Rec K (U ContVar (app (U Var (lamC K)) K) (lam (app K (U Var (lamC K)))))))
(define-type P (app K (U Var (lamC K))))
(define-type W (U Var (lamC K)))

(: f* (P -> Λ2))
(define (f* x) x)
