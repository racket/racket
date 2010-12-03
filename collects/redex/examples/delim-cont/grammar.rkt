#lang racket

(require redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression grammar:

;; `core-grammar' is typeset for the paper, but `grammar'
;; extends it with some standard things (such as arithmetic,
;; assignment, and output) to make testing easier.

(define-language core-grammar
  ;; Expressions
  ;;  Constrain `wcm' so that it can't
  ;;  wrap an immediate `wcm':
  (e m (wcm w m))
  (m x v (e e ...) (begin e e) (% e e e) (dw x e e e)) 
  
  ;; Values
  (v (list v ...) (λ (x ...) e) (cont v (hide-hole E)) (comp (hide-hole E))
     dynamic-wind abort current-marks
     cons u)
  (n number)
  
  ;; Primitives that need a wcm wrapper:
  (u call/cc call/comp call/cm)
  
  ;; Variables
  (x (variable-except λ if begin set!
                      call/cc cont 
                      % call/comp abort comp
                      dynamic-wind dw
                      call/cm wcm current-marks
                      zero? + print cons list first rest
                      * <>))
  
  ;; `wcm' bindings
  (w ((v v) ...))
  
  ;; Evaluation contexts
  ;;  Constrain `wcm' so it can't wrap a `wcm'.
  ;;  General evalation context:
  (E W (in-hole W (dw x e E e)))
  ;;  Evaluation context without `dw':
  (W M (wcm w M))
  (M hole (v ... W e ...) (begin W e) (% W e e) (% v e W) (% v W v))
  ;; Context ending on a dw boundary:
  (D hole (in-hole E (dw x e hole e))))

(define-extended-language grammar core-grammar
  
  (p (<> s       ; store
         (o ...) ; output
         e))     ; expression
  
  (m ....
     (if e e e)
     (set! x e))
  (v ....
     n #f #t
     zero? print + first rest)
  
  ;; Store
  (s ([x v] ...))
  
  ;; Output
  (o number)
  
  (M ....
     (set! x W)
     (if W e e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging:

(define-syntax (define-language-predicates stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([? (datum->syntax
                       #'id
                       (string->symbol (format "~s?" (syntax-e #'id)))
                       #'id)])
       #'(define ? (redex-match grammar id)))]
    [(_ id ...)
     #'(begin (define-language-predicates id) ...)]))

(define-language-predicates p e x v E)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide core-grammar grammar
         p? e? x? v? E?)
