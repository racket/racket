#lang typed/racket
(require "array-struct.rkt")
(provide for/array for*/array
         for/strict-array for*/strict-array)

; (for/strict-array elm-type de(m1 m2 ... mn) (clause ...) . defs+exprs)
;    Return an  m1 x m2 x ... mn  strict array with elements from the last expr.
;    The bindings in clauses run in parallel.
(define-syntax (for/strict-array stx)
  (syntax-case stx ()
    [(_ elm-type default-val () (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: v : (Vectorof elm-type) (make-vector 1 default-val))
         (for: ([i (in-range 0 1)] clause ...) 
           (define x (let () . defs+exprs))
           (vector-set! v i x))
         (unsafe-strict-array #() v)))]
    [(_ elm-type default-val (m1-expr) (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m1 : Index m1-expr)
         (define: v : (Vectorof elm-type) (make-vector m1 default-val))
         (for: ([i (in-range 0 m1)] clause ...) 
           (define x (let () . defs+exprs))
           (vector-set! v i x))
         (define: ds : (Vectorof Index) (vector m1))
         (unsafe-strict-array ds v)))]
    [(_ elm-type default-val (m-expr ...) (clause ...) . defs+exprs)
     (with-syntax ([(m ...) (generate-temporaries #'(m-expr ...))])
       (syntax/loc stx
         (let ()
           (define: m : Index m-expr) ...
           (define: length : Index (assert (* m ...) index?))
           (define: v : (Vectorof elm-type) (make-vector length default-val))
           (for: ([i (in-range 0 length)] clause ...)
             (define x (let () . defs+exprs))
             (vector-set! v i x))
           (define: ds : (Vectorof Index) (vector m ...))
           (unsafe-strict-array ds v))))]
    [_ (raise-syntax-error 
        'for/array 
        "expected (for/array <elm-type> <default-val> (<m-expr> ...) (for:-clause ...) . <defs+exprs>)")]))

(define-syntax (for*/strict-array stx)
  (syntax-case stx ()
    [(_ elm-type default-val () (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: v : (Vectorof elm-type) (make-vector 1 default-val))
         (define: i : Index 0) 
         (for*: (clause ...) 
           (define x (let () . defs+exprs))
           (vector-set! v i x)
           (set! i (assert (+ i 1) index?)))
         (unsafe-strict-array #() v)))]
    [(_ elm-type default-val (m1-expr) (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m1 : Index m1-expr)
         (define: v : (Vectorof elm-type) (make-vector m1 default-val))
         (define: i : Index 0)
         (let/ec: return : 'done
           (for*: ([i (in-range 0 m1)] clause ...)
             (define x (let () . defs+exprs))
             (vector-set! v i x)
             (set! i (assert (+ i 1) index?))
             (when (= i m1) (return 'done))))
         (define: ds : (Vectorof Index) (vector m1))
         (unsafe-strict-array ds v)))]
    [(_ elm-type default-val (m-expr ...) (clause ...) . defs+exprs)
     (with-syntax ([(m ...) (generate-temporaries #'(m-expr ...))])
       (syntax/loc stx
         (let ()
           (define: m : Index m-expr) ...
           (define: length : Index (assert (* m ...) index?))
           (define: v : (Vectorof elm-type) (make-vector length default-val))
           (define: i : Index 0)
           (let/ec: return : 'done
             (for*: ([i (in-range 0 length)] clause ...)
               (define x (let () . defs+exprs))
               (vector-set! v i x)
               (set! i (assert (+ i 1) index?))
               (when (= i length) (return 'done))))
           (define: ds : (Vectorof Index) (vector m ...))
           (unsafe-strict-array ds v))))]
    [_ (raise-syntax-error 
        'for/array 
        "expected (for*/array <elm-type> <default-val> (<m-expr> ...) (for:-clause ...) . <defs+exprs>)")]))


(define-syntax (for/array stx)
  (syntax-case stx ()
    [(_ . more)
     #'(array-view (for/strict-array . more))]))
       
(define-syntax (for*/array stx)
  (syntax-case stx ()
    [(_ . more)
     #'(array-view (for*/strict-array . more))]))


    

