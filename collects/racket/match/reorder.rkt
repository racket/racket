#lang racket/base

(require "patterns.rkt"
         (for-syntax racket/base))

(provide reorder-columns)

#|
(define p-x (make-Var #'x))
(define p-y (make-Var #'y))
(define p-d (make-Dummy #'_))

(define p-cons (make-Pair p-x p-y))
(define p-vec (make-Vector (list p-x p-y p-d)))

(define r1 (make-Row (list p-x) #'#f #f null))
(define r2 (make-Row (list p-y) #'#f #f null))
(define r3 (make-Row (list p-cons) #'#f #f null))
(define r4 (make-Row (list p-vec p-d) #'#f #f null))

(define r5 (make-Row (list p-x p-y p-cons) #'1 #f null))
(define r6 (make-Row (list p-cons p-y p-vec) #'1 #f null))
|#

(define-sequence-syntax in-par
    (lambda () (raise-syntax-error 'in-par "bad"))
    (lambda (stx)
      (syntax-case stx ()
        [((id) (_ lst-exprs))
         #'[(id)
            (:do-in
             ;;outer bindings
             ([(lst) lst-exprs])
             ;; outer check
             (void) ; (unless (list? lst) (in-list lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (not (ormap null? lst))
             ;; inner bindings
             ([(id) (map car lst)])
             ;; pre guard
             #t
             ;; post guard
             #t
             ;; loop args
             ((map cdr lst)))]]
        [_ (error 'no (syntax->datum stx))])))

(define (or-all? ps l)
  (ormap (lambda (p) (andmap p l)) ps))

(define (count-while pred l)
  (let loop ([l l] [r 0])
    (if (or (null? l) (not (pred (car l)))) r (loop (cdr l) (add1 r)))))

(define (score col)
  (define n (length col))
  (define c (car col))
  (define preds (list Var? Pair? Null?))
  (cond [(or-all? preds col) (add1 n)]
        [(andmap CPat? col) n]
        [(Var? c)    (count-while Var? col)]
        [(Pair? c)   (count-while Pair? col)]
        [(Vector? c) (count-while Vector? col)]
        [(Box? c)    (count-while Box? col)]
        [else 0]))

(define (reorder-by ps scores*)
  (for/fold 
      ([pats null])
    ([score-ref scores*])
    (cons (list-ref ps score-ref) pats)))


(define (reorder-columns rows vars)
  (define scores (for/list ([i (in-naturals)]
                            [column (in-par (map (compose Row-pats) rows))])
                   (cons i (score column))))
  (define scores* (reverse (map car (sort scores > #:key cdr))))
  (values
   (for/list ([row rows])
     (let ([ps (Row-pats row)])
       (make-Row (reorder-by ps scores*)
                 (Row-rhs row)
                 (Row-unmatch row)
                 (Row-vars-seen row))))
   (reorder-by vars scores*)))
