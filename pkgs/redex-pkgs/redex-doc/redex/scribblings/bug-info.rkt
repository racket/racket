#lang racket/base

(provide get-line-count
         get-counterexample
         get-error
         get-category
         get-counterexample-size
         get-counterexample-depth
         all-types/nums
         type->num->cat)

(define type->files
  (hash 'stlc '(("stlc.rkt" "redex/examples"))
        'poly-stlc '(("poly-stlc.rkt" "redex/examples"))
        'stlc-sub '(("stlc+lists+subst.rkt" "redex/examples"))
        'let-poly '(("let-poly.rkt" "redex/examples"))
        'rbtrees '(("rbtrees.rkt" "redex/examples"))
        'list-machine '(("list-machine.rkt" "redex/examples/list-machine")
                        ("list-machine-typing.rkt" "redex/examples/list-machine"))
        'delim-cont '(("grammar.rkt" "redex/examples/delim-cont")
                      ("reduce.rkt" "redex/examples/delim-cont")
                      ("meta.rkt" "redex/examples/delim-cont"))
        'rvm '(("verification.rkt" "redex/examples/racket-machine")
               ("grammar.rkt" "redex/examples/racket-machine")
               ("reduction.rkt" "redex/examples/racket-machine"))))

(define (type->bug-file type num)
  (collection-file-path (format "~a-~a.rkt" type num)
                        "redex" "benchmark" "models"
                        (format "~a" (type->dirname type))))

(define (type->dirname type)
  (case type
    [(stlc) 'stlc+lists]
    [(stlc-sub) 'stlc-subst]
    [else type]))

;; the order of this list is intended to match the
;; order of the subsections in the benchmark section
(define type->numof/order
  '((stlc 9)
    (poly-stlc 9)
    (stlc-sub 9)
    (let-poly 7)
    (list-machine 3)
    (rbtrees 3)
    (delim-cont 3)
    (rvm there-is-no-number-here)))

(define rvm-nums '(2 3 4 5 6 14 15))

(define all-types/nums
  (for*/list ([t (in-list (map car type->numof/order))]
              [n (if (equal? t 'rvm)
                     (in-list rvm-nums)
                     (in-range 1 (add1 (cadr (assoc t type->numof/order)))))])
    (list t n)))


(define type->num->cat
  (hash 
   'stlc 
   ; stlc: 1S 2M 3S 4S 5S 6M 7M 8? 9S
   (hash 1 'S
         2 'M
         3 'S
         4 'S
         5 'S
         6 'M
         7 'M
         8 'U
         9 'S)
   'poly-stlc
   ; poly-stlc: 1S 2M 3S 4S 5S 6M 7M 8? 9S
   (hash 1 'S
         2 'M
         3 'S
         4 'S
         5 'S
         6 'M
         7 'M
         8 'U
         9 'S)
   'stlc-sub
   ; stlc-sub: 1S 2S 3S 4M 5SM
   (hash 1 'S
         2 'S
         3 'S
         4 'D
         5 'SM
         6 'S
         7 'S
         8 'S
         9 'SM)
   'let-poly
   (hash 1 'S
         2 'D
         3 'M
         4 'S
         5 'M
         6 'M
         7 'D)
   'list-machine
   ; list-machine: 1S 2M 3S
   (hash 1 'S
         2 'M
         3 'S)
   'rbtrees
   ; rbtrees: 1SD 2SM 3SMD
   (hash 1 'M
         2 'M
         3 'S)
   'delim-cont
   ; delim-cont: 1M 2M 3SD
   (hash 1 'M
         2 'M
         3 'S)
   'rvm
   ; rvm: 2? 3D 4M 5M 6M 14M 15S
   (hash 2 'M
         3 'D
         4 'M
         5 'M
         6 'M
         14 'M
         15 'S)))

(define (get-category type num)
  (hash-ref (hash-ref type->num->cat type) num))

(define (get-line-count type)
  (number->string
   (for/sum ([cfp (in-list (hash-ref type->files type))])
     (define path (apply collection-file-path cfp))
     (line-count path))))

(define (bmark-path file)
  (collection-file-path file))

(define (line-count path)
  (call-with-input-file path
    (λ (in)
      (length
       (for/list ([line (in-lines in)]
                  #:unless (white-space/comment? line))
         'line)))))

(define (white-space/comment? line)
  (or (regexp-match? #rx"^[ \t]*$" line)
      (regexp-match? #rx"^[ \t]*;.*$" line)))

(define (get-counterexample-size type num)
  (define cx (get-counterexample type num))
  (count-size cx))

(define (get-counterexample-depth type num)
  (define cx (get-counterexample type num))
  (count-depth cx))

(define (get-counterexample type num)
  (define path (type->bug-file type num))
  (dynamic-require path 'small-counter-example))

(define (get-error type num)
  (define path (type->bug-file type num))
  (dynamic-require path 'the-error))

(define (count-depth l)
  (cond
    [(list? l) (+ 1 (apply max 0 (map count-depth l)))]
    [else 1]))

(define (count-size l)
  (cond
    [(list? l) (apply + 1 (map count-size l))]
    [else 1]))

(module+ test
  (require rackunit)
  (check-equal? (count-depth 1) 1)
  (check-equal? (count-depth '(1)) 2)
  (check-equal? (count-depth '((1))) 3)
  (check-equal? (count-depth '((1) (1))) 3)
  (check-equal? (count-depth '((1) (1) (1) (1 1 1 1 1 1))) 3)
  (check-equal? (count-size 1) 1)
  (check-equal? (count-size '(1)) 2)
  (check-equal? (count-size '((1))) 3)
  (check-equal? (count-size '((1) (1))) 5)
  (check-equal? (count-size '((1) (1) (1) (1 1 1 1 1 1))) 14))
