#lang racket/base
(require syntax/datum
         syntax/macro-testing)

(define (do-test expect got expr)
  (unless (equal? expect got) (error "failed\n" expr)))


(define-syntax test 
  (syntax-rules (datum-case datum)
    [(_ expect (datum-case expr () [pat (datum tmpl)]))
     (begin
       (test expect (values (datum-case expr () [pat (datum tmpl)])))
       (test expect (with-datum ([pat expr]) (datum tmpl)))
       (test expect (let ()
                      (define/with-datum pat expr)
                      (datum tmpl))))]
    [(_ expect expr)
     (do-test expect expr 'expr)]))

(test '(3 2 1)
      (datum-case '(1 2 3) ()
        [(a b c) (datum (c b a))]))

(test '(3 1 2)
      (datum-case '(1 2 3) ()
        [(a ... c) (datum (c a ...))]))

(test '(3 1 2)
      (datum-case '#(1 2 3) ()
        [#(a ... c) (datum (c a ...))]))

(test '(3 2 1)
      (datum-case '#(1 2 3) ()
        [#(a b c) (datum (c b a))]))

(test 5
      (datum-case '#&5 ()
        [#&x (datum x)]))

(test '(3 2 1)
      (datum-case '#&(1 2 3) ()
        [#&(a b c) (datum (c b a))]))

(test '(5)
      (datum-case '#&((((5)))) ()
        [#&((((x)))) (datum (x))]))

(test '(3 2 1)
      (datum-case '#s(q 1 2 3) ()
        [#s(q a b c) (datum (c b a))]))

(test '(3 2 1)
      (datum-case '(1 ! 2 % 3) (! %)
        [(a ! b % c) (datum (c b a))]))

(test '(3 2 1)
      (datum-case '#(1 ! 2 % 3) (! %)
        [#(a ! b % c) (datum (c b a))]))

(test 'x
      (datum x))

(test 'x
      (quasidatum x))
(test '(1 2 3)
      (quasidatum (1 (undatum (+ 1 1)) 3)))
(test '#(1 2 3)
      (quasidatum #(1 (undatum (+ 1 1)) 3)))
(test '(1 2 3)
      (quasidatum (1 (undatum-splicing (list (+ 1 1) 3)))))
(test '(1 2 3 4)
      (quasidatum (1 (undatum-splicing (list (+ 1 1) 3)) 4)))


(test '(0 (1 2 3))
      (datum-case '(1 2 3) ()
        [p (datum (0 p))]))

(test '(0 1 2 3)
      (datum-case '(1 2 3) ()
        [(n ...) (datum (0 n ...))]))

(test '(0 1 3 6 (2 $) (4 5 $) ($))
      (datum-case '(0 (1 2) (3 4 5) (6)) ()
        [(0 (m n ...) ...) (datum (0 m ... (n ... $) ...))]))
