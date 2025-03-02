#lang racket/base 

(require racket/list)

(provide windows 
         slice-by)

(module+ test (require rackunit))

(define (windows size step ls)
  (unless (exact-positive-integer? size)
    (raise-argument-error 'windows "exact-positive-integer?" 0 size step ls))
  (unless (exact-positive-integer? step)
    (raise-argument-error 'windows "exact-positive-integer?" 1 size step ls))
  (unless (list? ls)
    (raise-argument-error 'windows "list?" 2 size step ls))

  (let loop ([ls ls] [l (length ls)])
    (if (or (null? ls) (< l size))
        null
        (cons 
          (take ls size)
          (if (< l step)
              null
              (loop (drop ls step) (- l step)))))))

(module+ test
  (check-equal? (windows 3 3 '(1 1 1 2 2 2 3 3 3)) '((1 1 1) (2 2 2) (3 3 3)))
  (check-equal? (windows 3 1 '(1 2 3 4 5)) '((1 2 3) (2 3 4) (3 4 5)))
  (check-equal? (windows 3 3 '(1 1 1 2 2)) '((1 1 1)))
  (check-equal? (windows 3 7 '(1 1 1 2 2)) '((1 1 1)))
  (check-equal? (windows 1 1 '(1 2 3)) '((1) (2) (3)))
  (check-equal? (windows 1 2 '(1 2 3 4)) '((1) (3)))
  (check-equal? (windows 2 2 '()) '())
  (check-exn exn:fail:contract? (lambda () (windows 0 1 '(1))))
  (check-exn exn:fail:contract? (lambda () (windows 1 0 '(1))))
  (check-exn exn:fail:contract? (lambda () (windows 1 1 (void)))))


(define (slice-by f ls)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 2))
    (raise-argument-error 'slice-by "(-> any/c any/c any/c)" 0 f ls))
  (unless (list? ls)
    (raise-argument-error 'slice-by "list?" 1 f ls))

  (if (null? ls)
      null
      (let loop ([ls ls] [slice null] [result null])
        (cond [(null? (cdr ls)) (reverse (cons (reverse (cons (car ls) slice)) result))]
              [(f (car ls) (cadr ls)) (loop (cdr ls) (cons (car ls) slice) result)]
              [else (loop (cdr ls) 
                          null 
                          (cons (reverse (cons (car ls) slice)) result))]))))

(module+ test
  (define (on f g) (lambda (x y) (f (g x) (g y))))

  (check-equal? (slice-by eq? '()) '())
  (check-equal? (slice-by = '(1)) '((1)))
  (check-equal? (slice-by eq? '(1 1 2 2 3 3)) '((1 1) (2 2) (3 3)))
  (check-equal? (slice-by < '(1 1 2 2)) '((1) (1 2) (2)))
  (check-equal? (slice-by (on eq? positive?) '(1 2 3 -1 -2 -3 4)) '((1 2 3) (-1 -2 -3) (4)))
  (check-exn exn:fail:contract? (lambda () (slice-by positive? '())))
  (check-exn exn:fail:contract? (lambda () (slice-by equal? (void)))))
