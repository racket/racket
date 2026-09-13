#lang racket/base

(provide running-foldl
         running-foldr)

(module+ test (require rackunit))

(define running-foldl
  (case-lambda
    [(f init l)
     (check-running-fold 'running-foldl f init l null)
     (let loop ([l l] [acc init] [result null])
       (if (null? l)
           (reverse (cons acc result))
           (loop (cdr l) (f (car l) acc) (cons acc result))))]
    [(f init l . ls)
     (check-running-fold 'running-foldl f init l ls)
     (let loop ([ls (cons l ls)] [acc init] [result null])
       (if (null? (car ls)) ; `check-running-fold' ensures all lists have equal length
           (reverse (cons acc result))
           (loop (map cdr ls) 
                 (apply f (map-add car ls acc))
                 (cons acc result))))]))

(module+ test
  (check-equal? (running-foldl + 0 '()) '(0))
  (check-equal? (running-foldl + 0 '(1 2 3)) '(0 1 3 6))
  (check-equal? (running-foldl * 1 '(2 3 0 4)) '(1 2 6 0 0))
  (check-equal? (running-foldl (lambda (val _) val) 1 '(2 2 2)) '(1 2 2 2))
  (check-exn exn:fail:contract? (lambda () (running-foldl 1 0 0)))
  (check-exn exn:fail:contract? (lambda () (running-foldl + 0 0)))
  ; variadic tests
  (check-equal? (running-foldl + 0 '() '()) '(0))
  (check-equal? (running-foldl + 1 '(1 2 3) '(1 2 3)) '(1 3 7 13))
  (check-equal? (running-foldl * 1 '(2 3 0 4) '(1 4 4 5)) '(1 2 24 0 0))
  (check-exn exn:fail:contract? (lambda () (running-foldl + 0 '(1) '())))
  (check-exn exn:fail:contract? (lambda () (running-foldl + 0 '() (1)))))


(define running-foldr
  (case-lambda
    [(f init l)
     (check-running-fold 'running-foldr f init l null)
     (let loop ([l l])
       (if (null? l)
           (list init)
           (let ([accs (loop (cdr l))])
             (cons (f (car l) (car accs)) accs))))]
    [(f init l . ls)
     (check-running-fold 'running-foldr f init l ls)
     (let loop ([ls (cons l ls)])
       (if (null? (car ls)) ; `check-running-fold' ensures all lists have equal length
           (list init)
           (let ([accs (loop (map cdr ls))])
             (cons (apply f (map-add car ls (car accs))) 
                   accs))))]))

(module+ test
  (check-equal? (running-foldr + 0 '()) '(0))
  (check-equal? (running-foldr + 0 '(1 2 3)) '(6 5 3 0))
  (check-equal? (running-foldr * 1 '(2 3 0 4)) '(0 0 0 4 1))
  (check-equal? (running-foldr (lambda (val _) val) 1 '(2 2 2)) '(2 2 2 1))
  (check-exn exn:fail:contract? (lambda () (running-foldr 1 0 0)))
  (check-exn exn:fail:contract? (lambda () (running-foldr + 0 0)))
  ; variadic tests
  (check-equal? (running-foldr + 0 '() '()) '(0) )
  (check-equal? (running-foldr + 1 '(1 2 3) '(1 2 3)) '(13 11 7 1))
  (check-equal? (running-foldr * 1 '(2 3 0 4) '(1 4 4 5)) '(0 0 0 20 1) )
  (check-exn exn:fail:contract? (lambda () (running-foldr + 0 '(1) '())))
  (check-exn exn:fail:contract? (lambda () (running-foldr + 0 '() (1)))))


; helpers
(define (map-add f ls v)
  (let loop ([ls ls] [result null])
    (if (null? ls)
      (reverse (cons v result))
      (loop (cdr ls) (cons (f (car ls)) result)))))

(define (check-running-fold name proc init l more)
  (unless (procedure? proc)
    (apply raise-argument-error name "procedure?" 0 proc init l more))
  (unless (list? l)
    (apply raise-argument-error name "list?" 2 proc init l more))

  (if (null? more)
      (unless (procedure-arity-includes? proc 2)
        (raise-argument-error name "(-> any/c any/c any/c)" proc))
      (let ([len (length l)])
        (let loop ([remaining more] [n 3])
          (unless (null? remaining)
            (unless (list? (car remaining))
              (apply raise-argument-error name "list?" n proc init l more))
            (unless (eq? len (length (car remaining)))
              (raise-argument-error name
                                    "given list does not have the same size as the first list"
                                    (car remaining)))
            (loop (cdr remaining) (add1 n))))
        (unless (procedure-arity-includes? proc (+ 2 (length more)))
          (raise-argument-error name
                                (format "given procedure does not accept ~a arguments"
                                        (+ 2 (length more)))
                                proc)))))

