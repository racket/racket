#lang scheme

(require "teachprims.ss" "teach.ss")

(provide intermediate-andmap intermediate-ormap)

(define-teach intermediate andmap
  (lambda (f l)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (hocheck 'andmap "first argument must be a <procedure> that accepts one argument, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'andmap "second argument must be of type <list>, given ~e" l))
    (let loop ([l l])
      (if (null? l) #t (beginner-and (f (car l)) (loop (cdr l)))))))

(define-teach intermediate ormap
  (lambda (f l)
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (hocheck 'andmap "first argument must be a <procedure> that accepts one argument, given ~e" f))
    (unless (beginner-list? l) 
      (hocheck 'andmap "second argument must be of type <list>, given ~e" l))
    (let loop ([l l])
      (if (null? l) #f (beginner-or (f (car l)) (loop (cdr l)))))))


#| TESTS 

(with-handlers ((exn:fail:contract? void))
  (intermediate-ormap cons '(1 2 3)))

(with-handlers ((exn:fail:contract? void))
  (intermediate-andmap cons '(1 2 3)))

(with-handlers ((exn:fail:contract? void))
  (intermediate-ormap add1 1))

(with-handlers ((exn:fail:contract? void))
  (intermediate-andmap add1 1))

(with-handlers ((exn:fail:contract? void))
  (intermediate-ormap (lambda (x) x) '(1 2 3)))

(with-handlers ((exn:fail:contract? void))
  (intermediate-andmap (lambda (x) x) '(1 2 3)))

(with-handlers ((exn:fail:contract? void))
  (intermediate-andmap add1 '(1 2 3)))

(unless (equal? (intermediate-ormap odd? '(1 2 3)) #t) (error 'x "1"))
(unless (equal? (intermediate-andmap odd? '(1 2 3)) #f) (error 'x "2"))
(unless (equal? (intermediate-andmap odd? '(1 3 5)) #t) (error 'x "3"))
(unless (equal? (intermediate-ormap even? '(1 3 5)) #f) (error 'x "1"))

|#
