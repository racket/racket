#lang racket/base
(require (for-template racket/base))
(provide txlift
         get-txlifts
         get-txlifts-as-definitions
         call/txlifts
         with-txlifts
         with-txlifts/defs)

;; Like lifting definitions, but within a single transformer.

(define current-liftbox (make-parameter #f))

(define (call/txlifts proc)
  (parameterize ((current-liftbox (box null)))
    (proc)))

(define (txlift expr)
  (let ([liftbox (current-liftbox)])
    (check 'txlift liftbox)
    (let ([var (car (generate-temporaries '(txlift)))])
      (set-box! liftbox (cons (list var expr) (unbox liftbox)))
      var)))

(define (get-txlifts)
  (let ([liftbox (current-liftbox)])
    (check 'get-txlifts liftbox)
    (reverse (unbox liftbox))))

(define (get-txlifts-as-definitions)
  (let ([liftbox (current-liftbox)])
    (check 'get-txlifts-as-definitions liftbox)
    (map (lambda (p)
           #`(define #,@p))
         (reverse (unbox liftbox)))))

(define (check who lb)
  (unless (box? lb)
    (error who "not in a txlift-catching context")))

(define (with-txlifts proc)
  (call/txlifts
   (lambda ()
     (let ([v (proc)])
       (with-syntax ([((var rhs) ...) (get-txlifts)])
         #`(let* ([var rhs] ...) #,v))))))

(define (with-txlifts/defs proc)
  (call/txlifts
   (lambda ()
     (let ([v (proc)])
       (with-syntax ([(def ...) (get-txlifts-as-definitions)])
         #`(begin def ... #,v))))))
