#lang racket/base
(require (for-template racket/base))
(provide txlift
         get-txlifts-as-definitions
         with-txlifts
         call/txlifts)

;; Like lifting definitions, but within a single transformer.

;; current-liftbox : Parameter of [#f or (Listof (list Id Stx))]
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

(define (get-txlifts [who 'gex-txlifts])
  (let ([liftbox (current-liftbox)])
    (check who liftbox)
    (begin0 (reverse (unbox liftbox))
      (set-box! liftbox null))))

(define (get-txlifts-as-definitions)
  (define lifts (get-txlifts 'get-txlifts-as-definitions))
  (map (lambda (p) #`(define #,@p)) lifts))

(define (check who lb)
  (unless (box? lb)
    (error who "not in a txlift-catching context")))

(define (with-txlifts proc)
  (call/txlifts
   (lambda ()
     (let ([v (proc)])
       (with-syntax ([((var rhs) ...) (get-txlifts)])
         #`(let* ([var rhs] ...) #,v))))))
