#lang racket

(provide % abort call/comp call/cm current-marks
         (rename-out [_call/cc call/cc]
                     [_if if] 
                     [_+ +]
                     [_print print]
                     [_cons cons]
                     [_set! set!]
                     [_zero? zero?])
         (struct-out runtime-error) raise-runtime-error first-error)

(define tag
  (let ([tags (make-hash)])
    (λ (v)
      (hash-ref tags v 
                (λ ()
                  (let ([t (make-continuation-prompt-tag)])
                    (hash-set! tags v t)
                    t))))))

(define-syntax-rule (% tag-val expr handler)
  (call-with-continuation-prompt 
   (λ () expr)
   (let ([v tag-val])
     (if (let comparable? ([v v])
           (cond [(procedure? v) #f]
                 [(list? v) (andmap comparable? v)]
                 [else #t]))
         (tag v)
         (raise-runtime-error '% "non-procedure" v)))
   (let ([h handler])
     (λ (x) (h x)))))

(define (abort tag-val result) 
  (abort-current-continuation (tag tag-val) result))

(define ((force-unary f) x) (f x))

(define (_call/cc proc tag-val)
  (call/cc (compose proc force-unary) (tag tag-val)))

(define (call/comp proc tag-val) 
  (call-with-composable-continuation (compose proc force-unary) (tag tag-val)))

(define (call/cm key val thunk)
  (with-continuation-mark key val (thunk)))

(define (current-marks key tag-val)
  (continuation-mark-set->list 
   (current-continuation-marks (tag tag-val))
   key))

(define-syntax-rule (_if e1 e2 e3)
  (let ([v1 e1])
    (case v1
      [(#t) e2]
      [(#f) e3]
      [else (raise-runtime-error 'if "#t or #f" v1)])))

(define (_+ x y) (+ x y))

(define (_print n)
  (if (number? n)
      (begin (print n) #f)
      (raise-runtime-error 'print "number" n)))

(define (_cons x xs)
  (if (list? xs)
      (cons x xs)
      (raise-runtime-error 'cons "list?" xs)))

(define-syntax-rule (_set! x e)
  (begin (set! x e) #f))

(define (_zero? x)
  (equal? 0 x))

(define first-error (make-parameter #f))
(struct runtime-error (primitive expected given) #:prefab)
(define (raise-runtime-error . details)
  (define error (apply runtime-error details))
  (when (first-error)
    (unless (unbox (first-error))
      (set-box! (first-error) error)))
  (raise error))