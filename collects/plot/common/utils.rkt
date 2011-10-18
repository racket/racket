#lang racket/base

;; Extra functions that can't be easily categorized (i.e. math, vector).

(require racket/sequence racket/list)

(provide (all-defined-out))


(define (sequence-take seq start end)
  (for/list ([e  (sequence-tail seq start)] 
             [_  (in-range (- end start))])
    e))

(define (list-index v lst [equal? equal?])
  (let loop ([lst lst] [idx 0])
    (cond [(null? lst)  -1]
          [(equal? v (car lst))  idx]
          [else  (loop (cdr lst) (add1 idx))])))

(define (assoc-cons hash key new-value)
  (let loop ([hash  hash])
    (cond [(empty? hash)  (list (cons key (list new-value)))]
          [else
           (define entry (first hash))
           (cond [(equal? (car entry) key)  (cons (cons key (cons new-value (cdr entry)))
                                                  (rest hash))]
                 [else  (cons (first hash) (loop (rest hash)))])])))

(define (vector-find-index pred? xs [start 0] [end (vector-length xs)])
  (for/first ([i  (in-range start end)]
              #:when (pred? (vector-ref xs i)))
    i))

(define ((sorted-apply sort f) lst)
  (define h
    (let ([sorted-lst  (sort lst)])
      (make-hash (map cons sorted-lst (f sorted-lst)))))
  (map (λ (e) (hash-ref h e)) lst))

(define (transpose lsts)
  (apply map list lsts))

(define (equal?* xs)
  (cond [(empty? xs)         #f]
        [(empty? (rest xs))  #t]
        [else  (and (equal? (first xs) (second xs))
                    (equal?* (rest xs)))]))

(define (group-neighbors lst equiv?)
  (reverse
   (map reverse
        (cond
          [(empty? lst)  empty]
          [else
           (for/fold ([res  (list (list (first lst)))]) ([e  (in-list (rest lst))])
             (cond
               [(andmap (λ (e2) (equiv? e e2)) (first res))  (cons (cons e (first res)) (rest res))]
               [else  (list* (list e) res)]))]))))

(define (parameterize-procedure t)
  (define parameterization (current-parameterization))
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (call-with-parameterization
      parameterization
      (λ () (keyword-apply t kws kw-args rest))))))
