#lang racket/base

;; Extra functions that can't be easily categorized (i.e. math, vector).

(require racket/sequence racket/list racket/match)

(provide (all-defined-out))


(define (sequence-take seq start end)
  (for/list ([e  (sequence-tail seq start)] 
             [_  (in-range (- end start))])
    e))

(define (list-index v lst [equal? equal?])
  (for/first ([e  (in-list lst)] [i  (in-naturals)] #:when (equal? e v))
    i))

(define (list-duplicate-index lst)
  (let loop ([lst lst] [j 0])
    (cond [(empty? lst)  #f]
          [else
           (define fst (first lst))
           (define idx
             (for/first ([e  (in-list (rest lst))] [i  (in-naturals)] #:when (equal? e fst))
               (+ i j 1)))
           (if idx idx (loop (rest lst) (+ j 1)))])))

(define (assoc-cons hash key new-value)
  (let loop ([hash  hash])
    (cond [(empty? hash)  (list (cons key (list new-value)))]
          [else
           (define entry (first hash))
           (cond [(equal? (car entry) key)  (cons (cons key (cons new-value (cdr entry)))
                                                  (rest hash))]
                 [else  (cons (first hash) (loop (rest hash)))])])))

(define (vector-find-index pred? xs [start 0] [end (vector-length xs)])
  (for/first ([i  (in-range start end)] #:when (pred? (vector-ref xs i)))
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

#;
(define (parameterize-procedure t)
  (define parameterization (current-parameterization))
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (call-with-parameterization
      parameterization
      (λ () (keyword-apply t kws kw-args rest))))))

;; f : any -> any
;; Returns a wrapper for 'f' that preserves most of the parameter values
;; in the dynamic extent where 'parameterize-procedure' is applied.
(define (parameterize-procedure f)
  (struct apply-thread (channel thread) #:transparent)
  (struct apply-command (kws kw-values rest) #:transparent)
  (struct exception-response (exception) #:transparent)
  (struct values-response (values) #:transparent)
  ;; A synchronous channel for commands and responses
  (define ch (make-channel))
  ;; The command loop
  (define (command-loop)
    (match-define (apply-command kws kw-values rest) (channel-get ch))
    (with-handlers ([(λ (e) #t)  (λ (e) (channel-put ch (exception-response e)))])
      (channel-put ch (call-with-values (λ () (keyword-apply f kws kw-values rest))
                                        (λ vals (values-response vals)))))
    (command-loop))
  ;; Save the thread in a struct so it'll get closed over
  (define th (apply-thread ch (thread command-loop)))
  ;; Return the wrapper
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (match-define (apply-thread ch _) th)
     (channel-put ch (apply-command kws kw-args rest))
     (match (channel-get ch)
       [(exception-response e)  (raise e)]
       [(values-response vals)  (apply values vals)]))))
