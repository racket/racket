#lang racket/base
(require racket/contract)
(provide uf-set? uf-new)
(provide
 (contract-out
  [uf-union! (-> uf-set? uf-set? void?)]
  [uf-find (-> uf-set? any/c)]
  [uf-set-canonical! (-> uf-set? any/c void?)]
  [uf-same-set? (-> uf-set? uf-set? boolean?)]))

(struct uf-set (x rank) #:mutable
  #:methods gen:custom-write
  [(define write-proc 
     (λ (uf port mode)
       (write-string "#<uf-set: " port)
       (define recur
         (case mode
           [(#t) write]
           [(#f) display]
           [else (λ (p port) (print p port mode))]))
       (recur (uf-find uf) port)
       (write-string ">" port)))])

(define (uf-new x) (uf-set (box x) 0))

(define (uf-union! _a _b) 
  (define a (uf-get-root _a))
  (define b (uf-get-root _b))
  (unless (eq? a b)
    (define a-rank (uf-set-rank a))
    (define b-rank (uf-set-rank b))
    (cond
      [(< a-rank b-rank)
       (set-uf-set-x! a b)]
      [else
       (set-uf-set-x! b a)
       (when (= a-rank b-rank)
         (set-uf-set-rank! a (+ a-rank 1)))])))

(define (uf-find a) (unbox (uf-get-box a)))

(define (uf-set-canonical! a b)
  (set-box! (uf-get-box a) b))

(define (uf-same-set? a b)
  (eq? (uf-get-box a) (uf-get-box b)))

(define (uf-get-box a) (uf-set-x (uf-get-root a)))

(define (uf-get-root a)
  (let loop ([c a]
             [p (uf-set-x a)])
    (cond
      [(box? p) c]
      [else
       (define fnd (loop p (uf-set-x p)))
       (set-uf-set-x! c fnd)
       fnd])))

(module+ test
  (require rackunit 
           racket/pretty
           racket/set)
  
  (check-equal? (uf-find (uf-new 1)) 1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find a))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find b))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find a)
                  (uf-find a))
                1)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)])
                  (uf-union! a b)
                  (uf-find b)
                  (uf-find b))
                1)
  
  (check-equal? (let ([a (uf-new 1)])
                  (uf-union! a a)
                  (uf-find a))
                1)
  
  (check-equal? (uf-same-set? (uf-new 1) (uf-new 2)) #f)
  (check-equal? (uf-same-set? (uf-new 1) (uf-new 1)) #f)
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 1)])
                  (uf-union! a b) 
                  (uf-same-set? a b))
                #t)
  (check-equal? (let ([sp (open-output-string)])
                  (display (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: x>")
  (check-equal? (let ([sp (open-output-string)])
                  (write (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: \"x\">")
  (check-equal? (let ([sp (open-output-string)])
                  (print (uf-new "x") sp)
                  (get-output-string sp))
                "#<uf-set: \"x\">")
  (check-equal? (let ([sp (open-output-string)])
                  (define x (vector 1))
                  (define a (uf-new x))
                  (vector-set! x 0 a)
                  (write x sp)
                  (get-output-string sp))
                "#0=#(#<uf-set: #0#>)")
  (check-equal? (let ([sp (open-output-string)])
                  (define a (uf-new #f))
                  (uf-set-canonical! a a)
                  (write a sp)
                  (get-output-string sp))
                "#0=#<uf-set: #0#>")
  
  
  (let ([a (uf-new 1)]
        [b (uf-new 2)]
        [c (uf-new 3)]
        [d (uf-new 4)]
        [e (uf-new 5)])
    (uf-union! a b)
    (uf-union! c d)
    (uf-union! b d)
    (uf-union! c e)
    (check-equal? (uf-find a)
                  (uf-find e)))
  
  (let ([a (uf-new 1)]
        [b (uf-new 2)]
        [c (uf-new 3)]
        [d (uf-new 4)]
        [e (uf-new 5)])
    (uf-union! a b)
    (uf-union! c d)
    (uf-union! a c)
    (uf-union! c e)
    (check-equal? (uf-find a)
                  (uf-find e)))
  
  (let ([a (uf-new 1)]
        [b (uf-new 2)]
        [c (uf-new 3)]
        [d (uf-new 4)]
        [e (uf-new 5)])
    (uf-union! a b)
    (uf-union! c d)
    (uf-union! a d)
    (uf-union! c e)
    (check-equal? (uf-find a)
                  (uf-find e)))
  
  (let ([a (uf-new 1)]
        [b (uf-new 2)]
        [c (uf-new 3)]
        [d (uf-new 4)]
        [e (uf-new 5)])
    (uf-union! a b)
    (uf-union! c d)
    (uf-union! b c)
    (uf-union! c e)
    (check-equal? (uf-find a)
                  (uf-find e)))
  
  (check-equal? (let ([a (uf-new 1)]
                      [b (uf-new 2)]
                      [c (uf-new 3)]
                      [d (uf-new 4)])
                  (uf-union! a b)
                  (uf-union! c d)
                  (uf-union! a c)
                  (max (uf-set-rank a)
                       (uf-set-rank b)
                       (uf-set-rank c)
                       (uf-set-rank d)))
                2)
  
  (let ((uf-sets (for/list ((x (in-range 8))) (uf-new x))))
    (uf-union! (list-ref uf-sets 5) (list-ref uf-sets 7))
    (uf-union! (list-ref uf-sets 1) (list-ref uf-sets 6))
    (uf-union! (list-ref uf-sets 6) (list-ref uf-sets 5))
    (uf-union! (list-ref uf-sets 4) (list-ref uf-sets 7))
    (uf-union! (list-ref uf-sets 2) (list-ref uf-sets 0))
    (uf-union! (list-ref uf-sets 2) (list-ref uf-sets 5))
    (check-equal? (uf-find (list-ref uf-sets 4))
                  (uf-find (list-ref uf-sets 7))))
  
  
  (define (run-random-tests)
    (define (make-random-sets num-sets)
      (define uf-sets 
        (for/list ([x (in-range num-sets)])
          (uf-new x)))
      (define edges (make-hash (build-list num-sets (λ (x) (cons x (set))))))
      (define (add-edge a-num b-num)
        (hash-set! edges a-num (set-add (hash-ref edges a-num) b-num)))
      (define ops '())
      (for ([op (in-range (random 10))])
        (define a-num (random num-sets))
        (define b-num (random num-sets))
        (define a (list-ref uf-sets a-num))
        (define b (list-ref uf-sets b-num))
        (set! ops (cons `(uf-union! (list-ref uf-sets ,a-num)
                                    (list-ref uf-sets ,b-num))
                        ops))
        (uf-union! a b)
        (add-edge a-num b-num)
        (add-edge b-num a-num))
      (define code `(let ([uf-sets 
                           (for/list ([x (in-range ,num-sets)])
                             (uf-new x))])
                      ,@(reverse ops)))
      (values uf-sets edges code))
    
    (define (check-canonical-has-path uf-sets edges code)
      (for ([set (in-list uf-sets)]
            [i (in-naturals)])
        (define canon (uf-find set))
        (define visited (make-hash))
        (define found?
          (let loop ([node i])
            (cond
              [(= node canon) #t]
              [(hash-ref visited node #f) #f]
              [else
               (hash-set! visited node #t)
               (for/or ([neighbor (in-set (hash-ref edges node))])
                 (loop neighbor))])))
        (unless found?
          (pretty-print code (current-error-port))
          (error 'union-find.rkt "mismatch; expected a link from ~s to ~s, didn't find it"
                 i canon))))
    
    (define (check-edges-share-canonical uf-sets edges code)
      (for ([(src dests) (in-hash edges)])
        (for ([dest (in-set dests)])
          (define sc (uf-find (list-ref uf-sets src)))
          (define dc (uf-find (list-ref uf-sets dest)))
          (unless (= sc dc)
            (pretty-print code (current-error-port))
            (error 'union-find.rkt 
                   "mismatch; expected sets ~s and ~s to have the same canonical element, got ~s and ~s"
                   src dest
                   sc dc)))))
    
    (for ([x (in-range 10000)])
      (define-values (sets edges code) 
        (make-random-sets (+ 2 (random (+ 1 (floor (/ x 100)))))))
      (check-canonical-has-path sets edges code)
      (check-edges-share-canonical sets edges code)))
  
  (run-random-tests)
  
  (random-seed 0)
  (time (run-random-tests)))
