#lang racket/base
(require racket/contract
         racket/list)

(struct machine (guts next)
        #:mutable
        #:property prop:procedure
        (λ (m i)
          ((machine-next m) i)))
(struct machine-accepting machine ())

(define (machine->accepting m)
  (if (machine-accepting? m)
      m
      (machine-accepting 
       (machine-guts m) 
       (machine-next m))))
(define (machine->non-accepting m)
  (if (machine-accepting? m)
      (machine 
       (machine-guts m) 
       (machine-next m))
      m))
(define (replace-guts ng m)
  (define const
    (if (machine-accepting? m) machine-accepting machine))
  (const ng (machine-next m)))  

(define (machine-complement m)
  (define const
    (if (machine-accepting? m) machine machine-accepting))
  (const
   `(complement ,m)
   (λ (input)
     (machine-complement (m input)))))

(define (machine-union m1 m2)
  (cond
    [(eq? m1 machine-null)
     m2]
    [(eq? m2 machine-null)
     m1]
    [(eq? m1 machine-epsilon)
     (machine->accepting m2)]
    [(eq? m2 machine-epsilon)
     (machine->accepting m1)]
    [else
     (define const
       (if (or (machine-accepting? m1)
               (machine-accepting? m2))
           machine-accepting
           machine))
     (const
      `(union ,m1 ,m2)
      (λ (input)
        (machine-union (m1 input) (m2 input))))]))

(define (machine-intersect m1 m2)
  (machine-complement
   (machine-union
    (machine-complement m1)
    (machine-complement m2))))

(define (machine-seq* m1 make-m2)
  (cond
    [(eq? m1 machine-epsilon)
     (make-m2)]
    [(eq? m1 machine-null)
     machine-null]
    [else
     (define next
       (machine
        `(seq* ,m1 ,make-m2)
        (λ (input)
          (machine-seq* (m1 input) make-m2))))
     (if (machine-accepting? m1)
         (machine-union next (make-m2))
         next)]))

(define (machine-seq m1 m2)
  (machine-seq* m1 (λ () m2)))

(define (machine-star m1)
  (cond
    [(eq? m1 machine-epsilon)
     machine-sigma*]
    [(eq? m1 machine-null)
     machine-null]
    [else
     (machine->accepting
      (machine-seq* 
       ; Since seq* will force the RHS if m1 is accepting, this could go into
       ; an infinite loop. However, by removing the accepting-ness, we don't change
       ; the overall behavior because we ultimately make it initially accepting.
       (machine->non-accepting m1)
       (λ () (machine-star m1))))]))

(define (machine-delay make-m)
  (define m
    (machine
     `(delay ,make-m)
     (λ (input)
       ; XXX We don't change its accepting-ness
       (define nm (make-m))
       (set-machine-guts! m (machine-guts nm))
       (set-machine-next! m (machine-next nm))
       (nm input))))
  m)

(define (machine-accepts? m evts)
  (if (empty? evts)
      (machine-accepting? m)
      (machine-accepts? (m (first evts)) (rest evts))))
(define (machine-accepts?/prefix-closed m evts)
  (if (empty? evts)
      (machine-accepting? m)
      (let ([n (m (first evts))])
        (and (machine-accepting? n)
             (machine-accepts? n (rest evts))))))

(define machine-null
  (machine 'null (λ (input) machine-null)))
(define machine-epsilon
  (machine-accepting 'epsilon (λ (input) machine-null)))
(define machine-sigma*
  (machine-accepting 'sigma* (λ (input) machine-sigma*)))

(require racket/match)
(define (machine-explain m)
  (match (machine-guts m)
    [`(complement ,i)
     `(complement ,(machine-explain i))]
    [`(seq* ,a ,b)
     ; If a is epsilon, then we shouldn't show this, but we would've
     ; just returned b anyways.
     (machine-explain a)]
    [`(union ,a ,b)
     `(union ,(machine-explain a)
             ,(machine-explain b))]
    [`(delay ,i)
     ; If we have run it before, we'll never get this.
     `delay]
    [`null
     `null]
    [`epsilon
     `epsilon]
    [`sigma*
     `sigma*]
    [any
     any]))

(define-syntax-rule (provide/contract* [id ctc] ...)
  (provide id ...))

(provide machine?
         machine-accepting?
         machine
         machine-accepting)
(provide/contract*
 [machine-explain (machine? . -> . any/c)]
 [machine-accepts? (machine? (listof any/c) . -> . boolean?)]
 [machine-accepts?/prefix-closed (machine? (listof any/c) . -> . boolean?)]
 #;[struct machine ([next (any/c . -> . machine?)])]
 #;[struct (machine-accepting machine) ([next (any/c . -> . machine?)])]
 [machine-null machine?]
 [machine-epsilon machine?]
 [machine-sigma* machine?]
 [machine-complement (machine? . -> . machine?)]
 [machine-union (machine? machine? . -> . machine?)]
 [machine-intersect (machine? machine? . -> . machine?)]
 [machine-delay ((-> machine?) . -> . machine?)]
 [machine-seq* (machine? (-> machine?) . -> . machine?)]
 [machine-seq (machine? machine? . -> . machine?)]
 [machine-star (machine? . -> . machine?)])
