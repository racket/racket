#lang racket/base

(require (for-syntax racket/base)
         racket/performance-hint)

(provide mmap
         mfor-each
         mlist
         mlist?
         mlength
         mappend
         mappend!
         mreverse
         mreverse!
         mlist-tail
         mlist-ref
         mmemq
         mmemv
         mmember
         massq
         massv
         massoc
         mlist->list
         list->mlist
         mlistof)

(begin-encourage-inline
 (define mmap
   (case-lambda
    [(f l) (let loop ([l l])
             (cond
              [(null? l) null]
              [else (mcons (f (mcar l)) (loop (mcdr l)))]))]
    [(f l1 l2) (let loop ([l1 l1][l2 l2])
                 (cond
                  [(null? l1) null]
                  [else (mcons (f (mcar l1) (mcar l2))
                               (loop (mcdr l1) (mcdr l2)))]))]
    [(f l . ls) (let loop ([l l][ls ls])
                  (cond
                   [(null? l) null]
                   [else (mcons (apply f (mcar l) (map mcar ls))
                                (loop (mcdr l) (map mcdr ls)))]))]))

 (define mfor-each
   (case-lambda
    [(f l) (let loop ([l l])
             (cond
              [(null? l) (void)]
              [else (f (mcar l))
                    (loop (mcdr l))]))]
    [(f l1 l2) (let loop ([l1 l1][l2 l2])
                 (cond
                  [(null? l1) (void)]
                  [else (f (mcar l1) (mcar l2))
                        (loop (mcdr l1) (mcdr l2))]))]
    [(f l . ls) (let loop ([l l][ls ls])
                  (cond
                   [(null? l) (void)]
                   [else (apply f (mcar l) (map mcar ls))
                         (loop (mcdr l) (map mcdr ls))]))])))

(define (list->mlist l)
  (cond
   [(null? l) null]
   [else (mcons (car l) (list->mlist (cdr l)))]))

(define (mlist->list l)
  (cond
   [(null? l) null]
   [else (cons (mcar l) (mlist->list (mcdr l)))]))

(define-syntax mlist
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! id . _) (raise-syntax-error #f
                                          "cannot mutate imported variable"
                                          stx
                                          #'id)]
       [(_ a) #'(mcons a null)]
       [(_ a b) #'(mcons a (mcons b null))]
       [(_ a b c) #'(mcons a (mcons b (mcons c null)))]
       [(_ arg ...) #'(-mlist arg ...)]
       [_ #'-mlist]))))

(define -mlist
  (let ([mlist
         (case-lambda
          [() null]
          [(a) (mcons a null)]
          [(a b) (mcons a (mcons b null))]
          [(a b c) (mcons a (mcons b (mcons c null)))]
          [(a b c d) (mcons a (mcons b (mcons c (mcons d null))))]
          [l (list->mlist l)])])
    mlist))

(define (mlist? l)
  (cond
   [(null? l) #t]
   [(mpair? l)
    (let loop ([turtle l][hare (mcdr l)])
      (cond
       [(null? hare) #t]
       [(eq? hare turtle) #f]
       [(mpair? hare)
        (let ([hare (mcdr hare)])
          (cond
           [(null? hare) #t]
           [(eq? hare turtle) #f]
           [(mpair? hare)
            (loop (mcdr turtle) (mcdr hare))]
           [else #f]))]
       [else #f]))]
   [else #f]))
  
(define (mlength l)
  (let loop ([l l][len 0])
    (cond
     [(null? l) len]
     [else (loop (mcdr l) (add1 len))])))

(define mappend
  (case-lambda
   [() null]
   [(a) a]
   [(a b) (let loop ([a a])
            (if (null? a)
                b
                (mcons (mcar a) (loop (mcdr a)))))]
   [(a . l) (mappend a (apply mappend l))]))

;; mappend! : like append, but mutate each list to refer to the next.
;; modeled loosely on the v372 behavior

(define mappend!
  (case-lambda
    [() null]
    [(a) a]
    [(a b) (if (null? a)
               b
               (let loop ([atail a])
                 (cond [(null? (mcdr atail)) (set-mcdr! atail b) a]
                       [else (loop (mcdr atail))])))]
    [(a . l) (mappend! a (apply mappend! l))]))

(define (mreverse l)
  (let loop ([l l][a null])
    (cond
     [(null? l) a]
     [else (loop (mcdr l) (mcons (mcar l) a))])))

(define (mreverse! l)
  (let loop ([l l][prev null])
    (cond
     [(null? l) prev]
     [else (let ([next (mcdr l)])
             (set-mcdr! l prev)
             (loop next l))])))

(define (mlist-tail l n)
  (cond
   [(zero? n) l]
   [else (mlist-tail (mcdr l) (sub1 n))]))

(define (mlist-ref l n)
  (cond
   [(zero? n) (mcar l)]
   [else (mlist-ref (mcdr l) (sub1 n))]))

(define (do-member =? v l)
  (let loop ([l l])
    (cond
     [(null? l) #f]
     [(=? v (mcar l)) l]
     [else (loop (mcdr l))])))

(define (mmemq v l)
  (do-member eq? v l))

(define (mmemv v l)
  (do-member eqv? v l))

(define (mmember v l)
  (do-member equal? v l))


(define (do-assoc =? v l)
  (let loop ([l l])
    (cond
     [(null? l) #f]
     [(=? v (mcar (mcar l))) (mcar l)]
     [else (loop (mcdr l))])))

(define (massq v l)
  (do-assoc eq? v l))

(define (massv v l)
  (do-assoc eqv? v l))

(define (massoc v l)
  (do-assoc equal? v l))

(define ((mlistof p?) l)
  (let loop ([l l])
    (cond
     [(null? l) #t]
     [(not (mpair? l)) #f]
     [(p? (mcar l)) (loop (mcdr l))]
     [else #f])))
