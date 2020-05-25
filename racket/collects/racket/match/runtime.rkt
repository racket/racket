#lang racket/base

(provide match-equality-test
         exn:misc:match?
         match:error
         matchable?
         match-prompt-tag
         mlist? mlist->list
         syntax-srclocs)

(define match-prompt-tag (make-continuation-prompt-tag 'match)) 

(define match-equality-test (make-parameter equal? #f 'match-equality-test))

(define-struct (exn:misc:match exn:fail) (value srclocs)
  #:property prop:exn:srclocs (lambda (ex) (exn:misc:match-srclocs ex))
  #:transparent)


(define (match:error val srclocs form-name)
  (raise (make-exn:misc:match
          (format "~a: no matching clause for ~e"
                  form-name val)
          (current-continuation-marks)
          val
          srclocs)))

;; can we pass this value to regexp-match?
(define (matchable? e)
  (or (string? e) (bytes? e)))

;; duplicated because we can't depend on `compatibility` here
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

(define (mlist->list l)
  (cond
   [(null? l) null]
   [else (cons (mcar l) (mlist->list (mcdr l)))]))

(define (syntax-srclocs stx)
  (list (srcloc (syntax-source stx)
                (syntax-line stx)
                (syntax-column stx)
                (syntax-position stx)
                (syntax-span stx))))
