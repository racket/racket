#lang racket/base

#|

Used to check an application site of a well-known
contract function to see if the shape is okay.

That is, each contract builds a valid-app-shapes struct
describing what application expression shapes are okay
and valid-argument-list? checks an application against
a valid-app-shape.

|#

(provide (struct-out valid-app-shapes)
         valid-argument-list?)

;; valid-arities : (or/c (listof nat) (improper-listof nat))
;;    -- if improper, then the last nat indicates that any number
;;       of args equal to or later than that last nat are okay
;; mandatory-kwds : (listof keyword?)
;; optional-kwds : (or/c (listof keyword?) 'any)
;;   'any indicates that any keyword is allowed
(struct valid-app-shapes (valid-arities mandatory-kwds optional-kwds) 
  #:prefab)

(define (valid-argument-list? app-stx the-valid-app-shape [log-problems? #t])
  (cond
    [the-valid-app-shape
     (define-values (kwds arg-count)
       (let loop ([stx (syntax-case app-stx ()
                         [(function . args) #'args])]
                  [kwds '()]
                  [arg-count 0])
         (syntax-case stx ()
           [(kwd kwd-arg . rest)
            (keyword? (syntax-e #'kwd))
            (loop #'rest (cons (syntax-e #'kwd) kwds) arg-count)]
           [(arg . rest)
            (loop #'rest kwds (+ arg-count 1))]
           [()
            (values kwds arg-count)])))
     
     (define good-arg-count?
       (let loop ([allowed-counts (valid-app-shapes-valid-arities the-valid-app-shape)])
         (cond
           [(null? allowed-counts) #f]
           [(number? allowed-counts) (arg-count . >= . allowed-counts)]
           [else (or (= arg-count (car allowed-counts))
                     (loop (cdr allowed-counts)))])))
     
     (define ans?
       (and good-arg-count?
            (for/and ([kwd (in-list (valid-app-shapes-mandatory-kwds the-valid-app-shape))])
              (member kwd kwds))
            (for/and ([kwd (in-list kwds)])
              (or (member kwd (valid-app-shapes-mandatory-kwds the-valid-app-shape))
                  (member kwd (valid-app-shapes-optional-kwds the-valid-app-shape))))
            #t))
     (when log-problems?
       (unless ans?
         (log-problem app-stx)))
     ans?]
    [else #t]))

(define-logger optimizer)
(define (log-problem stx)
  (log-optimizer-warning 
   "warning in ~a:~a:~a: contract system detects procedure incorrectly applied"
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)))
