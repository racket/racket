#lang racket/base
(require (for-template racket/base))
#|

Used to check an application site of a well-known
contract function to see if the shape is okay.

That is, each contract builds a valid-app-shapes struct
describing what application expression shapes are okay
and valid-argument-list? checks an application against
a valid-app-shape.

|#

(provide (struct-out valid-app-shapes)
         valid-argument-list?
         generate-medium-speed-wrapper)

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

;; called in the case that the identifier isn't used directly in an
;; application. Try to generate a case-lambda that can still avoid
;; the chaperone creation
(define (generate-medium-speed-wrapper the-valid-app-shape
                                       chaperone-expr
                                       extra-arg-function
                                       neg-party-id
                                       add-medium-speed-kwd-wrapper-id
                                       expected-name)
  (cond
    [(and the-valid-app-shape
          (null? (valid-app-shapes-mandatory-kwds the-valid-app-shape))
          (null? (valid-app-shapes-optional-kwds the-valid-app-shape)))
     (define chaperone-expr-id (car (generate-temporaries '(medium-speed-wrapper))))
     (define (mk-n-ids n) (generate-temporaries (build-list n (λ (x) 'medium-speed-wrapper-arg))))
     (define case-lambda-clauses
       (let loop ([valid-arities (valid-app-shapes-valid-arities the-valid-app-shape)])
         (cond
           [(null? valid-arities)
            (list #`[args (apply #,chaperone-expr-id args)])]
           [(number? valid-arities)
            (with-syntax ([(x ...) (mk-n-ids valid-arities)]
                          [(rest-arg) (generate-temporaries '(medium-speed-wrapper-dot-arg))])
              (list
               #`[(x ... . rest-arg) (apply #,extra-arg-function #,neg-party-id x ... rest-arg)]))]
           [else
            (with-syntax ([(x ...) (mk-n-ids (car valid-arities))])
              (cons #`[(x ...) (#,extra-arg-function #,neg-party-id x ...)]
                    (loop (cdr valid-arities))))])))
     #`(let ([#,chaperone-expr-id #,chaperone-expr])
         (#,add-medium-speed-kwd-wrapper-id
          #,chaperone-expr-id
          (let ([#,expected-name (case-lambda #,@case-lambda-clauses)])
            #,expected-name)))]
    [else chaperone-expr]))

(define-logger optimizer)
(define (log-problem stx)
  (log-optimizer-warning 
   "warning in ~a:~a:~a: contract system detects procedure incorrectly applied"
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)))
