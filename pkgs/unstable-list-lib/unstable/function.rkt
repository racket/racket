#lang racket/base
(require racket/match
         (for-syntax racket/base racket/list))
(provide conjoin
         disjoin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Higher-Order Boolean Operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ryanc: adjusted limit of inner cases from 8 to 2
;; All uses so far seem to be predicates, so more cases seem
;; unnecessary. Also, all uses so far are first-order, so
;; outer case-lambda* might be better replaced with macro.

(define conjoin
  (case-lambda*
   [(f ... 8)
    (make-intermediate-procedure
     'conjoined
      [(x (... ...) 2) (and (f x (... ...)) ...)]
      [xs (and (apply f xs) ...)]
      #:keyword
      [(keys vals . args)
       (and (keyword-apply f keys vals args) ...)])]
   [fs
    (make-intermediate-procedure
     'conjoined
     [(x ... 2) (andmap (lambda (f) (f x ...)) fs)]
     [xs (andmap (lambda (f) (apply f xs)) fs)]
     #:keyword
     [(keys vals . args)
      (andmap (lambda (f) (keyword-apply f keys vals args)) fs)])]))

(define disjoin
  (case-lambda*
   [(f ... 8)
    (make-intermediate-procedure
     'disjoined
      [(x (... ...) 2) (or (f x (... ...)) ...)]
      [xs (or (apply f xs) ...)]
      #:keyword
      [(keys vals . args)
       (or (keyword-apply f keys vals args) ...)])]
   [fs
    (make-intermediate-procedure
     'disjoined
     [(x ... 2) (ormap (lambda (f) (f x ...)) fs)]
     [xs (ormap (lambda (f) (apply f xs)) fs)]
     #:keyword
     [(keys vals . args)
      (ormap (lambda (f) (keyword-apply f keys vals args)) fs)])]))

(define-syntax (make-intermediate-procedure stx)
  (syntax-case stx [quote]
    [(_ (quote name) positional-clause ... #:keyword keyword-clause)
     (syntax/loc stx
       (make-keyword-procedure
        (let* ([name (case-lambda keyword-clause)]) name)
        (let* ([name (case-lambda* positional-clause ...)]) name)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Automatic case-lambda repetition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (split-syntax-at orig stx id)
  (let loop ([found #f]
             [seen null]
             [stx stx])
    (syntax-case stx []
      [(head . tail)
       (and (identifier? #'head)
            (free-identifier=? #'head id))
       (if found
           (raise-syntax-error
            #f
            (format "duplicate occurrence of ~a" (syntax-e id))
            orig
            #'head)
           (loop (list (reverse seen) #'head #'tail)
                 (cons #'head seen)
                 #'tail))]
      [(head . tail) (loop found (cons #'head seen) #'tail)]
      [_ found])))

(define-for-syntax (expand-ellipsis-clause stx pattern expr)
  (cond
   [(split-syntax-at stx pattern #'(... ...))
    =>
    (lambda (found)
      (syntax-case found [...]
        [([pre ... repeat] (... ...) [count post ... . tail])
         (and (identifier? #'repeat)
              (exact-nonnegative-integer? (syntax-e #'count)))
         (build-list
          (add1 (syntax-e #'count))
          (lambda (i)
            (with-syntax ([(var ...)
                           (generate-temporaries
                            (build-list i (lambda (j) #'repeat)))]
                          [body expr])
              (list
               (syntax/loc pattern (pre ... var ... post ... . tail))
               (syntax/loc expr
                 (let-syntax ([the-body
                               (lambda _
                                 (with-syntax ([(repeat (... ...)) #'(var ...)])
                                   #'body))])
                   the-body))))))]
        [(pre mid post)
         (raise-syntax-error
          #f
          "expected ellipsis between identifier and natural number literal"
          stx
          #'mid)]))]
   [else (list (list pattern expr))]))

(define-syntax (case-lambda* stx)
  (syntax-case stx []
    [(_ [pattern body] ...)
     (with-syntax ([([pattern body] ...)
                    (append-map
                     (lambda (p e) (expand-ellipsis-clause stx p e))
                     (syntax->list #'(pattern ...))
                     (syntax->list #'(body ...)))])
       (syntax/loc stx
         (case-lambda [pattern body] ...)))]))
