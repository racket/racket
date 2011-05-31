#lang racket/base

(require unstable/match racket/match racket/set
         racket/dict syntax/id-table racket/syntax unstable/syntax
         "../utils/utils.rkt"
         (for-template racket/base)
         (types type-table utils subtype)
         (rep type-rep))

(provide log-optimization log-missed-optimization
         print-log clear-log
         *show-optimized-code*
         subtypeof? isoftype?
         mk-unsafe-tbl
         n-ary->binary
         unboxed-gensym reset-unboxed-gensym
         optimize)

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(struct log-entry (msg line col) #:transparent)

;; we keep track of log entries, to avoid repetitions that would be
;; caused by traversing the same syntax multiple times (which is not
;; a problem per se)
(define log-so-far (set))
(define (do-logging msg stx)
  (let* ([new-message (format "~a ~a ~a -- ~a"
                              (syntax-source-file-name stx)
                              (line+col->string stx)
                              (syntax->datum stx)
                              msg)]
         [new-entry (log-entry new-message
                               (syntax-line stx)
                               (syntax-column stx))])
    (unless (set-member? log-so-far new-entry)
      (set! log-so-far (set-add log-so-far new-entry)))))
;; once the optimizer is done, we sort the log according to source
;; location, then print it
(define (print-log)
  (for-each (lambda (x) (log-warning (log-entry-msg x)))
            (sort (set->list log-so-far)
                  (lambda (x y)
                    (match* (x y)
                      [((log-entry msg-x line-x col-x)
                        (log-entry msg-y line-y col-y))
                       (cond [(not (or line-x line-y))
                              ;; neither have location, sort by message
                              (string<? msg-x msg-y)]
                             [(not line-y) #f]
                             [(not line-x) #t]
                             [else
                              ;; both have location
                              (let* ([loc-x (+ (* 1000 line-x) col-x)]
                                     ;; 1000 is a conservative bound
                                     [loc-y (+ (* 1000 line-y) col-y)])
                                (cond [(= loc-x loc-y)
                                       ;; same location, sort by message
                                       (string<? msg-x msg-y)]
                                      ;; sort by source location
                                      [else (< loc-x loc-y)]))])])))))
(define (clear-log)
  (set! log-so-far (set)))

(define (log-optimization kind stx) (do-logging kind stx))

;; Keep track of optimizations that "almost" happened, with the intention
;; of reporting them to the user.
;; This is meant to help users understand what hurts the performance of
;; their programs.
(define (log-missed-optimization kind stx [irritant #f])
  (do-logging (if irritant
                  (format "~a -- caused by: ~a ~a"
                          kind
                          (line+col->string irritant)
                          (syntax->datum irritant))
                  kind)
              stx))

;; if set to #t, the optimizer will dump its result to stdout before compilation
(define *show-optimized-code* #f)

;; is the syntax object s's type a subtype of t?
(define (subtypeof? s t)
  (match (type-of s)
    [(tc-result1: (== t (lambda (x y) (subtype y x)))) #t] [_ #f]))
;; similar, but with type equality
(define (isoftype? s t)
  (match (type-of s)
         [(tc-result1: (== t type-equal?)) #t] [_ #f]))

;; generates a table matching safe to unsafe promitives
(define (mk-unsafe-tbl generic safe-pattern unsafe-pattern)
  (for/fold ([h (make-immutable-free-id-table)]) ([g generic])
    (let ([f (format-id g safe-pattern g)] [u (format-id g unsafe-pattern g)])
      (dict-set (dict-set h g u) f u))))

;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
(define (n-ary->binary op arg1 arg2 rest)
  (for/fold ([o arg1])
      ([e (syntax->list #`(#,arg2 #,@rest))])
    #`(#,op #,o #,e)))

;; to generate temporary symbols in a predictable manner
;; these identifiers are unique within a sequence of unboxed operations
(define *unboxed-gensym-counter* 0)
(define (unboxed-gensym [name 'unboxed-gensym-])
  (set! *unboxed-gensym-counter* (add1 *unboxed-gensym-counter*))
  (format-unique-id #'here "~a~a" name *unboxed-gensym-counter*))
(define (reset-unboxed-gensym)
  (set! *unboxed-gensym-counter* 0))

;; to avoid mutually recursive syntax classes
;; will be set to the actual optimization function at the entry point
;; of the optimizer
(define optimize (make-parameter #f))
