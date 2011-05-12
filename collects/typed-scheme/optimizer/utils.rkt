#lang scheme/base

(require unstable/match racket/match
         racket/dict syntax/id-table racket/syntax unstable/syntax
         "../utils/utils.rkt"
         (for-template scheme/base)
         (types type-table utils subtype numeric-tower)
         (rep type-rep))

(provide *log-file* *log-to-log-file?* log-optimization *log-optimizations?*
         log-close-call *log-close-calls?*
         *show-optimized-code*
         subtypeof? isoftype?
         in-integer-layer? in-rational-layer? in-float-layer? in-real-layer?
         mk-unsafe-tbl
         n-ary->binary
         unboxed-gensym reset-unboxed-gensym
         optimize)

(define (in-command-line? opt)
  (member opt (vector->list (current-command-line-arguments))))

(define *log-file* "opt-log")
(define *log-to-log-file?* #f) ; otherwise, goes to stdout

(define (do-logging msg stx)
  (printf "~a line ~a col ~a - ~a - ~a\n"
          (syntax-source-file-name stx)
          (syntax-line stx) (syntax-column stx)
          (syntax->datum stx)
          msg))

(define *log-optimizations?* (in-command-line? "--log-optimizations"))
(define (log-optimization kind stx)
  (when *log-optimizations?*
    (do-logging kind stx)))

;; Keep track of optimizations that "almost" happened, with the intention
;; of reporting them to the user.
;; This is meant to help users understand what hurts the performance of
;; their programs.
(define *log-close-calls?* (in-command-line? "--log-close-calls"))
(define (log-close-call kind stx [irritant #f])
  (when *log-close-calls?*
    (do-logging (if irritant
                    (format "~a - caused by: ~a - ~a - ~a - ~a"
                            kind
                            (syntax-source-file-name irritant)
                            (syntax-line irritant) (syntax-column irritant)
                            (syntax->datum irritant))
                    kind)
                stx)))

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

;; layer predicates
;; useful in some cases where subtyping won't do
(define (in-integer-layer? t)
  (subtypeof? t -Int))
(define (in-rational-layer? t)
  (and (subtypeof? t -Rat)
       (not (subtypeof? t -Int))))
(define (in-float-layer? t)
  (subtypeof? t -Flonum))
(define (in-real-layer? t)
  (and (subtypeof? t -Real)
       (not (subtypeof? t -Rat))
       (not (subtypeof? t -Flonum))))

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
