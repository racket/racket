#lang scheme/base

(require unstable/match scheme/match
         racket/dict syntax/id-table unstable/syntax
         (for-template scheme/base scheme/flonum scheme/fixnum scheme/unsafe/ops)
         "../utils/utils.rkt"
         (types abbrev type-table utils subtype)
         (rep type-rep))

(provide log-optimization *log-optimizations?* *log-optimizatons-to-log-file?* *optimization-log-file*
         subtypeof? isoftype?
         mk-unsafe-tbl
         n-ary->binary
         unboxed-gensym reset-unboxed-gensym
         optimize)


(define *log-optimizations?* #f)
(define *log-optimizatons-to-log-file?* #f)
(define *optimization-log-file* "opt-log")
(define (log-optimization kind stx)
  (if *log-optimizations?*
      (printf "~a line ~a col ~a - ~a - ~a\n"
              (syntax-source stx) (syntax-line stx) (syntax-column stx)
              (syntax->datum stx)
              kind)
      #t))

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
;; necessary to have predictable symbols to add in the hand-optimized versions
;; of the optimizer tests (which check for equality of expanded code)
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
