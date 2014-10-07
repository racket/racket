#lang racket/base

;; This module provides functions that manipulate tables mapping expressions
;; to type information or other properties. This allows the optimizer or
;; other downstream analyses to use information from the type-checker.

;; TODO figure out why these imports are needed even though they don't seem to be.
(require racket/match
         syntax/parse
         "../utils/utils.rkt"
         (contract-req)
         (rep type-rep)
         (types utils union printer)
         (typecheck tc-app-helper)
         (utils tc-utils)
         (for-template racket/base))

(provide/cond-contract
 [add-typeof-expr (syntax? tc-results/c . -> . any/c)]
 [type-of (syntax? . -> . tc-results/c)]
 [reset-type-table (-> any/c)]
 [type-table->tooltips (-> (listof (vector/c any/c integer? integer? string?)))]
 [test-position-add-true (syntax? . -> . any)]
 [test-position-add-false (syntax? . -> . any)]
 [test-position-takes-true-branch (syntax? . -> . boolean?)]
 [test-position-takes-false-branch (syntax? . -> . boolean?)]
 [add-dead-lambda-branch (syntax? . -> . any)]
 [dead-lambda-branch? (syntax? . -> . boolean?)]
 [;; Register that the given expression should be ignored
  register-ignored! (syntax? . -> . any)]
 [;; Look up whether a given expression is ignored
  is-ignored? (syntax? . -> . boolean?)])

(provide ;; Syntax class for is-ignored?
         ignore-table^)

(define table (make-hasheq))

(define (reset-type-table) (set! table (make-hasheq)))

(define (add-typeof-expr e t)
  (when (optimize?)
    (hash-update! table e
                  ;; when typechecking a case-> type, types get added for
                  ;; the same subexpression multiple times, combine them
                  (lambda (old)
                    (match* (old t)
                      [((tc-result1: old-t) (tc-result1: t-t))
                       (ret (Un old-t t-t))]
                      [((tc-results: old-ts) (tc-results: t-ts))
                       ;; filters don't matter at this point, since only
                       ;; the optimizer reads this table
                       (unless (= (length old-ts) (length t-ts))
                         (int-err
                          "type table: number of values don't agree ~a ~a"
                          old-ts t-ts))
                       (ret (map Un old-ts t-ts))]
                      [(_ _) t])) ; irrelevant to the optimizer, just clobber
                  t)))

(define (type-of e)
  (hash-ref table e
            (lambda () (int-err (format "no type for ~a at: ~a line ~a col ~a"
                                        (syntax->datum e)
                                        (syntax-source e)
                                        (syntax-line e)
                                        (syntax-column e))))))

;; Convert the contents of the type table to a format that check-syntax
;; can understand in order to draw type tooltips
(define (type-table->tooltips)
  (for/fold ([tooltips '()])
            ([(stx results) (in-hash table)]
             #:when (and (syntax-position stx)
                         (syntax-span stx))
             #:unless (error-at-stx-loc? stx))
    ;; `printed-types` is #f if we should skip the type because it's
    ;; something not worth printing like Bottom or Error.
    (define printed-types
      (match results
        [(tc-result1: type)
         (and (not (or (Error? type) (Bottom? type)))
              ;; cleanup-type is essential here so that this doesn't slow
              ;; down compilation excessively (e.g., serializing the 4k type
              ;; of the + function)
              (pretty-format-type (cleanup-type type)))]
        [(or (tc-results: types)
             (tc-results: types _ _ _ _)) ; FIXME, account for dty/dbound
         (apply string-append
                (for/list ([(type index) (in-indexed (in-list types))])
                  (format "Value ~a:~n  ~a~n"
                          (add1 index)
                          (pretty-format-type (cleanup-type type)
                                              #:indent 2))))]
        [(tc-any-results: _) "AnyValues"]))
    (cond [(not printed-types) tooltips]
          ;; Put the tooltip only on the parens for compound expressions
          ;; but put them on the whole expression for literals. This avoids
          ;; overlapping tooltips.
          [(or (not (pair? (syntax-e stx)))
               ;; special-case quote because there's no worry of overlap
               ;; in a (quote ...) and because literals expand out to a
               ;; use of quote.
               (let ([fst (car (syntax-e stx))])
                 (and (identifier? fst)
                      (free-identifier=? fst #'quote))))
           (cons (vector stx
                         (sub1 (syntax-position stx))
                         (+ (sub1 (syntax-position stx)) (syntax-span stx))
                         printed-types)
                 tooltips)]
          [else
           (list* (vector stx
                          (sub1 (syntax-position stx))
                          (syntax-position stx)
                          printed-types)
                  (vector stx
                          (sub1 (+ (sub1 (syntax-position stx))
                                   (syntax-span stx)))
                          (+ (sub1 (syntax-position stx))
                             (syntax-span stx))
                          printed-types)
                   tooltips)])))

;; For expressions in test position keep track of if it evaluates to true/false
(define test-position-table/true (make-hasheq))
(define test-position-table/false (make-hasheq))

(define (test-position-add-true expr)
  (hash-set! test-position-table/true expr #t))
(define (test-position-add-false expr)
  (hash-set! test-position-table/false expr #t))

(define (test-position-takes-true-branch expr)
  (hash-ref test-position-table/true expr #f))
(define (test-position-takes-false-branch expr)
  (hash-ref test-position-table/false expr #f))

;; keeps track of lambda branches that never get evaluated, so that the
;; optimizer can eliminate dead code. The key is the formals syntax object.
;; 1 possible value: #t
(define lambda-dead-table (make-hasheq))

(define (add-dead-lambda-branch formals)
  (when (optimize?)
    (hash-set! lambda-dead-table formals #t)))
(define (dead-lambda-branch? formals)
  (hash-ref lambda-dead-table formals #f))

;; The following provides functions for manipulating the ignore-table, which
;; stores expressions that should be ignored for type-checking, optimization,
;; and other type-related analyses.
;;
;; Since the type-checker doesn't add annotations to its input syntax, if
;; the type-checker discovers that something should be ignored by future
;; passes, it needs to use this side-channel.
(define ignore-table (make-hasheq))

(define (register-ignored! stx)
  (hash-set! ignore-table stx #t))

(define (is-ignored? stx)
  (hash-ref ignore-table stx #f))

(define-syntax-class ignore-table^
  (pattern _ #:when (is-ignored? this-syntax)))
