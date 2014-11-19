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
 [type-table->tooltips
  (-> (listof (vector/c any/c integer? integer? (or/c string? (-> string?)))))]
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

(define type-table (make-hasheq))

;; A struct that helps track tooltip types for a given location
;;   seen    - (Listof Syntax)
;;   results - a TC-Result
(struct tooltip (seen results) #:transparent)

;; tooltip-table : (Hash (List Int Int) tooltip)
;; This table keeps track of the same types as the type table
;; but indexed on source location for tooltips and only retaining the
;; last type for a given location.
(define tooltip-table (make-hash))

(define (reset-type-table) (set! type-table (make-hasheq)))

(define (add-typeof-expr e t)
  (when (and (syntax-position e) (syntax-span e))
    ;; Only keep the latest type for a given location, which means that
    ;; since typechecking proceeds inside-out we will only record the most
    ;; relevant type for the expansion of macros (which often have misleading
    ;; syntax locations on the subexpressions).
    (hash-update!
     tooltip-table
     (list (syntax-position e) (syntax-span e))
     (λ (old)
       (match-define (tooltip seen results) old)
       (if (member e seen)
           ;; the car should be the latest stx for the location
           (if (equal? e (car seen))
               ;; combine types seen at the latest
               (tooltip seen ((combine t) results))
               old)
           (tooltip (cons e seen) t)))
     (tooltip (list e) t)))
  (when (optimize?)
    (hash-update! type-table e (combine t) t)))

;; when typechecking a case-> type, types get added for
;; the same subexpression multiple times, combine them
(define ((combine new) old)
  (match* (old new)
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
    [(_ _) new])) ; irrelevant to the optimizer, just clobber

(define (type-of e)
  (hash-ref type-table e
            (lambda () (int-err (format "no type for ~a at: ~a line ~a col ~a"
                                        (syntax->datum e)
                                        (syntax-source e)
                                        (syntax-line e)
                                        (syntax-column e))))))

;; This macro is used to create a thunk that closes over the type
;; names that should be used to print the type. This is needed to ensure that
;; DrRacket can show the tooltips with the right abbreviations.
(define-syntax-rule (printer-thunk tbl e)
  (λ () (parameterize ([current-type-names tbl]) e)))

;; Convert the contents of the type table to a format that check-syntax
;; can understand in order to draw type tooltips
(define (type-table->tooltips)
  (define type-names (current-type-names))
  (for/fold ([tooltips '()])
            ([(pos+span stx+results) (in-hash tooltip-table)]
             #:unless (error-at-stx-loc? (car (tooltip-seen stx+results))))
    (match-define (list pos span) pos+span)
    (match-define (tooltip (cons stx _) results) stx+results)
    ;; `printed-type-thunks` is #f if we should skip the type because it's
    ;; something not worth printing like Bottom or Error.
    (define printed-type-thunks
      (match results
        [(tc-result1: type)
         (and (not (or (Error? type) (Bottom? type)))
              ;; cleanup-type is essential here so that this doesn't slow
              ;; down compilation excessively (e.g., serializing the 4k type
              ;; of the + function)
              (printer-thunk type-names
                (pretty-format-type (cleanup-type type))))]
        [(or (tc-results: types)
             (tc-results: types _ _ _ _)) ; FIXME, account for dty/dbound
         (printer-thunk type-names
           (apply string-append
                  (for/list ([(type index) (in-indexed (in-list types))])
                    (format "Value ~a:~n  ~a~n"
                            (add1 index)
                            (pretty-format-type (cleanup-type type)
                                                #:indent 2)))))]
        [(tc-any-results: _) "AnyValues"]))
    (cond [(not printed-type-thunks) tooltips]
          [else
           (append (make-tooltip-vector stx printed-type-thunks pos span)
                   tooltips)])))

;; make-tooltip-vector : Syntax Thunk Int Int -> (Listof Vector)
;; Compute the tooltip info to put in syntax properties
(define (make-tooltip-vector stx type-thunk position span)
  ;; Try to use the original syntax to guide the shape of the tooltip
  (define located (locate-stx stx))
  (cond ;; Put the tooltip only on the parens for compound expressions
        ;; but put them on the whole expression for literals. This avoids
        ;; overlapping tooltips.
        [(or (not (pair? (syntax-e located)))
             ;; special-case quote because there's no worry of overlap
             ;; in a (quote ...) and because literals expand out to a
             ;; use of quote.
             (let ([fst (car (syntax-e located))])
               (and (identifier? fst)
                    (free-identifier=? fst #'quote))))
         (list (vector stx
                       (sub1 (syntax-position stx))
                       (+ (sub1 (syntax-position stx)) (syntax-span stx))
                       type-thunk))]
        [else
         (list (vector stx
                       (sub1 (syntax-position stx))
                       (syntax-position stx)
                       type-thunk)
               (vector stx
                       (sub1 (+ (sub1 (syntax-position stx))
                                (syntax-span stx)))
                       (+ (sub1 (syntax-position stx))
                          (syntax-span stx))
                       type-thunk))]))

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
