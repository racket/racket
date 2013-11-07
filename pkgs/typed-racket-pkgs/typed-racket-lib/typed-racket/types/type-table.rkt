#lang racket/base

;; TODO figure out why these imports are needed even though they don't seem to be.
(require racket/match
         "../utils/utils.rkt"
         (contract-req)
         (types utils union)
         (utils tc-utils))

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

;; keeps track of expressions that always evaluate to true or always evaluate
;; to false, so that the optimizer can eliminate dead code
;; 3 possible values: 'tautology 'contradiction 'neither
(define tautology-contradiction-table (make-hasheq))

(define-values (add-tautology add-contradiction add-neither)
  (let ()
    (define ((mk t?) e)
      (when (optimize?)
        (hash-set! tautology-contradiction-table e t?)))
    (values (mk 'tautology) (mk 'contradiction) (mk 'neither))))
(define-values (tautology? contradiction? neither?)
  (let ()
    (define ((mk t?) e)
      (eq? t? (hash-ref tautology-contradiction-table e 'not-there)))
    (values (mk 'tautology) (mk 'contradiction) (mk 'neither))))

;; keeps track of lambda branches that never get evaluated, so that the
;; optimizer can eliminate dead code. The key is the formals syntax object.
;; 1 possible value: #t
(define lambda-dead-table (make-hasheq))

(define (add-dead-lambda-branch formals)
  (when (optimize?)
    (hash-set! lambda-dead-table formals #t)))
(define (dead-lambda-branch? formals)
  (hash-ref lambda-dead-table formals #f))


(provide/cond-contract
 [add-typeof-expr (syntax? tc-results/c . -> . any/c)]
 [type-of (syntax? . -> . tc-results/c)]
 [reset-type-table (-> any/c)]
 [add-tautology (syntax? . -> . any)]
 [add-contradiction (syntax? . -> . any)]
 [add-neither (syntax? . -> . any)]
 [tautology? (syntax? . -> . boolean?)]
 [contradiction? (syntax? . -> . boolean?)]
 [neither? (syntax? . -> . boolean?)]
 [add-dead-lambda-branch (syntax? . -> . any)]
 [dead-lambda-branch? (syntax? . -> . boolean?)])
