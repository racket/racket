#lang racket/base

(require syntax/id-table racket/dict racket/match mzlib/pconvert
         "../utils/utils.rkt" racket/syntax
         "../utils/tc-utils.rkt"
         (contract-req)
         (rep type-rep object-rep)
         (types utils union)
         (utils tc-utils)
         (env init-envs)
         (for-template 
          racket/base          
          (rep type-rep object-rep)
          (types utils union)
          (utils tc-utils)
          (env init-envs)))


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


(define struct-fn-table (make-free-id-table))

(define struct-constructor-table (make-free-id-table))

(define (add-struct-constructor! id) (dict-set! struct-constructor-table id #t))
(define (struct-constructor? id) (dict-ref struct-constructor-table id #f))

(define (add-struct-fn! id pe mut?) (dict-set! struct-fn-table id (list pe mut?)))

(define-values (struct-accessor? struct-mutator?)
  (let ()
    (define ((mk mut?) id)
      (cond [(dict-ref struct-fn-table id #f)
             => (match-lambda [(list pe m) (and (eq? m mut?) pe)] [_ #f])]
            [else #f]))
    (values (mk #f) (mk #t))))

(define (struct-fn-idx id)
  (match (dict-ref struct-fn-table id #f)
    [(list (StructPE: _ idx) _) idx]
    [_ (int-err (format "no struct fn table entry for ~a" (syntax->datum id)))]))

(define (make-struct-table-code)
  (parameterize ([current-print-convert-hook converter]
                 [show-sharing #f])
    (define/with-syntax (adds ...)      
      (for/list ([(k v) (in-dict struct-fn-table)]
                 #:when (bound-in-this-module k))
        (match v
          [(list pe mut?)
           #`(add-struct-fn! (quote-syntax #,k) #,(print-convert pe) #,mut?)])))
    #'(begin adds ...)))


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

(provide/cond-contract
 [add-typeof-expr (syntax? tc-results? . -> . any)]
 [type-of (syntax? . -> . tc-results?)]
 [reset-type-table (-> any)]
 [add-struct-fn! (identifier? StructPE? boolean? . -> . any)]
 [add-struct-constructor! (identifier? . -> . any)]
 [struct-constructor? (identifier? . -> . boolean?)]
 [struct-accessor? (identifier? . -> . (or/c #f StructPE?))]
 [struct-mutator? (identifier? . -> . (or/c #f StructPE?))]
 [struct-fn-idx (identifier? . -> . exact-integer?)]
 [make-struct-table-code (-> syntax?)]
 [add-tautology (syntax? . -> . any)]
 [add-contradiction (syntax? . -> . any)]
 [add-neither (syntax? . -> . any)]
 [tautology? (syntax? . -> . boolean?)]
 [contradiction? (syntax? . -> . boolean?)]
 [neither? (syntax? . -> . boolean?)])
