#lang racket/base

(require unstable/debug racket/contract "../utils/utils.rkt" syntax/id-table racket/dict racket/match
         (rep type-rep object-rep) (only-in (types abbrev utils) tc-results?) (utils tc-utils)
         (env init-envs) mzlib/pconvert)

(define table (make-hasheq))

(define (reset-type-table) (set! table (make-hasheq)))

(define (add-typeof-expr e t) 
  (when (optimize?)
    (hash-set! table e t)))

(define (type-of e)
  (hash-ref table e
            (lambda () (int-err (format "no type for ~a at: ~a line ~a col ~a"
                                        (syntax->datum e)
                                        (syntax-source e)
                                        (syntax-line e)
                                        (syntax-column e))))))


(define struct-fn-table (make-free-id-table))

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
    #`(begin #,@(for/list ([(k v) (in-dict struct-fn-table)]
                           #:when (bound-in-this-module k))
                  (match v
                    [(list pe mut?)
                     #`(add-struct-fn! (quote-syntax #,k) 
                                       #,(print-convert pe)
                                       #,mut?)])))))


;; keeps track of expressions that always evaluate to true or always evaluate
;; to false, so that the optimizer can eliminate dead code
(define tautology-contradiction-table (make-hasheq))

(define-values (add-tautology add-contradiction)
  (let ()
    (define ((mk t?) e)
      (when (optimize?)
        (hash-set! tautology-contradiction-table e t?)))
    (values (mk #t) (mk #f))))
(define-values (tautology? contradiction?)
  (let ()
    (define ((mk t?) e)
      (eq? t? (hash-ref tautology-contradiction-table e 'not-there)))
    (values (mk #t) (mk #f))))

(p/c [add-typeof-expr (syntax? tc-results? . -> . any/c)]
     [type-of (syntax? . -> . tc-results?)]
     [reset-type-table (-> any/c)]
     [add-struct-fn! (identifier? StructPE? boolean? . -> . any/c)]
     [struct-accessor? (identifier? . -> . (or/c #f StructPE?))]
     [struct-mutator? (identifier? . -> . (or/c #f StructPE?))]
     [struct-fn-idx (identifier? . -> . exact-integer?)]
     [make-struct-table-code (-> syntax?)]
     [add-tautology (syntax? . -> . any/c)]
     [add-contradiction (syntax? . -> . any/c)]
     [tautology? (syntax? . -> . boolean?)]
     [contradiction? (syntax? . -> . boolean?)])
