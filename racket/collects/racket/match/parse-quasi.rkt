#lang racket/base

(require (for-template racket/base)
         "patterns.rkt"
         "parse-helper.rkt")

(provide parse-quasi)

;; is pat a pattern representing a list?
(define (null-terminated? pat)
  (cond [(Pair? pat) (null-terminated? (Pair-d pat))]
        [(GSeq? pat) (null-terminated? (GSeq-tail pat))]
        [(Null? pat) #t]
        [else #f]))

;; combine a null-terminated pattern with another pattern to match afterwards
(define (append-pats p1 p2)
  (cond [(Pair? p1) (make-Pair (Pair-a p1) (append-pats (Pair-d p1) p2))]
        [(GSeq? p1) (make-GSeq (GSeq-headss p1)
                               (GSeq-mins p1)
                               (GSeq-maxs p1)
                               (GSeq-onces? p1)
                               (append-pats (GSeq-tail p1) p2)
                               (GSeq-mutable? p1))]
        [(Null? p1) p2]
        [else (error 'match "illegal input to append-pats")]))

;; parse stx as a quasi-pattern
;; parse parses unquote
(define (parse-quasi stx parse)
  (define (pq s) (parse-quasi s parse))
  (syntax-case stx (quasiquote unquote quote unquote-splicing)
    [(unquote p) (parse #'p)]
    [((unquote-splicing p) . rest)
     (let ([pat (parameterize ([in-splicing? #t]) (parse #'p))]
           [rpat (pq #'rest)])
       (if (null-terminated? pat)
           (append-pats pat rpat)
           (raise-syntax-error 'match "non-list pattern inside unquote-splicing"
                               stx #'p)))]
    [(p dd . rest)
     (ddk? #'dd)
     ;; FIXME: parameterize dd-parse so that it can be used here
     (let* ([count (ddk? #'dd)]
            [min (and (number? count) count)])
       (make-GSeq
        (parameterize ([match-...-nesting (add1 (match-...-nesting))])
          (list (list (pq #'p))))
        (list min)
        ;; no upper bound
        (list #f)
        ;; patterns in p get bound to lists
        (list #f)
        (pq #'rest)
        #f))]
    [(a . b) (make-Pair (pq #'a) (pq #'b))]
    ;; prefab structs
    [struct
     (prefab-struct-key (syntax-e #'struct))
     (let ([key (prefab-struct-key (syntax-e #'struct))]
           [pats (cdr (vector->list (struct->vector (syntax-e #'struct))))])
       (make-And (list (make-Pred #`(struct-type-make-predicate (prefab-key->struct-type '#,key #,(length pats))))
                       (make-App #'struct->vector
                                 (list (make-Vector (cons (make-Dummy #f) (map pq pats)))))))
       #;
       (make-PrefabStruct key (map pq pats)))]
    ;; the hard cases
    [#(p ...)
     (ormap (lambda (p)
              (or (ddk? p)
                  (syntax-case p (unquote-splicing)
                    [(unquote-splicing . _) #t]
                    [_ #f])))
            (syntax->list #'(p ...)))
     (make-And (list (make-Pred #'vector?)
                     (make-App #'vector->list
                               (list (pq (quasisyntax/loc stx (p ...)))))))]
    [#(p ...)
     (make-Vector (map pq (syntax->list #'(p ...))))]
    [bx
     (box? (syntax-e #'bx))
     (make-Box (pq (unbox (syntax-e #'bx))))]    
    [()
     (make-Null (make-Dummy #f))]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in quasipattern" stx))]))
