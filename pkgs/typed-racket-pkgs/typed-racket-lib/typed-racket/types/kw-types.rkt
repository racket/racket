#lang racket/base

(require "abbrev.rkt" "../rep/type-rep.rkt"
         "union.rkt" "../utils/tc-utils.rkt"
         racket/list racket/dict racket/match)

;; convert : [Listof Keyword] [Listof Type] [Listof Type] [Option Type] [Option Type] -> (values Type Type)
(define (convert kw-t plain-t opt-t rng rest drest split?)
  (define-values (mand-kw-t opt-kw-t) (partition (match-lambda [(Keyword: _ _ m) m]) kw-t))

  (when drest
    (int-err "drest passed to kw-convert"))

  (define arities
    (for/list ([i (in-range (length opt-t))])
      (make-arr* (append plain-t (take opt-t i))
                 rng
                 #:kws kw-t
                 #:rest rest
                 #:drest drest)))
  (define ts 
    (flatten
     (list
      (for/list ([k (in-list mand-kw-t)])
        (match k
          [(Keyword: _ t _) t]))
      (for/list ([k (in-list opt-kw-t)])
        (match k
          [(Keyword: _ t _) (list (-opt t) -Boolean)]))
      plain-t
      (for/list ([t (in-list opt-t)]) (-opt t))
      (for/list ([t (in-list opt-t)]) -Boolean)
      ;; the kw function protocol passes rest args as an explicit list
      (if rest (-lst rest) empty))))
  (define ts/true
    (flatten
     (list
      (for/list ([k (in-list mand-kw-t)])
        (match k
          [(Keyword: _ t _) t]))
      (for/list ([k (in-list opt-kw-t)])
        (match k
          [(Keyword: _ t _) (list t (-val #t))]))
      plain-t
      (for/list ([t (in-list opt-t)]) t)
      (for/list ([t (in-list opt-t)]) (-val #t))
      ;; the kw function protocol passes rest args as an explicit list
      (if rest (-lst rest) empty))))
  (define ts/false
    (flatten
     (list
      (for/list ([k (in-list mand-kw-t)])
        (match k
          [(Keyword: _ t _) t]))
      (for/list ([k (in-list opt-kw-t)])
        (match k
          [(Keyword: _ t _) (list (-val #f) (-val #f))]))
      plain-t
      (for/list ([t (in-list opt-t)]) (-val #f))
      (for/list ([t (in-list opt-t)]) (-val #f))
      ;; the kw function protocol passes rest args as an explicit list
      (if rest (-lst rest) empty))))
  (if split?
      (make-Function (list (make-arr* ts/true rng)
                           (make-arr* ts/false rng)))
      (make-Function (list (make-arr* ts rng)))))

(define (prefix-of a b)
  (define (rest-equal? a b)
    (match* (a b)
      [(#f #f) #t]
      [(#f _) #f]
      [(_ #f) #f]
      [(a b) (type-equal? a b)]))
  (define (drest-equal? a b)
    (match* (a b)
      [((list t b) (list t* b*)) (and (type-equal? t t*) (equal? b b*))]
      [(#f #f) #t]
      [(_ _) #f]))
  (define (kw-equal? a b)
    (and (equal? (length a) (length b))
         (for/and ([k1 (in-list a)] [k2 (in-list b)])
           (type-equal? k1 k2))))
  (match* (a b)
    [((arr: args result rest drest kws)
      (arr: args* result* rest* drest* kws*))
     (and (< (length args) (length args*))
          (rest-equal? rest rest*)
          (drest-equal? drest drest*)
          (type-equal? result result*)
          (kw-equal? kws kws*)
          (for/and ([p (in-list args)] [p* (in-list args*)])
            (type-equal? p p*)))]))

(define (arity-length a)
  (match a
    [(arr: args result rest drest kws) (length args)]))


(define (arg-diff a1 a2)
  (match a2
    [(arr: args _ _ _ _) (drop args (arity-length a1))]))

(define (find-prefixes l)
  (define l* (sort l < #:key arity-length))
  (for/fold ([d (list)]) ([e (in-list l*)])
    (define prefix (for/or ([p (in-dict-keys d)])
                     (and (prefix-of p e) p)))
    (if prefix
        (dict-set d prefix (arg-diff prefix e))
        (dict-set d e empty))))

(define (kw-convert ft #:split [split? #f])
    (match ft
      [(Function: arrs)
       (define table (find-prefixes arrs))
       (define fns 
         (for/list ([(k v) (in-dict table)])
           (match k
             [(arr: mand rng rest drest kws)
              (convert kws mand v rng rest drest split?)])))
       (apply cl->* fns)]
      [(Poly-names: names (Function: arrs))
       (define table (find-prefixes arrs))
       (define fns 
         (for/list ([(k v) (in-dict table)])
           (match k
             [(arr: mand rng rest drest kws)
              (convert kws mand v rng rest drest split?)])))
       (make-Poly names (apply cl->* fns))]
      [_ (int-err "kw-convert: non-function type ~a" ft)]))

(provide kw-convert)
