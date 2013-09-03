#lang racket/base

(require "abbrev.rkt" "../rep/type-rep.rkt"
         "../utils/tc-utils.rkt"
         racket/list racket/set racket/dict racket/match)

;; convert : [Listof Keyword] [Listof Type] [Listof Type] [Option Type]
;;           [Option Type] [Option (Pair Type symbol)] boolean -> Type
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
      (for/list ([t (in-list opt-t)]) (-val #f)))))
  (make-Function
    (if split?
        (remove-duplicates
          (list (make-arr* ts/true rng #:rest rest #:drest drest)
                (make-arr* ts/false rng #:rest rest #:drest drest)))
        (list (make-arr* ts rng #:rest rest #:drest drest)))))

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

(define (inner-kw-convert arrs split?)
  (define table (find-prefixes arrs))
  (define fns
    (for/set ([(k v) (in-dict table)])
      (match k
        [(arr: mand rng rest drest kws)
         (convert kws mand v rng rest drest split?)])))
  (apply cl->* (set->list fns)))

(define (kw-convert ft #:split [split? #f])
    (match ft
      [(Function: arrs)
       (inner-kw-convert arrs split?)]
      [(Poly-names: names f)
       (make-Poly names (kw-convert f #:split split?))]
      [(PolyDots-names: names f)
       (make-PolyDots names (kw-convert f #:split split?))]))

(define ((opt-convert-arr required-pos optional-pos) arr)
  (match arr
    [(arr: args result #f #f '())
     (define num-args (length args))
     (and (>= num-args required-pos)
          (<= num-args (+ required-pos optional-pos))
          (let* ([required-args (take args required-pos)]
                 [opt-args (drop args required-pos)]
                 [missing-opt-args (- (+ required-pos optional-pos) num-args)]
                 [present-flags (map (λ (t) (-val #t)) opt-args)]
                 [missing-args (make-list missing-opt-args (-val #f))])
            (make-arr (append required-args
                              opt-args
                              missing-args
                              present-flags
                              missing-args)
                      result
                      #f
                      #f
                      '())))]
    [(arr: args result _ _ _) #f]))

(define (opt-convert ft required-pos optional-pos)
  (let/ec exit
    (let loop ((ft ft))
      (match ft
        [(Function: arrs)
         (let ((arrs (map (opt-convert-arr required-pos optional-pos) arrs)))
           (if (andmap values arrs)
               (make-Function arrs)
               (exit #f)))]
        [(Poly-names: names f)
         (make-Poly names (loop f))]
        [(PolyDots-names: names f)
         (make-PolyDots names (loop f))]
        [t t]))))

(provide kw-convert opt-convert)
