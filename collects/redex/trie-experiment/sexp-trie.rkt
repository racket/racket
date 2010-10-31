#lang racket

(require rackunit)

(provide empty-sexp-trie
         lookup
         insert
         atom-map-histo)

(struct sexp-trie (atoms   ; (dict/c any α)
                   lists) #:transparent) ; (or/c (sexp-list-trie α) #f)

(struct sexp-list-trie (empty       ; (or/c #f α)
                        non-empty) #:transparent) ; (sexp-trie (sexp-list-trie α))

;; empty-sexp-trie: sexp-trie α
(define empty-sexp-trie (sexp-trie '() #f))

;; lookup: sexp (sexp-trie α) -> (or/c #f α)
(define (lookup s t)
  (if (list? s)
      (and (sexp-trie-lists t)
           (lookup-list s (sexp-trie-lists t)))
      (dict-ref (sexp-trie-atoms t) s #f)))

;; lookup-list: (listof sexp) (sexp-list-trie α) -> (or/c #f α)
(define (lookup-list ss t)
  (match ss
    ['() (sexp-list-trie-empty t)]
    [(cons s ss’)
     (match (lookup s (sexp-list-trie-non-empty t))
       [#f #f]
       [t’ (lookup-list ss’ t’)])]))

;; replace: sexp (α -> α) (sexp-trie α) -> (sexp-trie α)
(define (replace s f t)
  (let ([t (or t empty-sexp-trie)])
    (if (list? s)
        (let ([t’ (or (sexp-trie-lists t)
                      (sexp-list-trie #f empty-sexp-trie))])
          (sexp-trie (sexp-trie-atoms t)
                     (replace-list s f t’)))
        (sexp-trie (dict-set (sexp-trie-atoms t) s
                             (f (dict-ref (sexp-trie-atoms t) s #f)))
                   (sexp-trie-lists t)))))

;; replace-list: (listof sexp) (α -> α) (sexp-list-trie α) -> (sexp-list-trie α)
(define (replace-list ss f t) 
  (match ss
    ['() 
     (sexp-list-trie 
      (f (sexp-list-trie-empty t))
      (sexp-list-trie-non-empty t))]
    [(cons s ss’)
     (sexp-list-trie
      (sexp-list-trie-empty t)
      (replace 
       s
       (λ (t’) (replace-list ss’ f (or t’ (sexp-list-trie #f empty-sexp-trie))))
       (sexp-list-trie-non-empty t)))]))

;; insert: sexp α (sexp-trie α) -> (sexp-trie α)
(define (insert s v t) (replace s (λ (_) v) t))

(define-syntax (test-insert-lookup stx)
  (syntax-case stx ()
    [(_ (insertions ...) (lookups ...))
     (with-syntax ([(checks ...)
                    (for/list ([lookup-stx (syntax->list #'(lookups ...))])
                      (with-syntax ([(k . v) lookup-stx])
                        (syntax/loc lookup-stx
                          (check-equal? (lookup 'k t) 'v))))])
       #'(let ([t (for/fold ([t empty-sexp-trie]) ([i '(insertions ...)])
                    (insert (car i) (cdr i) t))])
           checks ...))]))

(define-syntax-rule (test-inserted insertions)
  (test-insert-lookup insertions insertions))

(test-insert-lookup () ((a . #f)))
(test-insert-lookup ((a . 1))
                    ((a . 1) (b . #f)))
(test-inserted ((a . 1) (b . 2)))
(test-insert-lookup ((a . 1) ((b) . 2))
                    ((a . 1) ((b) . 2)))
(test-insert-lookup (((b) . 2) (a . 1))
                    ((a . 1) ((b) . 2)))
(test-inserted (((a b c) . 1) 
                ((a b d) . 2)
                ((a b c d) . 3)))
(test-insert-lookup (((a b c) . 1))
                    (((a b) . #f)))
(test-inserted (((((a))) . 1)))
(test-inserted (((a) . 1) ((b) . 1)))
(test-insert-lookup () ((() . #f)))
(test-inserted ((a . 1) (() . 2) ((a) . 3)))
(test-inserted ((() . 2) ((a) . 3)))
(test-inserted ((a . 1) ((a) . 3) (() . 2)))
(test-inserted (((a) . 3) (() . 2)))
(test-inserted (((a) . 1) ((b) . 2)))
(test-inserted (((a b) . 1) ((a c) . 2)))

; atom-map-histo: (sexp-trie α) -> (listof (cons/c nat/c nat/c))
(define (atom-map-histo t)
  (define h (make-hash))
  (for ([c (let counts ([t t])
             (match t
               [(sexp-trie as l)
                (for/fold ([cs (cons (length as) (counts l))]) ([a as])
                          (match-let ([(cons _ x) a])
                            (cond [(sexp-list-trie? x)
                                   (append (counts x) cs)]
                                  [else cs])))]
               [(sexp-list-trie e n)
                (append (counts e) (counts n))]
               [_ '()]))])
       (hash-update! h c add1 0))
  (sort (hash-map h cons) <= #:key car))

(check-equal?
 (atom-map-histo
  (insert '(c d) #t (insert '(b) #t (insert 'a #t empty-sexp-trie))))
 '((0 . 2) (1 . 2) (2 . 1)))

(check-equal?
 (atom-map-histo (insert '(() a) 1 empty-sexp-trie))
 '((0 . 4) (1 . 1)))