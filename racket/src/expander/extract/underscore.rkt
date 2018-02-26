#lang racket/base

(provide simplify-underscore-numbers)

;; Small changes to the input code can trigger lots of renumberings
;; for local variables, where the expander adds "_<num>" suffixes to
;; generate local-variable names, and the "<num>"s count up across all
;; symbols. Renumber with symbol-specific counting to reduce
;; unneccessary changes to generated code. A simple strategy works
;; because no primitive or exported name has a "_<num>" suffix.

(define (simplify-underscore-numbers s)
  (define replacements (make-hasheq))
  (define base-counts (make-hasheq))
  (let loop ([s s])
    (cond
     [(symbol? s)
      (cond
       [(hash-ref replacements s #f)
        => (lambda (r) r)]
       [else
        (define str (symbol->string s))
        (define m (regexp-match-positions #rx"_[0-9]+$" str))
        (cond
         [(not m)
          (hash-set! replacements s s)
          s]
         [else
          (define base (substring str 0 (caar m)))
          (define base-s (string->symbol base))
          (define n (hash-ref base-counts base-s 0))
          (hash-set! base-counts base-s (add1 n))
          (define r (string->symbol (format "~a_~a" base n)))
          (hash-set! replacements s r)
          r])])]
     [(pair? s)
      (if (eq? (car s) 'quote)
          s
          (cons (loop (car s)) (loop (cdr s))))]
     [else s])))
