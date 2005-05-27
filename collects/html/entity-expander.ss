;; copyright by Paul Graunke June 2000 AD
(module entity-expander mzscheme
  (provide empty-entity-expander extend-entity-expander expand-entities)
  ;; warning - this is a really inefficient implementation of a nice interface.
  ;; building one dfa would be much better
  
  ;; Entity-expander : String -> String
  
  ;; empty-entity-expander : Entity-expander
  (define (empty-entity-expander x) x)
  
  ;; extend-entity-expander : Symbol String Entity-expander -> Entity-expander
  (define extend-entity-expander
    (let ([fix-rhs (regexp "&")])
      (lambda (name rhs to-extend)
        (let ([pattern (regexp (format "%~a;?" name))]
              [rhs (regexp-replace* fix-rhs rhs "\\\\&")])
          (lambda (input)
            (to-extend (regexp-replace* pattern input rhs)))))))
  
  ;; expand-entities : Entity-expander String -> String
  (define (expand-entities expander string)
    (let loop ([prev string])
      (let ([new (expander prev)])
        (if (string=? new prev)
            new
            (loop new))))))
