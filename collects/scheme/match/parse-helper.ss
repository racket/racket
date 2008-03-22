#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap
         syntax/stx
         scheme/struct-info
         "patterns.ss"
         "compiler.ss"
         (only-in srfi/1 delete-duplicates))

(provide ddk? parse-literal all-vars pattern-var? match:syntax-err
         matchable?)

(define (matchable? e)
  (or (string? e) (bytes? e)))

;; raise an error, blaming stx
(define (match:syntax-err stx msg)
  (raise-syntax-error #f msg stx))

;; pattern-var? : syntax -> bool
;; is p an identifier representing a pattern variable?
(define (pattern-var? p)
  (and (identifier? p)
       (not (ddk? p))))

;; ddk? : syntax -> number or boolean
;; if #f is returned, was not a ddk identifier
;; if #t is returned, no minimum
;; if a number is returned, that's the minimum
(define (ddk? s*)
  (define (./_ c)
    (or (equal? c #\.)
        (equal? c #\_)))
  (let ([s (syntax->datum s*)])
    (and (symbol? s)
         (if (memq s '(... ___)) #t
             (let* ((s (symbol->string s)))                    
               (and (3 . <= . (string-length s))
                    (./_ (string-ref s 0))
                    (./_ (string-ref s 1))                    
                    (let ([n (string->number (substring s 2))])
                      (cond 
                        [(not n) #f]
                        [(zero? n) #t]
                        [(exact-nonnegative-integer? n) n]
                        [else (raise-syntax-error 'match "invalid number for ..k pattern" s*)]))))))))


;; parse-literal : scheme-val -> pat option
;; is v is a literal, return a pattern matching it
;; otherwise, return #f
(define (parse-literal v)
  (if (or (number? v) 
          (string? v) 
          (keyword? v)
          (symbol? v) 
          (bytes? v) 
          (regexp? v)
          (boolean? v)
          (char? v))
      (make-Exact v)
      #f))

;; (listof pat) syntax -> void
;; check that all the ps bind the same set of variables
(define (all-vars ps stx)
  (when (null? ps)
    (error 'bad))
  (let* ([first-vars (bound-vars (car ps))]
         [l (length ps)]
         [ht (make-free-identifier-mapping)])
    (for-each (lambda (v) (free-identifier-mapping-put! ht v 1)) first-vars)
    (for-each (lambda (p) 
                (for-each (lambda (v) 
                            (cond [(free-identifier-mapping-get ht v (lambda () #f)) 
                                   =>
                                   (lambda (n)
                                     (free-identifier-mapping-put! ht v (add1 n)))]
                                  [else (raise-syntax-error 'match "variable not bound in all or patterns" stx v)]))
                          (bound-vars p)))
              (cdr ps))
    (free-identifier-mapping-for-each
     ht
     (lambda (v n)
       (unless (= n l)
         (raise-syntax-error 'match "variable not bound in all or patterns" stx v))))))