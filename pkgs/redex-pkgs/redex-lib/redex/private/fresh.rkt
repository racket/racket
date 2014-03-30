#lang racket/base
(require racket/contract
         racket/set)

(define (variable-not-in sexp var)
  (define var-str (symbol->string var))
  (define var-prefix
    (let ([m (regexp-match #rx"^(.*[^0-9])[0-9]+$" var-str)])
      (if m
          (cadr m)
          var-str)))
  (define found-exact-var? #f)
  (define nums
    (let loop ([sexp sexp]
               [nums (set)])
      (cond
        [(pair? sexp) (loop (cdr sexp) (loop (car sexp) nums))]
        [(symbol? sexp) 
         (when (equal? sexp var)
           (set! found-exact-var? #t))
         (define str (symbol->string sexp))
         (define match (regexp-match #rx"([0-9]+)$" str))
         (if (and match
                  (is-prefix? var-prefix str))
             (set-add nums (string->number (list-ref match 1)))
             nums)]
        [else nums])))
  (cond
    [(not found-exact-var?) var]
    [(set-empty? nums) (string->symbol (format "~a1" var))]
    [else (string->symbol (format "~a~a" var-prefix (find-best-number nums)))]))

(define (find-best-number nums)
  (let loop ([sorted (sort (set->list nums) <)]
             [i 1])
    (cond
      [(null? sorted) i]
      [else 
       (define fst (car sorted))
       (cond
         [(< i fst) i]
         [(> i fst) (loop (cdr sorted) i)]
         [(= i fst) (loop (cdr sorted) (+ i 1))])])))

(define (variables-not-in sexp vars)
  (let loop ([vars vars]
             [sexp sexp])
    (cond
      [(null? vars) null]
      [else 
       (define new-var (variable-not-in sexp (car vars)))
       (cons new-var
             (loop (cdr vars)
                   (cons new-var sexp)))])))

(define (is-prefix? str1 str2)
  (and (<= (string-length str1) (string-length str2))
       (for/and ([c1 (in-string str1)]
                 [c2 (in-string str2)])
         (equal? c1 c2))))

(provide/contract
 [variable-not-in (any/c symbol? . -> . symbol?)]
 [variables-not-in (any/c (listof symbol?) . -> . (listof symbol?))])
