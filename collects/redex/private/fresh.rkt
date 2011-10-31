#lang racket/base
(require racket/contract)

(define re:gen-d #rx".*[^0-9]([0-9]+)$")
(define (variable-not-in sexp var)
  (let* ([var-str (symbol->string var)]
         [var-prefix (let ([m (regexp-match #rx"^(.*[^0-9])[0-9]+$" var-str)])
                       (if m
                           (cadr m)
                           var-str))]
         [found-exact-var? #f]
         [nums (let loop ([sexp sexp]
                          [nums null])
                 (cond
                   [(pair? sexp) (loop (cdr sexp) (loop (car sexp) nums))]
                   [(symbol? sexp) 
                    (when (eq? sexp var)
                      (set! found-exact-var? #t))
                    (let* ([str (symbol->string sexp)]
                           [match (regexp-match re:gen-d str)])
                      (if (and match
                               (is-prefix? var-prefix str))
                          (cons (string->number (cadr match)) nums)
                          nums))]
                   [else nums]))])
    (cond
      [(not found-exact-var?) var]
      [(null? nums) (string->symbol (format "~a1" var))]
      [else (string->symbol (format "~a~a" var-prefix (find-best-number nums)))])))

(define (find-best-number nums)
  (let loop ([sorted (sort nums <)]
             [i 1])
    (cond
      [(null? sorted) i]
      [else 
       (let ([fst (car sorted)])
         (cond
           [(< i fst) i]
           [(> i fst) (loop (cdr sorted) i)]
           [(= i fst) (loop (cdr sorted) (+ i 1))]))])))

(define (variables-not-in sexp vars)
  (let loop ([vars vars]
             [sexp sexp])
    (cond
      [(null? vars) null]
      [else 
       (let ([new-var (variable-not-in sexp (car vars))])
         (cons new-var
               (loop (cdr vars)
                     (cons new-var sexp))))])))

(define (is-prefix? str1 str2)
  (and (<= (string-length str1) (string-length str2))
       (equal? str1 (substring str2 0 (string-length str1)))))

(provide/contract
 [variable-not-in (any/c symbol? . -> . symbol?)]
 [variables-not-in (any/c (listof symbol?) . -> . (listof symbol?))])
