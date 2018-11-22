
(define gensym
  (case-lambda
   [() (#%gensym)]
   [(s) (cond
         [(string? s) (#%gensym (append-gensym-counter s))]
         [(symbol? s) (#%gensym (append-gensym-counter (chez:symbol->string s)))]
         [else (raise-argument-error
                'gensym
                "(or/c symbol? string?)"
                s)])]))

(define gensym-counter (box 0))

(define (append-gensym-counter s)
  (let loop ()
    (let ([c (#%unbox gensym-counter)])
      (if (#%box-cas! gensym-counter c (add1 c))
          (string-append s (number->string c))
          (loop)))))

(define/who (symbol-interned? s)
  (check who symbol? s)
  (not (gensym? s)))

(define unreadable-unique-name "gr8mwsuasnvzbl9jjo6e9b-")

(define/who (symbol-unreadable? s)
  (check who symbol? s)
  (and (gensym? s)
       (equal? (gensym->unique-string s)
               (string-append unreadable-unique-name (symbol->string s)))))

(define/who (symbol->string s)
  (check who symbol? s)
  (string-copy (chez:symbol->string s)))

(define/who (string->uninterned-symbol str)
  (check who string? str)
  (chez:gensym (string->immutable-string str)))

(define/who (string->unreadable-symbol str)
  (check who string? str)
  (chez:gensym (string->immutable-string str)
               (string-append unreadable-unique-name str)))

(define/who symbol<?
  (case-lambda
   [(a b)
    (check who symbol? a)
    (check who symbol? b)
    (string<? (symbol->string a)
              (symbol->string b))]
   [(a . as)
    (check who symbol? a)
    (let loop ([a a] [as as] [r #t])
      (cond
       [(null? as) r]
       [else
        (let ([b (car as)])
          (check who symbol? b)
          (loop b (cdr as) (and r (symbol<? a b))))]))]))
