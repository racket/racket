
(define gensym
  (case-lambda
   [() (gensym "g")]
   [(s) (cond
         [(string? s) (string->uninterned-symbol (append-gensym-counter s))]
         [(symbol? s) (string->uninterned-symbol (append-gensym-counter (#%symbol->string s)))]
         [else (raise-argument-error
                'gensym
                "(or/c symbol? string?)"
                s)])]))

(define gensym-counter (box 0))

(define (append-gensym-counter s)
  (let loop ()
    (let ([c (#%unbox gensym-counter)])
      (if (#%box-cas! gensym-counter c (add1 c))
          (let ([a (string-append s (number->string c))])
            (#%$string-set-immutable! a)
            a)
          (loop)))))

(define/who (symbol-interned? s)
  (check who symbol? s)
  (not (or (gensym? s)
           (uninterned-symbol? s))))

(define unreadable-unique-name "unreadable:")
(define unreadable-unique-name-length (string-length unreadable-unique-name))

(define/who (symbol-unreadable? s)
  (check who symbol? s)
  (and (gensym? s)
       (let ([u (gensym->unique-string s)]
             [str (#%symbol->string s)])
         (let ([len (string-length str)])
           (and (fx= (string-length u)
                     (fx+ unreadable-unique-name-length len))
                (let loop ([i 0])
                  (or (fx= i unreadable-unique-name-length)
                      (and (char=? (string-ref unreadable-unique-name i)
                                   (string-ref u i))
                           (loop (fx+ i 1)))))
                (let loop ([i 0])
                  (or (fx= i len)
                      (and (char=? (string-ref str i)
                                   (string-ref u (fx+ unreadable-unique-name-length i)))
                           (loop (fx+ i 1))))))))))

(define/who (symbol->string s)
  (check who symbol? s)
  (string-copy (#%symbol->string s)))

(define/who (symbol->immutable-string s)
  (check who symbol? s)
  (#%symbol->string s))

(define/who (string->unreadable-symbol str)
  (check who string? str)
  (#%gensym str
            (let ([a (string-append unreadable-unique-name str)])
              (#%$string-set-immutable! a)
              a)))

(define/who symbol<?
  (case-lambda
   [(a b)
    (check who symbol? a)
    (check who symbol? b)
    (string<? (#%symbol->string a)
              (#%symbol->string b))]
   [(a . as)
    (check who symbol? a)
    (let loop ([a a] [as as] [r #t])
      (cond
       [(null? as) r]
       [else
        (let ([b (car as)])
          (check who symbol? b)
          (loop b (cdr as) (and r (symbol<? a b))))]))]))
