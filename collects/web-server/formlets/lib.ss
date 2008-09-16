#lang scheme
(require web-server/private/request-structs
         xml)

; Combinators
(define (const x) (lambda _ x))
(define (id x) x)

; Formlets
(define (pure x)
  (lambda (i)
    (values empty (const x) i)))

(define (cross f p)
  (lambda (i)
    (let*-values ([(x1 g i) (f i)]
                  [(x2 q i) (p i)])
      (values (append x1 x2)
              (lambda (env)
                (let ([ge (g env)]
                      [qe (q env)])
                  (ge qe)))
              i))))

;; This is gross because OCaml auto-curries
(define (cross* f . gs)
  (lambda (i)
    (let*-values ([(fx fp fi) (f i)]
                  [(gs-x gs-p gs-i)
                   (let loop ([gs gs]
                              [xs empty]
                              [ps empty]
                              [i fi])
                     (if (empty? gs)
                         (values (reverse xs) (reverse ps) i)
                         (let-values ([(gx gp gi) ((first gs) i)])
                           (loop (rest gs) (list* gx xs) (list* gp ps) gi))))])
      (values (apply append fx gs-x)
              (lambda (env)
                (let ([fe (fp env)]
                      [gs-e (map (lambda (g) (g env)) gs-p)])
                  (apply fe gs-e)))
              gs-i))))

(define (xml x)
  (lambda (i)
    (values (list x) (const id) i)))

(define (xml-forest x)
  (lambda (i)
    (values x (const id) i)))

(define (text x)
  (xml x))

(define (tag-xexpr t ats f)
  (lambda (i)
    (let-values ([(x p i) (f i)])
      (values (list (list* t ats x))  p i))))

(define (next-name i)
  (values (format "input_~a" i) (add1 i)))
(define (input i)
  (let-values ([(w i) (next-name i)])
    (values (list `(input ([name ,w])))
            (lambda (env) (bindings-assq (string->bytes/utf-8 w) env))
            i)))

; Helpers
(define (formlet-display f)
  (let-values ([(x p i) (f 0)])
    x))

(define (formlet-process f r)
  (let-values ([(x p i) (f 0)])
    (p (request-bindings/raw r))))

; Input Formlets
(define input-string
  (cross 
   (pure (lambda (bf)
           (bytes->string/utf-8 (binding:form-value bf))))
   input))
         
(define input-int
  (cross
   (pure string->number)
   input-string))

(define input-symbol
  (cross
   (pure string->symbol)
   input-string))

; Contracts
(define xexpr-forest/c
  (listof xexpr?))

(define (formlet/c c)
  (integer? . -> . 
            (values xexpr-forest/c
                    ((listof binding?) . -> . (coerce-contract 'formlet/c c))
                    integer?)))

(define alpha any/c)
(define beta any/c)

(provide/contract
 [formlet/c (any/c . -> . contract?)]
 [pure (alpha
        . -> . (formlet/c alpha))]
 [cross ((formlet/c (alpha . -> . beta))
         (formlet/c alpha)
         . -> . (formlet/c beta))]
 [cross* (((formlet/c (() () #:rest (listof alpha) . ->* . beta)))
          () #:rest (listof (formlet/c alpha))
          . ->* . (formlet/c beta))]
 [xml (xexpr? . -> . (formlet/c procedure?))]
 [xml-forest (xexpr-forest/c . -> . (formlet/c procedure?))]
 [text (string? . -> . (formlet/c procedure?))]
 [tag-xexpr (symbol? (listof (list/c symbol? string?)) (formlet/c alpha) . -> . (formlet/c alpha))]
 [input-string (formlet/c string?)]
 [input-int (formlet/c integer?)]
 [input-symbol (formlet/c symbol?)]
 [formlet-display ((formlet/c alpha) . -> . xexpr-forest/c)]
 [formlet-process ((formlet/c alpha) request? . -> . alpha)])