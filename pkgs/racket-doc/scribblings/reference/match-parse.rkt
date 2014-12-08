#lang racket/base
(require scribble/scheme
         scribble/basic
         scribble/struct
         scribble/manual
         (for-label racket/base))

(provide parse-match-grammar)

(define (match-nonterm s)
  (make-element variable-color (list s)))

(define (fixup s middle)
  (lambda (m)
    (make-element #f
                  (list (fixup-meaning (substring s 0 (caar m)))
                        middle
                        (fixup-meaning (substring s (cdar m)))))))

(define (fixup-meaning s)
  (cond
   [(regexp-match-positions #rx"pattern" s)
    => (fixup s "pattern")]
   [(regexp-match-positions #rx"equal%" s)
    => (fixup s (racket equal?))]
   [(regexp-match-positions #rx"pat" s)
    => (fixup s (fixup-sexp 'pat))]
   [(regexp-match-positions #rx"qp" s)
    => (fixup s (fixup-sexp 'qp))]
   [(regexp-match-positions #rx"lvp" s)
    => (fixup s (fixup-sexp 'lvp))]
   [(regexp-match-positions #rx"struct-id" s)
    => (fixup s (fixup-sexp 'struct-id))]
   [(regexp-match-positions #rx"pred-expr" s)
    => (fixup s (fixup-sexp 'pred-expr))]
   [(regexp-match-positions #rx"expr" s)
    => (fixup s (fixup-sexp 'expr))]
   [(regexp-match-positions #rx"[*][*][*]" s)
    => (fixup s (racketidfont "..."))]
   [(regexp-match-positions #rx"[(]" s)
    => (fixup s (racketparenfont "("))]
   [(regexp-match-positions #rx"[)]" s)
    => (fixup s (racketparenfont ")"))]
   [(regexp-match-positions #rx"K" s)
    => (fixup s (match-nonterm "k"))]
   [else s]))

(define (fixup-rhs s)
  (let ([r (read (open-input-string s))])
    (to-element (fixup-sexp r))))

(define (fixup-sexp s)
  (cond
   [(pair? s)
    (cons (fixup-sexp (car s))
          (fixup-sexp (cdr s)))]
   [(vector? s)
    (list->vector (map fixup-sexp (vector->list s)))]
   [(box? s)
    (box (fixup-sexp (unbox s)))]
   [(symbol? s)
    (case s
      [(lvp pat qp literal ooo datum struct-id
            string bytes number character expr id
            rx-expr px-expr pred-expr
            derived-pattern)
       (match-nonterm (symbol->string s))]
      [(QUOTE VAR LIST LIST-REST LIST-NO-ORDER VECTOR HASH-TABLE BOX STRUCT 
              REGEXP PREGEXP AND OR NOT APP ? QUASIQUOTE CONS MCONS)
       (make-element symbol-color (list (string-downcase (symbol->string s))))]
      [(***)
       (make-element symbol-color '("..."))]
      [(___) (make-element symbol-color '("___"))]
      [(__K)
       (make-element #f (list (make-element symbol-color '("__"))
                              (match-nonterm "k")))]
      [(..K)
       (make-element #f (list (make-element symbol-color '(".."))
                              (match-nonterm "k")))]
      [else
       s])]
   [else s]))

(define re:start-prod #rx"^([^ ]*)( +)::= (.*[^ ])( +)[@](.*)$")
(define re:or-prod #rx"^( +) [|]  (.*[^ ])( +)[@](.*)$")
(define re:eng-prod #rx"^([^ ]*)( +):== (.*)$")

(define (parse-match-grammar grammar)
  (define lines (let ([lines (regexp-split "\r?\n" grammar)])
                  (reverse (cdr (reverse (cdr lines))))))

  (define spacer (hspace 1))

  (define (to-flow e)
    (make-flow (list (make-paragraph (list e)))))

  (define (table-line lhs eql rhs desc)
    (list (to-flow lhs)
          (to-flow spacer)
          (to-flow eql)
          (to-flow spacer)
          (to-flow rhs)
          (to-flow spacer)
          (to-flow desc)))

  (define equals (tt "::="))
  (define -or- (tt " | "))

  (make-table
   #f
   (map
    (lambda (line)
      (cond
       [(regexp-match re:start-prod line)
        => (lambda (m)
             (let ([prod (list-ref m 1)]
                   [lspace (list-ref m 2)]
                   [val (list-ref m 3)]
                   [rspace (list-ref m 4)]
                   [meaning (list-ref m 5)])
               (table-line (match-nonterm prod)
                           equals
                           (fixup-rhs val)
                           (fixup-meaning meaning))))]
       [(regexp-match re:eng-prod line)
        => (lambda (m)
             (let ([prod (list-ref m 1)]
                   [lspace (list-ref m 2)]
                   [meaning (list-ref m 3)])
               (table-line (match-nonterm prod)
                           equals
                           "???"
                           (fixup-meaning meaning))))]
       [(regexp-match re:or-prod line)
        => (lambda (m)
             (let ([lspace (list-ref m 1)]
                   [val (list-ref m 2)]
                   [rspace (list-ref m 3)]
                   [meaning (list-ref m 4)])
               (table-line spacer
                           -or-
                           (fixup-rhs val)
                           (fixup-meaning meaning))))]
       [else (error 'make-match-grammar
                    "non-matching line: ~e"
                    line)]))
    lines)))


