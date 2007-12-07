#lang scheme/base
(require scribble/scheme
         scribble/basic
         scribble/struct
         scribble/manual
         (for-label scheme/base))

(provide match-grammar)

(define grammar "
pat     ::= id                                @match anything, bind identifier
         |  _                                 @match anything
         |  literal                           @match literal
         |  (QUOTE datum)                     @match equal% value
         |  (LIST lvp ...)                    @match sequence of lvps
         |  (LIST-REST lvp ... pat)           @match lvps consed onto a pat
         |  (LIST-NO-ORDER pat ...)           @match pats in any order
         |  (LIST-NO-ORDER pat ... lvp)       @match pats in any order
         |  (VECTOR lvp ...)                  @match vector of pats
         |  (HASH-TABLE (pat pat) ...)        @match hash table
         |  (HASH-TABLE (pat pat) ...+ ooo)   @match hash table
         |  (BOX pat)                         @match boxed pat
         |  (STRUCT struct-id (pat ...))      @match struct-id instance
         |  (REGEXP rx-expr)                  @match string
         |  (REGEXP rx-expr pat)              @match string, result with pat
         |  (PREGEXP px-expr)                 @match string
         |  (PREGEXP px-expr pat )            @match string, result with pat
         |  (AND pat ...)                     @match when all pats match
         |  (OR pat ...)                      @match when any pat match
         |  (NOT pat ...)                     @match when no pat matches
         |  (APP expr pat)                    @match (expr value) to pat
         |  (? expr pat ...)                  @match if (expr value) and pats
         |  (set! id)                         @match anything, bind as setter
         |  (get! id)                         @match anything, bind as getter
         |  (QUASIQUOTE qp)                   @match a quasipattern
         |  derived-pattern                   @match using extension
literal ::= ()                                @match the empty list
         |  #t                                @match true
         |  #f                                @match false
         |  string                            @match equal% string
         |  number                            @match equal% number
         |  char                              @match equal% character
lvp     ::= (code:line pat ooo)               @greedily match pat instances
         |  pat                               @match pat
qp      ::= literal                           @match literal
         |  id                                @match symbol
         |  (qp ...)                          @match sequences of qps
         |  (qp ... . qp)                     @match qps ending qp
         |  (qp ... ooo)                      @match qps ending repeated qp
         |  #(qp ...)                         @match vector of qps
         |  #&qp                              @match boxed qp
         |  ,pat                              @match pat
         |  ,@(LIST lvp ...)                  @match lvps, spliced
         |  ,@(LIST-REST lvp ... pat)         @match lvps plus pat, spliced
         |  ,@'qp                             @match list-matching qp, spliced
ooo     ::= ***                               @zero or more; *** is literal
         |  ___                               @zero or more
         |  ..K                               @K or more
         |  __K                               @K or more
")

(define (match-nonterm s)
  (make-element "schemevariable" (list s)))

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
    => (fixup s (scheme equal?))]
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
    => (fixup s (schemeidfont "..."))]
   [(regexp-match-positions #rx"[(]" s)
    => (fixup s (schemeparenfont "("))]
   [(regexp-match-positions #rx"[)]" s)
    => (fixup s (schemeparenfont ")"))]
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
            string number character expr id
            rx-expr px-expr pred-expr
            derived-pattern)
       (match-nonterm (symbol->string s))]
      [(QUOTE LIST LIST-REST LIST-NO-ORDER VECTOR HASH-TABLE BOX STRUCT 
              REGEXP PREGEXP AND OR NOT APP ? get! set! QUASIQUOTE)
       (make-element "schemesymbol" (list (string-downcase (symbol->string s))))]
      [(***)
       (make-element "schemesymbol" '("..."))]
      [(___) (make-element "schemesymbol" '("___"))]
      [(__K)
       (make-element #f (list (make-element "schemesymbol" '("__"))
                              (match-nonterm "k")))]
      [(..K)
       (make-element #f (list (make-element "schemesymbol" '(".."))
                              (match-nonterm "k")))]
      [else
       s])]
   [else s]))

(define re:start-prod "^([^ ]*)( +)::= (.*[^ ])( +)[@](.*)$")
(define re:or-prod "^( +) [|]  (.*[^ ])( +)[@](.*)$")
(define re:eng-prod "^([^ ]*)( +):== (.*)$")

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

(define match-grammar
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
