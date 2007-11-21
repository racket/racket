#lang scheme/base
(require scribble/scheme
         scribble/basic
         scribble/struct)

(provide match-grammar)

(define grammar "
pat     ::= other-identifier                  @Match anything, bind identifier
         |  _                                 @Match anything
         |  literal                           @Match literal
         |  (QUOTE datum)                     @Match equal% datum (e.g., symbol)
         |  (LIST lvp ...)                    @Match sequence of lvps
         |  (LIST-REST lvp ... pat)           @Match lvps consed onto a pat
         |  (LIST-NO-ORDER pat ... lvp)       @Match arguments in a list in any order
         |  (VECTOR lvp ... lvp)              @Match vector of pats
         |  (HASH-TABLE (pat pat) ...)        @Match hash table mapping pats to pats
         |  (HASH-TABLE (pat pat) ... ooo)    @Match hash table mapping pats to pats
         |  (BOX pat)                         @Match boxed pat
         |  (STRUCT struct-name (pat ...))    @Match struct-name instance
         |  (REGEXP rx-expr)                  @Match astr using (r-match rx-expr ...)
         |  (REGEXP rx-expr pat)              @Match astr to rx-expr, pat matches regexp result
         |  (PREGEXP prx-expr)                @Match astr using (pr-match prx-expr ...)
         |  (PREGEXP prx-expr pat)            @Match astr to prx-expr, pat matches pregexp result
         |  (AND pat ...)                     @Match when all pats match
         |  (OR pat ...)                      @Match when any pat match
         |  (NOT pat ...)                     @Match when no pat match
         |  (APP expr pat)                    @Match when result of applying expr to the value matches pat
         |  (? pred-expr pat ...)             @Match if pred-expr is true on the value, and all pats match
         |  (set! identifier)                 @Match anything, bind as setter
         |  (get! identifier)                 @Match anything, bind as getter
         |  (QUASIQUOTE qp)                   @Match a quasipattern
literal ::= ()                                @Match the empty list
         |  #t                                @Match true
         |  #f                                @Match false
         |  string                            @Match equal% string
         |  number                            @Match equal% number
         |  character                         @Match equal% character
lvp     ::= (code:line pat ooo)               @Greedily match pat instances
         |  pat                               @Match pat
ooo     ::= ***                               @Zero or more (where *** is a keyword)
         |  ___                               @Zero or more
         |  ..K                               @K or more, where K is a non-negative integer
         |  __K                               @K or more, where K is a non-negative integer
qp      ::= literal                           @Match literal
         |  identifier                        @Match equal% symbol
         |  (qp ...)                          @Match sequences of qps
         |  (qp ... . qp)                     @Match sequence of qps consed onto a qp
         |  (qp ... ooo)                      @Match qps consed onto a repeated qp
         |  #(qp ...)                         @Match vector of qps
         |  #&qp                              @Match boxed qp
         |  ,pat                              @Match pat
         |  ,@(LIST lvp ...)                  @Match lvp sequence, spliced
         |  ,@(LIST-REST lvp ... pat)         @Match lvp sequence plus pat, spliced
         |  ,@'qp                             @Match list-matching qp, spliced
")

(define (match-nonterm s)
  (make-element "schemevariable" (list s)))
  
(define (fixup-meaning s)
  s)

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
      [(lvp pat qp literal other-identifier ooo)
       (match-nonterm (symbol->string s))]
      [(QUOTE LIST)
       (make-element "schemesymbol" (list (string-downcase (symbol->string s))))]
      [else
       s])]
   [else s]))

(define re:start-prod "^([^ ]*)( +)::= (.*[^ ])( +)[@](.*)$")
(define re:or-prod "^( +) [|]  (.*[^ ])( +)[@](.*)$")
(define re:eng-prod "^([^ ]*)( +):== (.*)$")

(define lines (regexp-split "\n" (substring grammar 1 (sub1 (string-length grammar)))))

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
                           (fixup-meaning meaning))))]))
    lines)))
