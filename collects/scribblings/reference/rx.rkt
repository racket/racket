#lang at-exp racket/base
(require scribble/core scribble/manual scribble/bnf
         racket/list racket/string)

(define grammar @string-append{
  Regexp   ::= Pces               Match Pces                                      #co
            |  Regexp|Regexp      Match either Regexp, try left first             #co
  Pces     ::= Pce                Match Pce                                       #co
            |  PcePces            Match Pce followed by Pces                      #co
  Pce      ::= Repeat             Match Repeat, longest possible                  #co
            |  Repeat?            Match Repeat, shortest possible                 #co
            |  Atom               Match Atom exactly once                         #co
  Repeat   ::= Atom*              Match Atom 0 or more times                      #co
            |  Atom+              Match Atom 1 or more times                      #co
            |  Atom?              Match Atom 0 or 1 times                         #co
  Repeat   ::= ...                ...                                             #px
            |  Atom{N}            Match Atom exactly N times                      #px
            |  Atom{N,}           Match Atom N or more times                      #px
            |  Atom{,M}           Match Atom between 0 and M times                #px
            |  Atom{N,M}          Match Atom between N and M times                #px
  Atom     ::= (Regexp)           Match sub-expression Regexp and report          #co
            |  [Rng]              Match any character in Rng                      #co
            |  [^Rng]             Match any character not in Rng                  #co
            |  .                  Match any (except newline in multi mode)        #co
            |  ^                  Match start (or after newline in multi mode)    #co
            |  $                  Match end (or before newline in multi mode)     #co
            |  Literal            Match a single literal character                #co
            |  (?Mode:Regexp)     Match Regexp using Mode                         #co
            |  (?>Regexp)         Match Regexp, only first possible               #co
            |  Look               Match empty if Look matches                     #co
            |  (?TstPces|Pces)    Match 1st Pces if Tst, else 2nd Pces            #co
            |  (?TstPces)         Match Pces if Tst, empty if not Tst             #co
  Atom     ::= ...                ...                                             #px
            |  \N                 Match latest reported match for N##th _(_       #px
            |  Class              Match any character in Class                    #px
            |  \b                 Match _\w*_ boundary                            #px
            |  \B                 Match where _\b_ does not                       #px
            |  \p{Property}       Match (UTF-8 encoded) in Property               #px
            |  \P{Property}       Match (UTF-8 encoded) not in Property           #px
  Literal  :== Any character except _(_, _)_, _*_, _+_, _?_, _[_, _._, _^_, _\_, or _|_                #rx
  Literal  :== Any character except _(_, _)_, _*_, _+_, _?_, _[_, _]_, _{_, _}_, _._, _^_, _\_, or _|_ #px
            |  \Aliteral                Match Aliteral                            #ot
  Aliteral :== Any character                                                      #rx
  Aliteral :== Any character except _a_-_z_, _A_-_Z_, _0_-_9_                     #px
  Rng      ::= ]                  Rng contains _]_ only                           #co
            |  -                  Rng contains _-_ only                           #co
            |  Mrng               Rng contains everything in Mrng                 #co
            |  Mrng-              Rng contains _-_ and everything in Mrng         #co
  Mrng     ::= ]Lrng              Mrng contains _]_ and everything in Lrng        #co
            |  -Lrng              Mrng contains _-_ and everything in Lrng        #co
            |  Lirng              Mrng contains everything in Lirng               #co
  Lirng    ::= Riliteral          Lirng contains a literal character              #co
            |  Riliteral-Rliteral Lirng contains Unicode range inclusive          #co
            |  LirngLrng          Lirng contains everything in both               #co
  Lrng     ::= ^                  Lrng contains _^_                               #co
            |  Rliteral-Rliteral  Lrng contains Unicode range inclusive           #co
            |  ^Lrng              Lrng contains _^_ and more                      #co
            |  Lirng              Lrng contains everything in Lirng               #co
  Look     ::= (?=Regexp)         Match if Regexp matches                         #mode
            |  (?!Regexp)         Match if Regexp doesn't match                   #mode
            |  (?<=Regexp)        Match if Regexp matches preceding               #mode
            |  (?<!Regexp)        Match if Regexp doesn't match preceding         #mode
  Tst      ::= (N)                True if Nth _(_ has a match                     #mode
            |  Look               True if Look matches                            #mode
  Lirng    ::= ...                ...                                             #px
            |  Class              Lirng contains all characters in Class          #px
            |  Posix              Lirng contains all characters in Posix          #px
            |  \Eliteral          Lirng contains Eliteral                         #px
  Riliteral :== Any character except _]_, _-_, or _^_                             #rx
  Riliteral :== Any character except _]_, _\_, _-_, or _^_                        #px
  Rliteral  :== Any character except _]_ or _-_                                   #rx
  Rliteral  :== Any character except _]_, _\_, or _-_                             #px
  Eliteral  :== Any character except _a_-_z_, _A_-_Z_                             #px
  Mode     ::=                    Like the enclosing mode                         #mode
            |  Modei              Like Mode, but case-insensitive                 #mode
            |  Mode-i             Like Mode, but sensitive                        #mode
            |  Modes              Like Mode, but not in multi mode                #mode
            |  Mode-s             Like Mode, but in multi mode                    #mode
            |  Modem              Like Mode, but in multi mode                    #mode
            |  Mode-m             Like Mode, but not in multi mode                #mode
  Class    ::= \d                 Contains _0_-_9_                                #cat
            |  \D                 Contains ASCII other than those in _\d_         #cat
            |  \w                 Contains _a_-_z_, _A_-_Z_, _0_-_9_, ___         #cat
            |  \W                 Contains ASCII other than those in _\w_         #cat
            |  \s                 Contains space, tab, newline, formfeed, return  #cat
            |  \S                 Contains ASCII other than those in _\s_         #cat
  Posix    ::= [:alpha:]          Contains _a_-_z_, _A_-_Z_                       #cat
            |  [:alnum:]          Contains _a_-_z_, _A_-_Z_, _0_-_9_              #cat
            |  [:ascii:]          Contains all ASCII characters                   #cat
            |  [:blank:]          Contains space and tab                          #cat
            |  [:cntrl:]          Contains all characters with scalar value < 32  #cat
            |  [:digit:]          Contains _0_-_9_                                #cat
            |  [:graph:]          Contains all ASCII characters that use ink      #cat
            |  [:lower:]          Contains space, tab, and ASCII ink users        #cat
            |  [:print:]          Contains _A_-_Z_                                #cat
            |  [:space:]          Contains space, tab, newline, formfeed, return  #cat
            |  [:upper:]          Contains _A_-_Z_                                #cat
            |  [:word:]           Contains _a_-_z_, _A_-_Z_, _0_-_9_, ___         #cat
            |  [:xdigit:]         Contains _0_-_9_, _a_-_f_, _A_-_F_              #cat
  Property ::= Category           Includes all characters in Category             #cat
            |  ^Category          Includes all characters not in Category         #cat
  Category ::= Ll                 Letter, lowercase                               #ucat
            |  Lu                 Letter, uppercase                               #ucat
            |  Lt                 Letter, titlecase                               #ucat
            |  Lm                 Letter, modifier                                #ucat
            |  L&                 Union of _Ll_, _Lu_, _Lt_, and _Lm_             #ucat
            |  Lo                 Letter, other                                   #ucat
            |  L                  Union of _L&_ and _Lo_                          #ucat
            |  Nd                 Number, decimal digit                           #ucat
            |  Nl                 Number, letter                                  #ucat
            |  No                 Number, other                                   #ucat
            |  NN                 Union of _Nd_, _Nl_, and _No_                   #ucat
            |  Ps                 Punctuation, open                               #ucat
            |  Pe                 Punctuation, close                              #ucat
            |  Pi                 Punctuation, initial quote                      #ucat
            |  Pf                 Punctuation, final quote                        #ucat
            |  Pc                 Punctuation, connector                          #ucat
            |  Pd                 Punctuation, dash                               #ucat
            |  Po                 Punctuation, other                              #ucat
            |  P                  Union of _Ps_, _Pe_, _Pi_, _Pf_, _Pc_, _Pd_, and _Po_ #ucat
            |  Mn                 Mark, non-spacing                               #ucat
            |  Mc                 Mark, spacing combining                         #ucat
            |  Me                 Mark, enclosing                                 #ucat
            |  MM                 Union of _Mn_, _Mc_, and _Me_                   #ucat
            |  Sc                 Symbol, currency                                #ucat
            |  Sk                 Symbol, modifier                                #ucat
            |  Sm                 Symbol, math                                    #ucat
            |  So                 Symbol, other                                   #ucat
            |  S                  Union of _Sc_, _Sk_, _Sm_, and _So_             #ucat
            |  Zl                 Seaprator, line                                 #ucat
            |  Zp                 Seaparator, paragraph                           #ucat
            |  Zs                 Separator, space                                #ucat
            |  Z                  Union of _Zl_, _Zp_, and _Zs_                   #ucat
            |  Cc                 Other, control                                  #ucat
            |  Cf                 Other, format                                   #ucat
            |  Cs                 Other, surrogate                                #ucat
            |  Cn                 Other, not assigned                             #ucat
            |  Co                 Other, private use                              #ucat
            |  C                  Union of _Cc_, _Cf_, _Cs_, _Cn_, and _Co_       #ucat
            |  .                  Union of all Unicode categories                 #ucat
  })

(define-syntax regexp-case
  (syntax-rules (else)
    [(regexp-case str) (void)]
    [(regexp-case str [else b ...]) (let () b ...)]
    [(regexp-case str [(re v ...) b ...] . more)
     (let* ([m str] [m (and (string? m) (regexp-match re m))])
       (if m (apply (lambda (v ...) b ...) (cdr m))
             (regexp-case str . more)))]))

(define re:nonterm
  (let* ([xs (regexp-match* #px"(\n|^) *\\w+ +:[:=]= " grammar)]
         [xs (map (lambda (m) (car (regexp-match #px"\\w+" m))) xs)]
         [xs (string-join (remove-duplicates xs) "|")])
    (pregexp (string-append "^(.*?)("xs"|\\b[MN]\\b)(.*)$"))))

(define (fixup-ids s)
  (regexp-case s
    [(re:nonterm X N Y)
     `(,@(fixup-ids X) ,(nonterm (string-downcase N)) ,@(fixup-ids Y))]
    [(#rx"^(.*?) [|] (.*)$" X Y)
     `(,@(fixup-ids X) ,spacer ,(tt "|") ,spacer ,@(fixup-ids Y))]
    [(#rx"^(.*?)(MM|NN)(.*)$" X CH Y)
     `(,@(fixup-ids X) ,(substring CH 0 1) ,@(fixup-ids Y))]
    [(#rx"^(.*?)##(.*)$" X Y)
     `(,@(fixup-ids X) ,@(fixup-ids Y))]
    [(#rx"^\\.\\.\\.$") (list (element #f (list s)))]
    [(#rx"^$") null]
    [else (list s)]))

(define (lit-ize l)
  (map (lambda (i) (if (string? i) (litchar i) i)) l))

(define (as-meaning l)
  (map (lambda (s)
         (let loop ([s s])
           (regexp-case s
             [(#rx"^(.*?)_([^_]+|_)_(.*)$" X L Y)
              (element #f (list (loop X) (litchar L) (loop Y)))]
             [else s])))
       l))

(define (as-smaller l)
  (list (element "smaller" l)))

(define spacer (hspace 1))
(define ::=    (element #f (list spacer (tt "::=") spacer)))
(define -or-   (tt "|"))

(define grammar-lines
  (for/list ([line (in-list (regexp-split "\r*\n" grammar))]
             #:when (positive? (string-length line)))
    (regexp-case line
      [(#px"^(.*?) +#(\\w+)$" line kind) (cons (string->symbol kind) line)]
      [else (error 'grammar-lines "bad line: ~s" line)])))

(define (table-content modes)
  (define (cell x)
    (if (eq? x 'cont)
      x
      (paragraph plain (list (if (element? x) x (element #f x))))))
  (define (row . xs) (map cell xs))
  (define (render-line line)
    (regexp-case line
      [(#rx"^([^ ]*) +::= ((?:[^ ]+| [|] )*) +([^ ].*)$" prod val meaning)
       (row (fixup-ids prod) ::= (lit-ize (fixup-ids val))
            spacer (as-smaller (as-meaning (fixup-ids meaning))))]
      [(#rx"^([^ ]*) +:== (.*)$" prod meaning)
       (row (fixup-ids prod) ::= (as-meaning (fixup-ids meaning))
            'cont 'cont)]
      [(#rx"^ + [|]  ((?:[^ ]| [|] )*) +([^ ].*)$" val meaning)
       (row 'nbsp -or- (lit-ize (fixup-ids val))
            spacer (as-smaller (as-meaning (fixup-ids meaning))))]))
  (table (style #f (list (table-columns
                          (map (lambda (s) (style #f (list s)))
                               '(left left center left left left)))))
    (for/list ([line (in-list grammar-lines)] #:when (memq (car line) modes))
      (cons (paragraph plain (list spacer)) (render-line (cdr line))))))

(provide common-table rx-table px-table category-table)
(define common-table   (table-content '(co mode)))
(define rx-table       (table-content '(rx ot)))
(define px-table       (table-content '(px ot cat)))
(define category-table (table-content '(ucat)))

;; ----------------------------------------------------------------------

(define types @string-append{
  Regexp_1|Regexp_2 : <min(n1, n2),max(m1, m2)> @;
    iff Regexp_1 : <n1,m1> AND Regexp_2 : <n2,m2>

  PcePces : <n1+n2,m1+m2> iff Pce : <n1,m1> AND Pces : <n2,m2>

  Repeat? : <0,m> iff Repeat : <n,m>
  Atom* : <0,+inf.0> iff Atom : <n,m> AND n > 0

  Atom+ : <1,+inf.0> iff Atom : <n,m> AND n > 0
  Atom? : <0,m> iff Atom : <n,m>

  Atom{N} : <n*N,m*N> iff Atom : <n,m> AND n > 0

  Atom{N,} : <n*N,+inf.0> iff Atom : <n,m> AND n > 0

  Atom{,M} : <0,m*M> iff Atom : <n,m> AND n > 0

  Atom{N,M} : <n*N,m*M> iff Atom : <n,m> AND n > 0

  (Regexp) : <n,m> AND \alpha_N=n iff Regexp : <n,m>

  (?Mode:Regexp) : <n,m> iff Regexp : <n,m>

  (?=Regexp) : <0,0> iff Regexp : <n,m>
  (?!Regexp) : <0,0> iff Regexp : <n,m>

  (?<=Regexp) : <0,0> iff Regexp : <n,m> AND m < +inf.0
  (?<!Regexp) : <0,0> iff Regexp : <n,m> AND m < +inf.0

  (?>Regexp) : <n,m> iff Regexp : <n,m>

  (?TstPces_1|Pces_2) : <min(n1, n2),max(m1, m2)> @;
    iff Tst : <n0,m0> AND Pces_1 : <n1,m1> AND Pces_2 : <n2,m2>

  (?TstPces) : <0,m1> iff Tst : <n0,m0> AND Pces : <n1,m1>

  (N) : <\alpha_N,+inf.0>
  [Rng] : <1,1>
  [^Rng] : <1,1>

  . : <1,1>
  ^ : <0,0>
  $ : <0,0>

  Literal : <1,1>
  \N : <\alpha_N,+inf.0>
  Class : <1,1>

  \b : <0,0>
  \B : <0,0>

  \p{Property} : <1,6>
  \P{Property} : <1,6>})

(define (subscripts i)
  (regexp-case i
    [(#rx"^(.*)_(.)(.*)$" X S Y)
     `(,@(subscripts X) ,(element 'subscript (list S)) ,@(subscripts Y))]
    [(#rx"^(.*)([nm])([012]?)(.*)$" X V N Y)
     `(,@(subscripts X)
       ,(element 'italic (list V)) ,(element 'subscript (list N))
       ,@(subscripts Y))]
    [else (list i)]))

(define (meta i)
  (regexp-case i
    [(#rx"^(.*)(min|max)(.*)$" X M Y)
     `(,@(meta X) ,(element #f (list M)) ,@(meta Y))]
    [(#rx"^(.*)[+]inf[.]0(.*)$" X Y) `(,@(meta X) infin ,@(meta Y))]
    [(#rx"^(.*)[\\]alpha(.*)$"  X Y) `(,@(meta X) alpha ,@(meta Y))]
    [else (list i)]))

(define (fixup-one-type t)
  (let ([t (regexp-replace* #rx"<([^(,]*|[^,]*[(].*[)][^,]*),([^>]*)>"
                            t "[\\1, \\2]")])
    (append-map subscripts (append-map meta (fixup-ids t)))))

(define (fixup-type t)
  (regexp-case t
    [(#rx"^(.*?) AND (.*)$" X Y)
     `(,@(fixup-type X) ,(hspace 3) ,@(fixup-type Y))]
    [(#rx"^(.*?) : (.*)$" X Y)
     `(,@(lit-ize (append-map subscripts (fixup-ids X)))
       ,spacer ,(tt ":") ,spacer ,@(fixup-one-type Y))]
    [else (fixup-one-type t)]))

(provide type-table)
(define type-table
  (let ()
    (define rule-style
      (list (table-cells (list (list (style "inferencetop" '(center)))
                               (list (style "inferencebottom" '(center)))))))
    (define do-clauses
      (case-lambda
        [(bottom top)
         (table (style #f rule-style)
                (list (list (paragraph plain `(,spacer ,@top ,spacer)))
                      (list (paragraph plain `(,spacer ,@bottom ,spacer)))))]
        [(single) (paragraph plain single)]))
    (define (do-line line)
      (apply do-clauses (map fixup-type (regexp-split #rx" iff " line))))
    (define (do-row para)
      (add-between (map do-line (regexp-split #rx" *\r*\n" para))
                   (paragraph plain (list (hspace 3)))))
    (define (do-para para) (list (table plain (list (do-row para)))))
    (table (style #f (list (table-columns (list (style #f '(center))))))
      (add-between (map do-para (regexp-split #px"\r*\n(?: *\r*\n)+" types))
                   (list (paragraph plain (list spacer)))))))
