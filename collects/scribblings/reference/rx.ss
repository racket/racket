(module rx scheme/base
  (require scribble/core
           scribble/manual
           scribble/bnf)

  (define grammar "
Regexp   ::= Pieces                   Match Pieces                                                 #co
          |  Regexp|Regexp            Match either Regexp, try left first                          #co
Pieces   ::= Piece                    Match Piece                                                  #co
          |  PiecePieces              Match Piece followed by Pieces                               #co
Piece    ::= Repeat                   Match Repeat, longest possible                               #co
          |  Repeat?                  Match Repeat, shortest possible                              #co
          |  Atom                     Match Atom exactly once                                      #co
Repeat   ::= Atom*                    Match Atom 0 or more times                                   #co
          |  Atom+                    Match Atom 1 or more times                                   #co
          |  Atom?                    Match Atom 0 or 1 times                                      #co
Repeat   ::= ...                      ...                                                          #px
          |  Atom{N}                  Match Atom exactly N times                                   #px
          |  Atom{N,}                 Match Atom N or more times                                   #px
          |  Atom{,M}                 Match Atom between 0 and M times                             #px
          |  Atom{N,M}                Match Atom between N and M times                             #px
Atom     ::= (Regexp)                 Match sub-expression Regexp and report                       #co
          |  [Range]                  Match any character in Range                                 #co
          |  [^Range]                 Match any character not in Range                             #co
          |  .                        Match any (except newline in multi mode)                     #co
          |  ^                        Match start (or after newline in multi mode)                 #co
          |  $                        Match end (or before newline in multi mode)                  #co
          |  Literal                  Match a single literal character                             #co
          |  (?Mode:Regexp)           Match Regexp using Mode                                      #co
          |  (?>Regexp)               Match Regexp, only first possible                            #co
          |  Look                     Match empty if Look matches                                  #co
          |  (?PredPieces|Pieces)     Match 1st Pieces if Pred, else 2nd Pieces                    #co
          |  (?PredPieces)            Match Pieces if Pred, empty if not Pred                      #co
Atom     ::= ...                      ...                                                          #px
          |  %N                       Match latest reported match for N##th _(_                    #px
          |  Class                    Match any character in Class                                 #px
          |  %b                       Match %w* boundary                                           #px
          |  %B                       Match where %b does not                                      #px
          |  %p{Property}             Match (UTF-8 encoded) in Property                            #px
          |  %P{Property}             Match (UTF-8 encoded) not in Property                        #px
Literal  :== Any character except _(_, _)_, _*_, _+_, _?_, _[_, _._, _^_, _\\_, or _|_             #rx
Literal  :== Any character except _(_, _)_, _*_, _+_, _?_, _[_, _]_, _{_, _}_, _._, _^_, _\\_, or _|_ #px
          |  \\Aliteral                Match Aliteral                                              #ot
Aliteral :== Any character                                                                         #rx
Aliteral :== Any character except _a_-_z_, _A_-_Z_, _0_-_9_                                        #px
Range    ::= ]                        Range contains _]_ only                                      #co
          |  -                        Range contains _-_ only                                      #co
          |  Mrange                   Range contains everything in Mrange                          #co
          |  Mrange-                  Range contains _-_ and everything in Mrange                  #co
Mrange   ::= ]Lrange                  Mrange contains _]_ and everything in Lrange                 #co
          |  -Lrange                  Mrange contains _-_ and everything in Lrange                 #co
          |  Srange                   Mrange contains everything in Srange                         #co
Srange   ::= Sliteral                 Srange contains a literal character                          #co
          |  Sliteral-Rliteral        Srange contains Unicode range inclusive                      #co
          |  SrangeLrange             Srange contains everything in both                           #co
Lrange   ::= ^                        Lrange contains _^_                                          #co
          |  Rliteral-Rliteral        Lrange contains Unicode range inclusive                      #co
          |  ^Lrange                  Lrange contains _^_ and more                                 #co
          |  Srange                   Lrange contains everything in Srange                         #co
Look     ::= (?=Regexp)               Match if Regexp matches                                      #mode
          |  (?!Regexp)               Match if Regexp doesn't match                                #mode
          |  (?<=Regexp)              Match if Regexp matches preceding                           #mode
          |  (?<!Regexp)              Match if Regexp doesn't match preceding                     #mode
Pred     ::= (N)                      True if Nth _(_ has a match                                  #mode
          |  Look                     True if Look matches                                         #mode
Srange   ::= ...                      ...                                                          #px
          |  Class                    Srange contains all characters in Class                      #px
          |  Posix                    Srange contains all characters in Posix                      #px
          |  \\Eliteral               Srange contains Eliteral                                     #px
Sliteral :== Any character except _]_, _-_, or _^_                                                 #rx
Sliteral :== Any character except _]_, _\\_, _-_, or _^_                                           #px
Rliteral :== Any character except _]_ or _-_                                                       #rx
Rliteral :== Any character except _]_, _\\_, or _-_                                                #px
Eliteral :== Any character except _a_-_z_, _A_-_Z_                                                 #px
Mode     ::=                          Like the enclosing mode                                      #mode
          |  Modei                    Like Mode, but case-insensitive                              #mode
          |  Mode-i                   Like Mode, but sensitive                                     #mode
          |  Modes                    Like Mode, but not in multi mode                             #mode
          |  Mode-s                   Like Mode, but in multi mode                                 #mode
          |  Modem                    Like Mode, but in multi mode                                 #mode
          |  Mode-m                   Like Mode, but not in multi mode                             #mode
Class    ::= %d                       Contains _0_-_9_                                             #cat
          |  %D                       Contains ASCII other than those in %d                        #cat
          |  %w                       Contains _a_-_z_, _A_-_Z_, _0_-_9_, ___                      #cat
          |  %W                       Contains ASCII other than those in %w                        #cat
          |  %s                       Contains space, tab, newline, formfeed, return               #cat
          |  %S                       Contains ASCII other than those in %s                        #cat
Posix    ::= [:alpha:]                Contains _a_-_z_, _A_-_Z_                                    #cat
          |  [:alnum:]                Contains _a_-_z_, _A_-_Z_, _0_-_9_                           #cat
          |  [:ascii:]                Contains all ASCII characters                                #cat
          |  [:blank:]                Contains space and tab                                       #cat
          |  [:cntrl:]                Contains all characters with scalar value < 32               #cat
          |  [:digit:]                Contains _0_-_9_                                             #cat
          |  [:graph:]                Contains all ASCII characters that use ink                   #cat
          |  [:lower:]                Contains space, tab, and ASCII ink users                     #cat
          |  [:print:]                Contains _A_-_Z_                                             #cat
          |  [:space:]                Contains space, tab, newline, formfeed, return               #cat
          |  [:upper:]                Contains _A_-_Z_                                             #cat
          |  [:word:]                 Contains _a_-_z_, _A_-_Z_, _0_-_9_, ___                      #cat
          |  [:xdigit:]               Contains _0_-_9_, _a_-_f_, _A_-_F_                           #cat
Property ::= Category                 Includes all characters in Category                          #cat
          |  ^Category                Includes all characters not in Category                      #cat
Category ::= Ll | Lu | Lt | Lm        Unicode general category                                     #cat
          |  L&                       Union of Ll, Lu, Lt, and Lm                                  #cat
          |  Lo                       Unicode general category                                     #cat
          |  L                        Union of L& and Lo                                           #cat
          |  Nd | Nl | No             Unicode general category                                     #cat
          |  NN                       Union of Nd, Nl, and No                                      #cat
          |  Ps | Pe | Pi | Pf        Unicode general category                                     #cat
          |  Pc | Pd | Po             Unicode general category                                     #cat
          |  P                        Union of Ps, Pe, Pi, Pf, Pc, Pd, and Po                      #cat
          |  Mn | Mc | Me             Unicode general category                                     #cat
          |  MM                       Union of Mn, Mc, and Me                                      #cat
          |  Sc | Sk | Sm | So        Unicode general category                                     #cat
          |  S                        Union of Sc, Sk, Sm, and So                                  #cat
          |  Zl | Zp | Zs             Unicode general category                                     #cat
          |  Z                        Union of Zl, Zp, and Zs                                      #cat
          |  .                        Union of all general categories                              #cat
")

  (define (subs s)
    (cond
     [(equal? s "piece") "pce"]
     [(equal? s "pieces") "pces"]
     [(equal? s "range") "rng"]
     [(equal? s "mrange") "mrng"]
     [(equal? s "lrange") "lrng"]
     [(equal? s "srange") "lirng"]
     [(equal? s "sliteral") "riliteral"]
     [(equal? s "pred") "tst"]
     [else s]))

  (define (fixup-ids s)
    (let loop ([m (regexp-match-positions
                   #px"(Regexp)|(Pieces?)|(Atom)|(Repeat)|(Literal)|(Aliteral)|(Eliteral)|(Range)|(Srange)|(Lrange)|(Mrange)|(Sliteral)|(Rliteral)|(Mode)|(Class)|(Posix)|(Property)|(Category)|(Pred)|(Look)|(\\bN\\b)|(\\bM\\b)"
                   s)])
      (cond
       [m
        (append (fixup-ids (substring s 0 (caar m)))
                (list (nonterm (subs (string-downcase (substring s (caar m) (cdar m))))))
                (fixup-ids (substring s (cdar m))))]
       [(regexp-match-positions #rx" [|] " s)
        => (lambda (m)
             (append (fixup-ids (substring s 0 (caar m)))
                     (list spacer (tt "|") spacer)
                     (fixup-ids (substring s (cdar m)))))]
       [(regexp-match-positions #rx"MM|NN" s)
        => (lambda (m)
             (append (fixup-ids (substring s 0 (caar m)))
                     (list (substring s (caar m) (add1 (caar m))))
                     (fixup-ids (substring s (cdar m)))))]
       [(regexp-match-positions #rx"##" s)
        => (lambda (m)
             (append (fixup-ids (substring s 0 (caar m)))
                     (fixup-ids (substring s (cdar m)))))]
       [(string=? s "...") (list (make-element #f (list s)))]
       [(string=? s "") null]
       [else (list (regexp-replace* #rx"%" s "\\\\"))])))

  (define (lit-ize l)
    (map (lambda (i)
           (if (string? i)
               (litchar i)
               i))
         l))

  (define (as-meaning l)
    (map (lambda (s)
           (let loop ([s s])
             (cond
              [(and (string? s)
                    (regexp-match #rx"^(.*?)_([^_]+|_)_(.*)$" s))
               => (lambda (m)
                    (make-element #f (list (loop (cadr m))
                                           (litchar (caddr m))
                                           (loop (cadddr m)))))]
              [else s])))
         l))

  (define (as-smaller l)
    (list (make-element "smaller" l)))

  (define spacer (hspace 1))
  (define ::= (make-element #f (list (hspace 1) (tt "::=") spacer)))
  (define -or- (tt "|"))
  
  (define (table-content mode)
    (define re:output (regexp (format " *#~a$" mode)))
    (define re:start-prod "^([^ ]*)( +)::= ((?:[^ ]| [|] )*)( +)([^ ].*)$")
    (define re:or-prod "^( +) [|]  ((?:[^ ]| [|] )*)( +)([^ ].*)$")
    (define re:eng-prod "^([^ ]*)( +):== (.*)$")
    
    (define lines 
      (filter
       values
       (map (lambda (s)
              (cond
               [(regexp-match-positions re:output s)
                => (lambda (m)
                     (substring s 0 (caar m)))]
               [(regexp-match-positions #rx" *#[a-z]+$" s)
                #f]
	       [(equal? s "") #f]
	       [else
		(error 'lines "no match!?: ~s" s)]))
            (regexp-split "\r*\n" grammar))))

    (define table-lines
      (map
       (lambda (line)
         (cond
          [(regexp-match re:start-prod line)
           => (lambda (m)
                (let ([prod (list-ref m 1)]
                      [val (list-ref m 3)]
                      [meaning (list-ref m 5)])
                  (list (make-element #f (fixup-ids prod))
                        ::=
                        (make-element #f (lit-ize (fixup-ids val)))
                        spacer
                        (make-element #f (as-smaller (as-meaning (fixup-ids meaning)))))))]
          [(regexp-match re:eng-prod line)
           => (lambda (m)
                (let ([prod (list-ref m 1)]
                      [meaning (list-ref m 3)])
                  (list (make-element #f (fixup-ids prod))
                        ::=
                        (make-element #f (as-meaning (fixup-ids meaning)))
                        'cont
                        'cont)))]
          [(regexp-match re:or-prod line)
           => (lambda (m)
                (let ([val (list-ref m 2)]
                      [meaning (list-ref m 4)])
                  (list 'nbsp
                        -or-
                        (make-element #f (lit-ize (fixup-ids val)))
                        spacer
                        (make-element #f (as-smaller (as-meaning (fixup-ids meaning)))))))]))
       lines))

    (make-table
     (make-style #f (list (make-table-columns (map (lambda (s)
                                                     (make-style #f (list s)))
                                                   '(left left center left left left)))))
     (map (lambda (line)
            (cons (make-paragraph plain (list (hspace 1)))
                  (map (lambda (i)
                         (if (eq? i 'cont)
                             i
                             (make-paragraph plain (list i))))
                       line)))
          table-lines)))

  (define common-table
    (table-content "(co|mode)"))
  (define rx-table
    (table-content "(?:rx|ot)"))
  (define px-table
    (table-content "(?:px|ot|cat)"))

  (provide common-table
           rx-table
           px-table)

  ;; ----------------------------------------------------------------------

  (define types "
Regexp_1|Regexp_2 : <min(n1, n2),max(m1, m2)> iff Regexp_1 : <n1,m1> AND Regexp_2 : <n2,m2>

PiecePieces : <n1+n2,m1+m2> iff Piece : <n1,m1> AND Pieces : <n2,m2>

Repeat? : <0,m> iff Repeat : <n,m>
Atom* : <0,+inf.0> iff Atom : <n,m> AND n > 0

Atom+ : <1,+inf.0> iff Atom : <n,m> AND n > 0
Atom? : <0,m> iff Atom : <n,m>

Atom{N} : <n*N,m*N> iff Atom : <n,m> AND n > 0

Atom{N,} : <n*N,+inf.0> iff Atom : <n,m> AND n > 0

Atom{,M} : <0,m*M> iff Atom : <n,m> AND n > 0

Atom{N,M} : <n*N,m*M> iff Atom : <n,m> AND n > 0

(Regexp) : <n,m> AND \\alpha_N=n iff Regexp : <n,m>

(?Mode:Regexp) : <n,m> iff Regexp : <n,m>

(?=Regexp) : <0,0> iff Regexp : <n,m>
(?!Regexp) : <0,0> iff Regexp : <n,m>

(?<=Regexp) : <0,0> iff Regexp : <n,m> AND m < +inf.0
(?<!Regexp) : <0,0> iff Regexp : <n,m> AND m < +inf.0

(?>Regexp) : <n,m> iff Regexp : <n,m>

(?PredPieces_1|Pieces_2) : <min(n1, n2),max(m1, m2)> iff Pred : <n0,m0> AND Pieces_1 : <n1,m1> AND Pieces_2 : <n2,m2>

(?PredPieces) : <0,m1> iff Pred : <n0,m0> AND Pieces : <n1,m1>

(N) : <\\alpha_N,+inf.0>
[Range] : <1,1>
[^Range] : <1,1>

. : <1,1>
^ : <0,0>
$ : <0,0>

Literal : <1,1>
%N : <\\alpha_N,+inf.0>
Class : <1,1>

%b : <0,0>
%B : <0,0>

%p{Property} : <1,6>
%P{Property} : <1,6>")

  (define (subscripts i)
   (cond
    [(and (string? i)
          (regexp-match #rx"^(.*)_(.)(.*)$" i))
     => (lambda (m)
          (append
           (subscripts (cadr m))
           (list (make-element 'subscript (list (caddr m))))
           (subscripts (cadddr m))))]
    [(and (string? i)
          (regexp-match #rx"^(.*)([nm])([012]?)(.*)$" i))
     => (lambda (m)
          (append
           (subscripts (cadr m))
           (list (make-element 'italic (list (caddr m)))
                 (make-element 'subscript (list (cadddr m))))
           (subscripts (cadddr (cdr m)))))]
    [else (list i)]))

  (define (meta i)
    (cond
     [(and (string? i)
           (regexp-match #rx"^(.*)(min|max)(.*)$" i))
      => (lambda (m)
           (append (meta (cadr m))
                   (list (make-element #f (list (caddr m))))
                   (meta (cadddr m))))]
     [(and (string? i)
           (regexp-match #rx"^(.*)([+]inf[.]0)(.*)$" i))
      => (lambda (m)
           (append (meta (cadr m))
                   (list 'infin)
                   (meta (cadddr m))))]
     [(and (string? i)
           (regexp-match #rx"^(.*)([\\]alpha)(.*)$" i))
      => (lambda (m)
           (append (meta (cadr m))
                   (list 'alpha)
                   (meta (cadddr m))))]
     [else (list i)]))

  (define (fixup-one-type t) 
   (apply
    append
    (map subscripts
         (apply
          append
          (map meta (fixup-ids (regexp-replace* #rx"<([^(,]*|[^,]*[(].*[)][^,]*),([^>]*)>" t "[\\1, \\2]")))))))

  (define (fixup-type t) 
   (cond
    [(regexp-match-positions #rx" AND " t)
     => (lambda (m)
          (append (fixup-type (substring t 0 (caar m)))
                  (list (hspace 3))
                  (fixup-type (substring t (cdar m)))))]
    [(regexp-match-positions #rx" : " t)
     => (lambda (m)
          (append (lit-ize (apply append (map subscripts (fixup-ids (substring t 0 (caar m))))))
                  (list spacer (tt ":") spacer)
                  (fixup-one-type (substring t (cdar m)))))]
    [else (fixup-one-type t)]))

  (define (insert i l)
    (cond
     [(null? l) null]
     [(null? (cdr l)) l]
     [else (list* (car l) i (insert i (cdr l)))]))

  (define type-table
    (let* ([lines (regexp-split "\r*\n" types)]
           [lines (let loop ([lines lines])
                   (if (null? lines)
                       null
                       (let ([line (car lines)])
                         (if (equal? line "")
                             (cons null (loop (cdr lines)))
                             (let ([r (loop (cdr lines))])
                               (if (null? r)
                                   (list (list line))
                                   (cons (cons line (car r)) (cdr r))))))))])
      (make-table
       (make-style #f (list (make-table-columns (list (make-style #f '(center))))))
       (insert
       (list (make-paragraph plain (list spacer)))
       (map (lambda (line)
             (list
                (make-table
                 plain
                 (list
                  (insert
                  (make-paragraph plain (list (hspace 3)))
                  (map (lambda (line)
                           (call-with-values (lambda ()
                                               (apply values (regexp-split " iff " line)))
                             (case-lambda
                              [(bottom top)
                               (make-table
                                (make-style #f
                                 (list
                                  (make-table-cells (list (list (make-style "inferencetop" '(center)))
                                                          (list (make-style "inferencebottom" '(center)))))))
                                (list
                                 (list (make-paragraph plain (append (list spacer) (fixup-type top) (list spacer))))
                                 (list (make-paragraph plain (append (list spacer) (fixup-type bottom) (list spacer))))))]
                              [(single)
                               (make-paragraph plain (fixup-type line))])))
                       line))))))
            lines)))))

  (provide type-table))
