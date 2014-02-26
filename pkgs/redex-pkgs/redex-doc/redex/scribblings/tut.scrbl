#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval
          racket/runtime-path
          scriblib/autobib
          (for-syntax racket/base 
                      setup/path-to-relative 
                      setup/main-collects)
          setup/dirs
          "tut-util.rkt"
          (for-label racket/base
                     racket/gui
                     racket/pretty
                     racket/contract
                     pict
                     mrlib/graph
                     redex))

@(define amb-eval (make-base-eval))
@(interaction-eval #:eval amb-eval (require racket
                                            redex/reduction-semantics 
                                            redex/pict
                                            pict
                                            racket/pretty))
@(interaction-eval #:eval amb-eval (begin (pretty-print-columns 40) (random-seed 0)))

@(define orig-op (current-output-port))
@(define orig-ep (current-error-port))

@; evaluates its argument in the amb-eval, but sets
@; the output to go the real stdout & stdin; use this
@; to add extra test cases that don't get rendered
@; in the file (but that drdr can catch the output of)
@(define-syntax-rule
   (amb-try e ...)
   (amb-eval `(parameterize ([current-output-port ,orig-op]
                             [current-error-port ,orig-ep])
                ,'e ...)))

@; (interaction/test exp passed?) ==> renders as interaction would, 
@; but also checks to see if 'passed?'
@; holds for the result of 'exp'
@(define-for-syntax (loc stx)
   (let ([src (syntax-source stx)])
     (if (path? src)
         (path->relative-string/library src)
         #f)))
@(define-syntax (interaction/test stx)
   (syntax-case stx ()
     [(_ exp passed?)
      (with-syntax ([line (syntax-line stx)]
                    [src (loc stx)])
        #'(begin (amb-try (interaction/test/proc passed? exp src line))
                 (interaction #:eval amb-eval exp)))]))
@(amb-eval '(define (interaction/test/proc passed? val src line)
              (unless (passed? val)
                (eprintf "~a: test case on line ~a failed\n  ~s\n  pred: ~s\n" 
                         src line
                         val
                         (contract-name passed?)))))

@; just like (amb/test exp passed?), except it does not 
@; render the result
@(define-syntax (amb/test stx)
   (syntax-case stx ()
     [(_ exp passed?)
      (with-syntax ([line (syntax-line stx)]
                    [src (loc stx)])
        #'(begin (amb-try (interaction/test/proc passed? exp src line))
                 ""))]))

@(define-syntax (check-output stx)
   (syntax-case stx ()
     [(_ exp reg)
      (with-syntax ([line (syntax-line stx)]
                    [src (loc stx)])
        #'(check-output/proc 'exp reg line src))]))
@(define (check-output/proc exp reg line src)
   (define sp (open-output-string))
   (amb-eval `(parameterize ([current-output-port ,sp])
                ,exp))
   (define got (get-output-string sp))
   (unless (regexp-match reg got)
     (eprintf "~a: test case on line ~a failed\n  ~s\n  expected ~s\n"
              src line got reg)))

@title[#:tag "tutorial"]{Amb: A Redex Tutorial}

This tutorial is designed for those familiar with the call-by-value
λ-calculus (and evaluation contexts), but not Redex. 
The tutorial works though a model of
the λ-calculus extended with a variation on McCarthy's @racket[amb] operator
for ambiguous choice@~cite[amb1 amb2].

If you are not familiar with Racket, first try 
@other-doc['(lib "quick.scrbl" "scribblings/quick")] or
@other-doc['(lib "more.scrbl" "scribblings/more")].

The model includes a standard evaluation reduction relation and a type system.
Along the way, the tutorial demonstrates Redex's support for unit testing,
random testing, typesetting, metafunctions, reduction relations,
and judgment forms. It also includes a number of exercises to use
as jumping off points to explore Redex in more depth, and each of the
functions and syntactic forms used in the examples are linked to more
information.

@section{Defining a Language}

To get started, open DrRacket, and put the following two
lines at the top of the file (if the first line is not
there already, use the @onscreen{Language|Choose Language...}
menu item to make sure that DrRacket is set to use
the language declaration in the source).
@codeblock|{
#lang racket
(require redex)
}|
Those lines tell DrRacket that we're writing a program
in the @racketmodname[racket] language and we're going
to be using the @racketmodname[redex] DSL.

Next, enter the following definition.

@racketblock+eval[#:eval amb-eval
                         (define-language L
                           (e (e e)
                              (λ (x t) e)
                              x
                              (amb e ...)
                              number
                              (+ e ...)
                              (if0 e e e)
                              (fix e))
                           (t (→ t t) num)
                           (x variable-not-otherwise-mentioned))]


The @racket[define-language] form gives a name to
a grammar. In this case, @racket[L] is the Racket-level
name referring to the grammar containing the non-terminal
@racket[e], with six productions (application, abstraction,
variables, @racket[amb] expressions, numbers, and addition
expressions), the non-terminal @racket[t] with two productions,
and the non-terminal @racket[x] that uses the pattern
keyword @racket[variable-not-otherwise-mentioned]. This 
special pattern matches all symbols except those used
as literals in the grammar (in this case: @racket[λ],
@racket[amb], @racket[+], and @racket[→]).

Once we have defined the grammar, we can ask Redex if 
specific terms match the grammar. This expression checks
to see if the @racket[e] non-terminal (from @racket[L])
matches the object-language expression @racket[(λ (x) x)].

To do this, first click the @onscreen{Run} button in DrRacket's
toolbar and then enter the following expression after the prompt
For the remainder of this tutorial, expressions prefixed with a
@tt{>} are intended to be run in the interactions window (lower pane), 
and expressions without the @tt{>} prefix belong in the definitions 
window (the upper pane).

@interaction/test[(redex-match 
                   L
                   e
                   (term (λ (x) x)))
                  not]

In general, a @racket[redex-match] expression first names a language,
then a pattern, and then its third position is an arbitrary Racket expression.
In this case, we use @racket[term] to construct an Redex object-level expression.
The @racket[term] operator is much like Lisp's @racket[quasiquote]
(typically written @tt{`}).

This term does not match @racket[e]
(since @racket[e] insists the function parameters come with types), 
so Redex responds with @racket[#f], false.

When an expression does match, as with this one:

@interaction/test[(redex-match 
                   L
                   e
                   (term ((λ (x num) (amb x 1)) 
                          (+ 1 2))))
                  (list/c match?)]

Redex responds with bindings for all of the pattern variables.
In this case, there is just one, @racket[e],
and it matches the entire expression.

We can also use matching to extract sub-pieces. For example, we can
pull out the function and argument position of an application expression
like this:

@interaction/test[(redex-match 
                   L
                   (e_1 e_2)
                   (term ((λ (x num) (amb x 1)) 
                          (+ 1 2))))
                  (list/c match?)]

As you probably noticed, @racket[redex-match] returns a list of 
matches, not just a single match. The previous matches each only
matched a single way, so the corresponding lists only have a single
element. But a pattern may be ambiguous, e.g., the following pattern
which matches any non-empty sequence of expressions, but binds different
elements of the sequence in different ways:

@interaction/test[(redex-match 
                   L
                   (e_1 ... e_2 e_3 ...)
                   (term ((+ 1 2)
                          (+ 3 4)
                          (+ 5 6))))
                  (list/c match? match? match?)]

@exercise[]

Use @racket[redex-match] to extract the body of the
@racket[λ] expression from this object-language program:
@racketblock[((λ (x num) (+ x 1)) 
              17)]

@exercise[]

Use @racket[redex-match] to extract the range portion
of the type
@racket[(→ num (→ num num))].


@exercise[]

Redex's pattern language supports ambiguity through non-terminals,
the @racket[in-hole] pattern, and ellipsis placement (as in the example
just above). Use the latter source of ambiguity to design a pattern
that matches one way for each adjacent pair of expressions in
a sequence. That is, if you match the sequence @racket[(1 2 3 4)],
then you'd expect one match for 1 & 2, one match for 2 & 3, and one match for 3 & 4.
In general, this pattern should produce n matches when there are n+1
expressions in the sequence.

To test your solution use @racket[redex-match] like this:
@racketblock[(redex-match
              L
               (code:comment @#,t{your solution goes here})
              (term (1 2 3 4)))]
where you expect a result like this
@racketblock[(list
              (match (list (bind 'e_1 1) (bind 'e_2 2)))
              (match (list (bind 'e_1 2) (bind 'e_2 3)))
              (match (list (bind 'e_1 3) (bind 'e_2 4))))]
but possibly with more pattern variables in the resulting
match.

@amb/test[(redex-match
           L
           (e_1 ... e_2 e_3 e_4 ...)
           (term (1 2 3 4)))
          (list/c match? match? match?)]

@exercise["named-ellipses"]

The ellipsis pattern can also be ``named'' via subscripts that, when
duplicated, force the lengths of the corresponding sequences to match.
For example, the pattern
@racketblock[((λ (x ...) e) v ...)]
matches application expressions where the function may have a different
arity than the number of arguments it receives, but the pattern:
@racketblock[((λ (x ..._1) e) v ..._1)]
ensures that the number of @racket[x]s is the same as the number
of @racket[v]s.

Use this facility to write a pattern that matches odd length lists 
of expressions, returning one match for each pair of expressions
that are equidistant from the ends of the sequence. For example,
if matching the sequence @racket[(1 2 3 4 5)], there would be two
matches, one for the pair @racket[1] & @racket[5] and another for
the pair @racket[2] & @racket[4]. Your match should include the
the bindings @racket[e_left] and @racket[e_right] that extract these
pairs (one element of the pair bound to @racket[e_left] and the other
to @racket[e_right]). Test your pattern with @racket[redex-match].

@amb/test[(redex-match
           L
           (e_1 ..._1 e_left e_3 ..._2
            e_4
            e_5 ..._2 e_right e_7 ..._1)
           (term (1 2 3 4 5)))
          (list/c match? match?)]

@section{Typing}

To support a type system for our language, we need 
to define type environments, which we do by extending
the language @racket[L] with a new non-terminal 
@racket[Γ], that we use to represent environments.

@racketblock+eval[#:eval 
                  amb-eval
                  (define-extended-language L+Γ L
                    [Γ · (x : t Γ)])]
                  
The @racket[define-extended-language] form accepts the
name of the new language, the name of the extended 
langauge and then a series of non-terminals just
like @racket[define-language].

In the extended language, we can give all of the
typing rules for our language. Ignoring the 
@racket[#:mode] specification for a moment, the beginning
of this use of @racket[define-judgment-form] 
has a contract declaration indicating that the judgments all have
the shape @racket[(types Γ e t)]. 

@racketblock+eval[#:eval 
                  amb-eval
                  (define-judgment-form
                    L+Γ
                    #:mode (types I I O)
                    #:contract (types Γ e t)
                    
                    [(types Γ e_1 (→ t_2 t_3))
                     (types Γ e_2 t_2)
                     -------------------------
                     (types Γ (e_1 e_2) t_3)]
                    
                    [(types (x : t_1 Γ) e t_2)
                     -----------------------------------
                     (types Γ (λ (x t_1) e) (→ t_1 t_2))]
                    
                    [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
                     ---------------------------------------
                     (types Γ (fix e) (→ t_1 t_2))]
                    
                    [---------------------
                     (types (x : t Γ) x t)]
                    
                    [(types Γ x_1 t_1)
                     (side-condition (different x_1 x_2))
                     ------------------------------------
                     (types (x_2 : t_2 Γ) x_1 t_1)]
                    
                    [(types Γ e num) ...
                     -----------------------
                     (types Γ (+ e ...) num)]
                    
                    [--------------------
                     (types Γ number num)]
                    
                    [(types Γ e_1 num)
                     (types Γ e_2 t)
                     (types Γ e_3 t)
                     -----------------------------
                     (types Γ (if0 e_1 e_2 e_3) t)]
                    
                    [(types Γ e num) ...
                     --------------------------
                     (types Γ (amb e ...) num)])]

The first clause gives
the typing rule for aplication expressions, saying
that if @racket[e_1] has the type 
@racket[(→ t_2 t_3)] and @racket[e_2] has the type
@racket[t_2], then the application expression 
has the type @racket[t_3].

Similarly, the other clauses give the typing
rules for all of the other forms in the language.

Most of the rules use use @racket[types], or give base types to atomic
expressions, but the fourth rule is worth a special look. It says that
if a variable typechecks in some environment, then it also type
checks in an extended environment, provided that the environment extension
does not use the variable in question. 

The @racket[different] function is a metafunction, defined as you might expect:
@racketblock+eval[#:eval amb-eval
                         (define-metafunction L+Γ
                           [(different x_1 x_1) #f]
                           [(different x_1 x_2) #t])]

The @racket[#:mode] specification tells Redex how to compute derivations. In this case, 
the mode specification indicates that @racket[Γ] and @racket[e] are to be 
thought of as inputs, and the type position is to be thought of as an output.

Redex then checks that spec, making sure that, given a particular @racket[Γ] and 
@racket[e], it can compute a @racket[t] or, perhaps, multiple @racket[t]s (if
the patterns are ambiguous, or if multiple rules apply to a given pair of
@racket[Γ] and @racket[e]).

@amb-try[(test-equal (judgment-holds (types (x : num ·) y t_1) t_1)
                     (term ()))
         (test-equal (judgment-holds (types (x : num ·) x t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types (x : num (y : (→ num num) ·)) x t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types (x : num (y : (→ num num) ·)) y t_1) t_1)
                     (term ((→ num num))))
         (test-equal (judgment-holds (types (x : num (x : (→ num num) ·)) y t_1) t_1)
                     (term ()))
         (test-equal (judgment-holds (types · (+ 1 2 3) t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types · (λ (y num) y) t_1) t_1)
                     (term ((→ num num))))
         (test-equal (judgment-holds (types · (λ (y num) (λ (z num) z)) t_1) t_1)
                     (term ((→ num (→ num num)))))
         (test-equal (judgment-holds (types · (λ (y num) (λ (z num) y)) t_1) t_1)
                     (term ((→ num (→ num num)))))
         (test-equal (judgment-holds (types · (λ (y num) (λ (z num) q)) t_1) t_1)
                     (term ()))
         (test-equal (judgment-holds (types · (((λ (x num) (λ (y num) x)) 1) 2) t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types · (amb) t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types · (amb 1) t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types · (amb 1 2) t_1) t_1)
                     (term (num)))
         (test-equal (judgment-holds (types · (+ (amb (+ 1 2) (+ 3 4.5))
                                                 (amb (+ 5 6) (+ 7 8)))
                                            t_1)
                                     t_1)
                     (term (num)))]
@(check-output (test-results)
               #rx"All [0-9]+ tests passed")

@section{Testing Typing}

The @racket[judgment-holds] form checks to see if a potential judgment
is derivable. For example,

@interaction/test[(judgment-holds 
                   (types ·
                          ((λ (x num) (amb x 1)) 
                           (+ 1 2))
                          t)
                   t)
                  (list/c 'num)]

computes all of the types that the expression
@racketblock[((λ (x num) (amb x 1)) 
              (+ 1 2))]
has, returning a list of them (in this case, just one).

In general, the @racket[judgment-holds] form's first argument
is an instance of some judgment-form that should have concrete
terms for the @racket[I] positions in the mode spec, and patterns
in the positions labelled @racket[O]. Then, the second position
in @racket[judgment-holds] is an expression that can use the
pattern variables inside those @racket[O] positions. The result
of @racket[judgment-holds] will be a list of terms, one for
each way that the pattern variables in the @racket[O] positions
can be filled when evaluating @racket[judgment-holds]'s second
position.

For example,
if we wanted to extract only the range position of the type
of some function, we could write this:

@interaction/test[(judgment-holds 
                   (types ·
                          (λ (f (→ num (→ num num))) (f (amb 1 2)))
                          (→ t_1 t_2))
                   t_2)
                  (list/c (list/c '→ 'num 'num))]

The result of this expression is a singleton list containing the
function type that maps numbers to numbers. The reason you see
two open parentheses is that Redex exploits Racket's s-expressions
to reflect Redex terms as Racket values. Here's another way
to write the same value

@interaction[#:eval amb-eval (list (term (→ num num)))]

Racket's printer does not know that it should use @racket[term] for the
inner lists and @racket[list] (or @racket[quote]) for the
outer list, so it just uses the @racket[quote] notation for all
of them.

We can combine @racket[judgment-holds] with Redex's unit test support 
to build a small test suite:
@interaction[#:eval amb-eval
                    (test-equal 
                     (judgment-holds 
                      (types · (λ (x num) x) t)
                      t)
                     (list (term (→ num num))))
                    (test-equal 
                     (judgment-holds 
                      (types · (amb 1 2 3) t)
                      t)
                     (list (term num)))
                    (test-equal 
                     (judgment-holds 
                      (types · (+ 1 2) t)
                      t)
                     (list (term (→ num num))))]

Redex is silent when tests pass and gives the source location for the failures, as above.
The @racket[test-equal] form accepts two expressions, evaluates them,
and checks to see if they are @racket[equal?] (structural equality). 

To see a summary of the tests run so far, call @racket[test-results].

@interaction[#:eval amb-eval (test-results)]

@exercise[]

Remove the @racket[different] side-condition and demonstrate
how one expression now has multiple types, using @racket[judgment-holds].
That is, find a use of @racket[judgment-holds] that returns a list
of length two, with two different types in it.

@exercise[]

The typing rule for @racket[amb] is overly restrictive. In general,
it would be better to have a rule like this one:

@racketblock[[(types Γ e t) ...
              -----------------------
              (types Γ (amb e ...) t)]]

but Redex does not support this rule because the mode specification is 
not satisfied in the case that @racket[amb] has no subexpressions. That is,
any type should be okay in this case, but Redex cannot ``guess'' which type
is the one needed for a particular derivation, so it rejects the entire
@racket[define-judgment-form] definition. (The error message is different,
but this is the ultimate cause of the problem.)

Fix this by annotating @racket[amb] expressions with their types, making suitable
changes to the language as well as the @racket[define-judgment-form] for
@racket[types]. Add new test cases to make sure you've done this properly.

@section{Defining a Reduction Relation}

To reduce terms, Redex provides @racket[reduction-relation], a form 
that defines unary relations by cases. To define a reduction
relation for our @racket[amb] language, we first need to define
the evaluation contexts and values, so we extend the
language a second time.


@; these definitions are just like racketblock+eval, but also
@; preserve source locations so we can show typesetting
@; examples in a later section
@(require syntax/strip-context)
@(define-syntax (m stx)
   (syntax-case stx ()
     [(_ arg)
      (let ()
        (define rewritten
          (let loop ([stx #'arg])
            (cond
              [(syntax? stx) #`(datum->syntax #f
                                              #,(loop (syntax-e stx))
                                              (vector (convert-to-path 
                                                       '#,(and (path? (syntax-source stx))
                                                               (path->main-collects-relative (syntax-source stx))))
                                                      #,(syntax-line stx)
                                                      #,(syntax-column stx)
                                                      #,(syntax-position stx)
                                                      #,(syntax-span stx)))]
              [(pair? stx) #`(cons #,(loop (car stx))
                                   #,(loop (cdr stx)))]
              [(or (symbol? stx) (null? stx)
                   (number? stx) (keyword? stx)
                   (string? stx))
               #`'#,stx]
              [else (error 'm "unk ~s" stx)])))
        #`(let ()
            (amb-eval #,rewritten)
            (racketblock arg)))]))
@(define (convert-to-path src)
   (cond
     [(path? src) src]
     [(not src) src]
     [else
      (apply build-path
             (find-collects-dir) 
             (map bytes->path (cdr src)))]))

@m[(define-extended-language Ev L+Γ
     (p (e ...))
     (P (e ... E e ...))
     (E (v E) 
        (E e)
        (+ v ... E e ...)
        (if0 E e e)
        (fix E)
        hole)
     (v (λ (x t) e)
        (fix v)
        number))]

To give a suitable notion of evaluation for @racket[amb], we define
@racket[p], a non-terminal for programs. Each program consists of a
sequence of expressions and we will use them to represent the possible
ways in which an @racket[amb] expression could have been evaluated.
Initially, we will simply wrap an expression in a pair of parentheses
to generate a program that consists of that single expression.

The non-terminal @racket[P] gives the corresponding evaluation contexts
for @racket[p]s and says that evaluation can occur in any of them,
without restriction. The grammar for @racket[E] dictates that 
reduction may occur inside application expressions and addition expressions,
always from left to right.

To prepare for the reduction relation, we first define a metafunction
for summation.

@racketblock+eval[#:eval 
                  amb-eval
                  (define-metafunction Ev
                    Σ : number ... -> number
                    [(Σ number ...) 
                     ,(apply + (term (number ...)))])]

@amb/test[(term (Σ 1 2 3 4))
          (λ (x) (equal? x 10))]

This lifts the Racket function @racket[+] to Redex, giving it the name
@racket[Σ]. The unquote (comma) in the definition of the metafunction
escapes into Racket, using @racket[apply] and @racket[+] to sum
up the sequence of numbers that were passed to @racket[Σ]. As we've
noted before, the
@racket[term] operator is like Racket's @racket[quasiquote] operator, but
it is also sensitive to Redex pattern variables. In this case,
@racket[(term (number ...))] produces a list of numbers, extracting
the arguments from the call to @racket[Σ].

To define a reduction relation, we also have to define substitution.
Generally speaking, substitution functions are tricky to get right
and, since they generally are not shown in papers, we have defined
a workhorse substitution function in Racket that runs in near linear 
time. The source code is included with Redex. If you'd like to have a look,
evaluate the expression below in the REPL to find the precise path
on your system:

@centered{@racket[(collection-file-path "tut-subst.rkt" "redex")]}

(Test cases are in @filepath{test/tut-subst-test.rkt}, 
relative to @filepath{tut-subst.rkt}.)

That file contains the definition of the function @racket[subst/proc], 
which expects four
arguments: a predicate for determining if an expression is a variable,
a list of variables to replace, a list of terms to replace them with,
and a term to do the replacement inside (the function has a hard-wired
notion of the shape of all binding forms, but is agnostic to the other
expression forms in the language).

To use this substitution function, we also need to lift it into Redex, 
just like we did for @racket[Σ].

@racketblock+eval[#:eval 
                  amb-eval
                  (require redex/tut-subst)
                  (define-metafunction Ev
                    subst : x v e -> e
                    [(subst x v e) 
                     ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
                  (define x? (redex-match Ev x))]

In this case, we use @racket[term] to extract the values of the Redex variables
@racket[x], @racket[v], and @racket[e] and then pass them to @racket[subst/proc].

The definition of @racket[x?] uses a specialized, more efficient form of
@racket[redex-match]; supplying @racket[redex-match] with only two arguments
permits Redex to do some processing of the pattern, and it results in a
predicate that matches the pattern in the given language (which we can
supply directly to @racket[subst/proc]).

Using that substitution function, we can now give the reduction relation.

@m[(define red
     (reduction-relation
      Ev
      #:domain p
      (--> (in-hole P (if0 0 e_1 e_2))
           (in-hole P e_1)
           "if0t")
      (--> (in-hole P (if0 v e_1 e_2))
           (in-hole P e_2)
           (side-condition (not (equal? 0 (term v))))
           "if0f")
      (--> (in-hole P ((fix (λ (x t) e)) v))
           (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
           "fix")
      (--> (in-hole P ((λ (x t) e) v))
           (in-hole P (subst x v e))
           "βv")
      (--> (in-hole P (+ number ...))
           (in-hole P (Σ number ...))
           "+")
      (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...)
           (e_1 ... (in-hole E e_2) ... e_3 ...)
           "amb")))]

@amb-try[(test-->> red 
                   (term ((+ 1 2 3)))
                   (term (6)))
         (test-->> red 
                   (term ((+ 1 (+ 2 3))))
                   (term (6)))
         (test-->> red 
                   (term ((+ (+ 1 2) 3)))
                   (term (6)))
         (test-->> red
                   (term ((amb (+ 1 2) 4)))
                   (term (3 4)))
         (test-->> red
                   (term ((+ 1 (amb 1 2))))
                   (term (2 3)))
         (test-->> red
                   (term ((((λ (x num) (λ (y num) x)) 1) 2)))
                   (term (1)))
         (test-->> red
                   (term ((((λ (x num) (λ (y num) (amb x y))) 1) 2)))
                   (term (1 2)))
         (test-->> red
                   (term ((+ (amb (+ 1 2) (+ 3 4.5))
                             (amb (+ 5 6) (+ 7 8)))))
                   (term (14 18 18.5 22.5)))
         (test-->> red
                   (term ((if0 0 1 2)))
                   (term (1)))
         (test-->> red
                   (term ((if0 2 1 0)))
                   (term (0)))
         (test-->> red
                   (term (((fix (λ (f (→ num num)) (λ (x num) (if0 x 0 (f (+ x -1)))))) 2)))
                   (term (0)))]

@(check-output (test-results)
               #rx"All [0-9]+ tests passed")

The @racket[reduction-relation] form accepts the name of a language,
the domain of the relation (@racket[p] in this case), and 
then a series of rewriting rules, each of the form @racket[(--> pattern pattern)].

The first rule replaces @racket[if0] expressions when the test position
is @racket[0] by the second subexpression (the true branch). It uses
the @racket[in-hole] pattern, the Redex notation for context decomposition.
In this case, it decomposes a program into some @racket[P] with
an approriate @racket[if0] expression inside, and then the right-hand
side of the rule places @racket[e_1] into the same context.

The rule for the false branch should apply when the test position is
any value except @racket[0]. To establish this, we use a
@racket[side-condition]. In general, a side-condition is a Racket
expression that is evaluated for expressions where the pattern matches;
if it returns true, then the rule fires. In this case, we
use @racket[term] to extract the value of @racket[v] and then
compare it with @racket[0].

To explore the behavior of a reduction relation, Redex provides
@racket[traces] and @racket[stepper]. They both accept a reduction
relation and a term, and then show you how that term reduces in
a GUI. The GUI that @racket[traces] uses is better suited to
a quick overview of the reduction graph and @racket[stepper]
is better for more detailed explorations of reduction graphs that
have larger expressions in them.

@exercise[]

Evaluate 
@racketblock[(traces red 
                     (term ((+ (amb 1 2)
                               (amb 10 20)))))]
It does not show all of the terms by default, but 
one click the @onscreen{Reduce} button shows them all.

If you have Graphviz installed, Redex can use it to
lay out the graph; click @onscreen{Fix Layout} and
Redex will call out to @tt{dot} to lay out the graph.

@exercise[]

Design a function that accepts a number @racket[n] and
evaluates (ambiguously) to any of the numbers between 
@racket[n] and @racket[0]. Call it with @racket[10] and
look at the results in both @racket[traces] and
@racket[stepper].

Hint: to subtract 1 from @racket[n], use @racket[(+ n -1)]

@amb/test[(apply-reduction-relation*
           red
           (term (((fix
                    (λ (f (→ num num))
                      (λ (n num)
                        (if0 n
                             0
                             (amb n (f (+ n -1)))))))
                   10))))
          (list/c (list/c 10 9 8 7 6 5 4 3 2 1 0))]

@section{Testing Reduction Relations}

Redex provides @racket[test-->>] for using
testing the transitive closure of a reduction relation. 
If you supply it a reduction relation and two terms,
it will reduce the first term and make sure that it
yields the second.

@interaction[#:eval amb-eval
                    (test-->>
                     red
                     (term ((if0 1 2 3)))
                     (term (3)))
                    (test-->>
                     red
                     (term ((+ (amb 1 2)
                               (amb 10 20))))
                     (term (11 21 12 22)))
                    (test-results)]

The @racket[test-->] form is like @racket[test-->>], except
that it only reduces the term a single step. 

@interaction[#:eval amb-eval
                    (test-->
                     red
                     (term ((+ (amb 1 2) 3)))
                     (term ((+ 1 3) (+ 2 3))))
                    (test-results)]

If a term
produces multiple results, then each of the results
must be listed.

@interaction[#:eval amb-eval
                    (test-->
                     red
                     (term ((+ 1 2) (+ 3 4)))
                     (term (3 (+ 3 4)))
                     (term ((+ 1 2) 7)))
                    (test-results)]

Technically, when using @racket[test-->>],
it finds 
all irreducible terms that are reachable from the
given term, and expects them all to be listed,
with one special case: when it detects a cycle in the 
reduction graph, then it signals an error. (Watch out: when
the reduction graph is infinite and there are no cycles,
then @racket[test-->>] consumes all available memory.)

@interaction[#:eval amb-eval
                    (test-->>
                     red
                     (term (((fix (λ (x (→ num num)) x)) 1))))
                    (test-results)]

To suppress this behavior, pass @racket[#:cycles-ok]
to @racket[test-->>].

@interaction[#:eval amb-eval
                    (test-->>
                     red #:cycles-ok
                     (term (((fix (λ (x (→ num num)) x)) 1))))
                    (test-results)]

This test case has no expected results but still passes, since 
there are no irreducible terms reachable from the given term.

@exercise[]

Extend λ to support multiple arguments. Use the notation
@racket[(λ (x t) ... e)] for multi-arity λ expressions
because the @racket[subst/proc]
function works properly with @racket[λ] expressions of that
shape. Use this definition of @racket[subst].

@racketblock[(define-metafunction Ev
               subst : (x v) ... e -> e
               [(subst (x v) ... e) 
                ,(subst/proc x?
                             (term (x ...))
                             (term (v ...))
                             (term e))])]

Also, adjust the typing rules (and do not forget that an ellipsis can be named, 
as discussed in exercise @exref["named-ellipses"]).

@section{Random Testing}

Random testing is a cheap and easy way to find counter-examples to false
claims. Unsurprisingly, it is hard to pin down exactly which false claims
that random testing can provide counter-examples to. @citet[Hanford]
put it best (calling his random test case generator a syntax machine):
``[a]lthough as a writer of test cases, the syntax machine is certainly 
unintelligent, it is also uninhibited. It can test a [language] processor with many 
combinations that would not be thought of by a human test case writer.''

To get a sense of how random testing works, we define this
Racket predicate
@racketblock+eval[#:eval 
                  amb-eval
                  (define (progress-holds? e)
                    (if (types? e)
                        (or (v? e)
                            (reduces? e))
                        #t))]
that captures the statement of the progress result.

The three helper functions @racket[types?], @racket[v?],
and @racket[reduces?] can be defined by using our earlier
definitions of typing, the grammar, and the reduction relation,
plus calls into Redex:

@racketblock+eval[#:eval
                  amb-eval
                  (define (types? e)
                    (not (null? (judgment-holds (types · ,e t)
                                                t))))
                  
                  (define v? (redex-match Ev v))
                  
                  (define (reduces? e)
                    (not (null? (apply-reduction-relation 
                                 red
                                 (term (,e))))))]

The only new construct here is @racket[apply-reduction-relation], which accepts
a reduction and a term, and returns a list of expressions that it reduces to
in a single step. Thus, @racket[reduces?] returns @racket[#t] when the given
term is reducible and @racket[#f] otherwise.

Putting all of that together with @racket[redex-check] will cause
Redex to randomly generate 1,000 @racket[e]s and attempt to falsify them:

@interaction[#:eval
             amb-eval
             (redex-check Ev e (progress-holds? (term e)))]

The @racket[redex-check] form accepts the name of a language (@racket[Ev] in this case),
a pattern (@racket[e] in this case), and a Racket expression that returns a
boolean. It randomly generates expressions matching the pattern and then invokes
the expression in an attempt to elicit @racket[#f] from the Racket expression.

We can also ask redex-check how good of a job it is doing. 
Specifically, this expression re-runs the same random test, but
this time sets up some instrumenting infrastructure to 
determine how many of the reduction rules fire during the
testing. In this case, we create a coverage value that
indicates that we're interested in how many of the rules in
@racket[red] fired, and then we install it using the 
@racket[relation-coverage] parameter. In the dynamic
extent of the @racket[parameterize], then, the relation
will record how it gets tested. Once that returns we can use
@racket[covered-cases] to see exactly how many times each case
fired.

@interaction[#:eval
             amb-eval
             (let ([c (make-coverage red)])
               (parameterize ([relation-coverage (list c)])
                 (redex-check Ev e (progress-holds? (term e))))
               (covered-cases c))]

Not many of them! To improve coverage, we can tell redex-check
to try generating expressions using the patterns on the left-hand
side of the rules to generate programs, and then check to see if progress for each of
the expressions in the program:

@interaction[#:eval
             amb-eval
             (check-reduction-relation 
              red
              (λ (p) (andmap progress-holds? p)))]

The @racket[check-reduction-relation] is a shorthand for using @racket[redex-check] to
generate elements of the domain of the given reduction relation (@racket[red] in this
case), and then pass them to the given function, attempting to elicit @racket[#f].

In this case, since the domain of @racket[red] is @racket[p], the random generator
produces sequences of @racket[e] expressions, which are reflected
into Redex as lists, and so we simply try to see if progress holds
for each element of the list, using @racket[andmap].

Still no test failures, but installing the same coverage testing boilerplate around 
the call to @racket[check-reduction-relation] tells us that we got much
better coverage of the reduction system.

@interaction[#:eval
             amb-eval
             (let ([c (make-coverage red)])
               (parameterize ([relation-coverage (list c)])
                 (check-reduction-relation 
                  red
                  (λ (p) (andmap progress-holds? p)))
                 (covered-cases c)))]


@exercise[]

Remove one of the productions from @racket[E] (except @racket[hole]) 
and find an expression in the revised system that causes 
@racket[progress?] to return @racket[#f].

See if @racket[redex-check] can also falsify progress for the same
system.

@exercise[]

Formulate and randomly check type preservation. Usually, this lemma
says that if an expression has a type and it takes a step, then 
it produces an expression with the same type. In this case, however,  
formulate a predicate that accepts an expression and checks that, if
it has a type and takes a step, then all of the resulting expressions in the new
program have the same type.

@(check-output (let ()
                 (define (preservation-holds? e) 
                   (define tys (judgment-holds (types · ,e t) t))
                   (case (length tys)
                     [(0) #t]
                     [(1)
                      (for/and ([next-p (apply-reduction-relation red `(,e))])
                        (for/and ([next-e (in-list next-p)])
                          (define next-tys (judgment-holds (types · ,next-e t) t))
                          (and (= 1 (length next-tys))
                               (equal? (car next-tys)
                                       (car tys)))))]
                     [else #f]))
                 (check-reduction-relation
                  red
                  (λ (p) 
                    (andmap preservation-holds? p))))
               #rx"check-reduction-relation: no counterexamples in 1000 attempts")

@section{Typesetting the Reduction Relation}

Redex's typsetting facilities accept languages, metafunctions, reduction relations,
and judgment-forms and produce typeset output that can be included directly
into a figure in a paper.

@interaction[#:eval 
             amb-eval
             (render-reduction-relation red)]

The result of @racket[render-reduction-relation] is rendered directly in DrRacket's
interactions window, and also can be saved as a @filepath{.ps} file by passing
the name of the file as the second argument to @racket[render-reduction-relation].

Redex's typesetting also interoperates with the @racketmodname[pict] library.
If we pull it in with a @racket[require]:

@racketblock[(require #,(racketmodname pict))]

then we can use the pict primitives to combine typeset fragments into a larger whole.

@interaction[#:eval 
             amb-eval
             (scale (vl-append
                     20
                     (language->pict Ev)
                     (reduction-relation->pict red))
                    3/2)]

Generally speaking, Redex has reasonable default ways to typeset its
definitions, except when they escapes to Racket. In that case,
it typsets the code in a fixed-width font and makes the background pink to call our
attention to it. While it is possible to use @racket[with-unquote-rewriter] to 
tell Redex how to typset those regions, often it is easier to define a metafunction
and call it. In this case, we can use @racket[different] (defined earlier).

@m[(define if0-false-rule
     (reduction-relation
      Ev #:domain p
      (--> (in-hole P (if0 v e_1 e_2))
           (in-hole P e_2)
           (side-condition (term (different v 0)))
           "if0f")))]

Now when we typeset this reduction-relation there is no pink.

@interaction[#:eval amb-eval (render-reduction-relation if0-false-rule)]

Still, the typesetting is non-optimal, so we can use @racket[with-compound-rewriter]
to adjust the way calls to @racket[different] typeset.

@interaction[#:eval amb-eval 
                    (with-compound-rewriter
                     'different
                     (λ (lws)
                       (list "" (list-ref lws 2) " ≠ " (list-ref lws 3) ""))
                    (render-reduction-relation if0-false-rule))]

The compound rewriter is given a list of @racket[lw] structs that correspond to the 
untypeset sequence for a use of @racket[different], and then can replace them with
a different set of strings and @racket[lw]s. For more details on the structure of
@racket[lw] structs and to experiment with them, see @racket[to-lw].

@exercise[]

Redex uses the indentation and newlines in the program source code to
determine where the line breaks in the printed output goes, instead of
using a pretty-printer, so as to give Redex programmers fine-grained
control over how their models typeset.

Exploit this facility so that this expression produces an expression
with a minimum amount of whitespace within its bounding box.
(The call to @racket[frame] helps to clarify where the bounding
box is.)

@racketblock[(frame
              (vl-append
               20
               (language->pict Ev)
               (reduction-relation->pict red)))]

That is, adjust the whitespace in @racket[Ev] so that it
fills as much of the width established by rendering @racket[red].

@exercise[]

Typeset @racket[types]. Use a compound rewriter so a use of @racket[(type Γ e t)]
is rendered as @racketblock[Γ ⊢ e : t]

@generate-bibliography[]


@close-eval[amb-eval]


@; Needs a timeout for testing:
@(module* test racket/base
   (require (submod ".."))
   (module config info
     (define timeout 300)))
