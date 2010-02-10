#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval
          (for-syntax scheme/base)
          (for-label scheme/base
                     scheme/gui
                     scheme/pretty
                     scheme/contract
		     mrlib/graph
                     (only-in slideshow/pict pict? text dc-for-text-size text-style/c)
                     redex))

@(define-syntax (defpattech stx)
   (syntax-case stx ()
     [(_ arg)
      (identifier? #'arg)
      (let ([as (symbol->string (syntax-e #'arg))])
        #`(index '("Redex Pattern" #,as) (deftech #:style? #f @scheme[arg])))]))

@(define-syntax (pattech stx)
   (syntax-case stx ()
     [(_ arg)
      (identifier? #'arg)
      #`(tech #,(symbol->string (syntax-e #'arg)))]))

@(define-syntax (ttpattern stx)
   (syntax-case stx ()
    [(_ args ...)
     #'((tech (schemevarfont "pattern")) args ...)]
    [x (identifier? #'x) #'(tech (schemevarfont "pattern"))]))

@(define-syntax (pattern stx)
   (syntax-case stx ()
    [(_ args ...)
     #'((tech "pattern") args ...)]
    [x (identifier? #'x) #'(tech "pattern")]))

@(define-syntax (tttterm stx)
   (syntax-case stx ()
    [(_ args ...)
     #'((tech (schemevarfont "term")) args ...)]
    [x (identifier? #'x) #'(tech (schemevarfont "term"))]))

@(define-syntax (tterm stx)
   (syntax-case stx ()
    [(_ args ...)
     #'((tech "term") args ...)]
    [x (identifier? #'x) #'(tech "term")]))

@(define-syntax-rule (arrows a0 a ...)
   (make-blockquote #f 
    (list (make-paragraph 
           (list (schemeidfont (make-element #f (list (symbol->string 'a0))))
                 (make-element #f (list " " (hspace 1) " " (schemeidfont (symbol->string 'a)))) ...)))))

@(define redex-eval (make-base-eval))
@(interaction-eval #:eval redex-eval (require redex/reduction-semantics))

@title{@bold{Redex}: Practical Semantics Engineering}

@author["Robert Bruce Findler" "Casey Klein"]

PLT Redex consists of a domain-specific language for specifying
reduction semantics, plus a suite of tools for working with the
semantics. 

This is a reference manual for Redex. See
@link["http://redex.plt-scheme.org/"]{@tt{http://redex.plt-scheme.org/}}
for a gentler overview. (See also the @tt{examples} subdirectory in
the @tt{redex} collection.)

To load Redex use: @defmodule[redex] which provides all of
the names documented in this library.

The module @schememodname[redex/reduction-semantics]
provides only the non-GUI portions of what is described in
this manual (everything except the last two sections),
making it suitable for use with @tt{mzscheme} scripts.

@table-of-contents[]

@section{Patterns}

@defmodule[redex/reduction-semantics]

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

This section covers Redex's @deftech{pattern} language, used in many
of Redex's forms.

Note that pattern matching is caching (including caching the results
of side-conditions). This means that once a pattern has matched a
given term, Redex assumes that it will always match that term. 

This is the grammar for the Redex pattern language. Non-terminal
references are wrapped with angle brackets; otherwise identifiers
in the grammar are terminals.

@(schemegrammar* #;#:literals #;(any number string variable variable-except variable-prefix variable-not-otherwise-mentioned hole hide-hole name in-hole side-condition cross) 
   [pattern any 
            number 
            natural
            integer
            real
            string 
            variable 
            (variable-except <id> ...)
            (variable-prefix <id>)
            variable-not-otherwise-mentioned
            hole
            symbol
            (name <id> <pattern>)
            (in-hole <pattern> <pattern>)
            (hide-hole <pattern>)
            (side-condition <pattern> guard)
            (cross <id>)
            (<pattern-sequence> ...)
            <scheme-constant>]
   [pattern-sequence 
     <pattern> 
     (code:line ... (code:comment "literal ellipsis"))
     ..._id])

@itemize[

@item{The @defpattech[any] @pattern matches any sexpression.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[number] @pattern matches any number.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[natural] @pattern matches any exact 
non-negative integer.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[integer] @pattern matches any exact integer.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[real] @pattern matches any real number.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[string] @pattern matches any string. 
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[variable] @pattern matches any symbol.
This @pattern may also be suffixed with an underscore and another
identifier, in which case they bind the full name (as if it
were an implicit @pattech[name] @pattern) and match the portion
before the underscore.
}

@item{The @defpattech[variable-except] @pattern matches any symbol except those
listed in its argument. This is useful for ensuring that
keywords in the language are not accidentally captured by
variables. 
}

@item{ The @defpattech[variable-prefix] @pattern matches any symbol
that begins with the given prefix. }

@item{The @defpattech[variable-not-otherwise-mentioned] @pattern matches any
symbol except those that are used as literals elsewhere in
the language.
}

@item{The @defpattech[hole] @pattern matches anything when inside
the first argument to an @pattech[in-hole] @|pattern|. Otherwise, 
it matches only a hole.
}

@item{The @defpattech[symbol] @pattern stands for a literal symbol that must
match exactly, unless it is the name of a non-terminal in a
relevant language or contains an underscore. 

If it is a non-terminal, it matches any of the right-hand
sides of that non-terminal. If the non-terminal appears
twice in a single pattern, then the match is constrained
to expressions that are the same, unless the pattern is part
of a grammar, in which case there is no constraint.

If the symbol is a non-terminal followed by an underscore,
for example @tt{e_1}, it is implicitly the same as a name @pattern
that matches only the non-terminal, @tt{(@pattech[name] e_1 e)} for the
example. Accordingly, repeated uses of the same name are
constrainted to match the same expression.

If the symbol is a non-terminal followed by @tt{_!_}, for example
@tt{e_!_1}, it is also treated as a @|pattern|, but repeated uses of
the same @pattern are constrained to be different. For
example, this @|pattern|:

@schemeblock[(e_!_1 e_!_1 e_!_1)]

matches lists of three @tt{e}s, but where all three of them are
distinct.

Unlike a @tt{_} @|pattern|, the @tt{_!_} @|pattern|s do not bind names.

If @tt{_} names and @tt{_!_} are mixed, they are treated as
separate. That is, this @pattern @tt{(e_1 e_!_1)} matches just the
same things as @tt{(e e)}, but the second doesn't bind any
variables.

If the symbol otherwise has an underscore, it is an error.
}

@item{The @pattern @tt{(@defpattech[name] symbol @ttpattern)}
matches @ttpattern and binds using it to the name @tt{symbol}. 
}

@item{The @tt{(@defpattech[in-hole] @ttpattern @ttpattern)} @pattern
matches the first @|ttpattern|. This match must include exactly one match
against the second @|ttpattern|. If there are zero matches or more
than one match, an exception is raised.

When matching the first argument of in-hole, the @scheme[hole] @pattern
matches any sexpression. Then, the sexpression that matched the hole
@pattern is used to match against the second @|pattern|.
}

@item{The @tt{(@defpattech[hide-hole] @ttpattern)} @pattern matches what
the embedded @ttpattern matches but if the @pattern matcher is
looking for a decomposition, it ignores any holes found in
that @|ttpattern|.
}

@item{The @tt{(@defpattech[side-condition] @ttpattern guard)} @pattern
matches what the embedded @ttpattern matches, and then the guard
expression is evaluated. If it returns @scheme[#f], the @pattern fails
to match, and if it returns anything else, the @pattern matches. Any
occurrences of @scheme[name] in the @pattern (including those implicitly
there via @tt{_} pattersn) are bound using @scheme[term-let] in the
guard. 
}

@item{The @tt{(@defpattech[cross] symbol)} @pattern is used for the compatible
closure functions. If the language contains a non-terminal with the
same name as @scheme[symbol], the @pattern @scheme[(cross symbol)] matches the
context that corresponds to the compatible closure of that
non-terminal.
}

@item{The @tt{(@defpattech[pattern-sequence] ...)}
@pattern matches a sexpression
list, where each pattern-sequence element matches an element
of the list. In addition, if a list @pattern contains an
ellipsis, the ellipsis is not treated as a literal, instead
it matches any number of duplications of the @pattern that
came before the ellipses (including 0). Furthermore, each
@tt{(@pattech[name] symbol @ttpattern)} in the duplicated @pattern binds a
list of matches to @tt{symbol}, instead of a single match.  (A
nested duplicated @pattern creates a list of list matches,
etc.) Ellipses may be placed anywhere inside the row of
@|pattern|s, except in the first position or immediately after
another ellipses.

Multiple ellipses are allowed. For example, this @|pattern|:

@schemeblock[((name x a) ... (name y a) ...)]

matches this sexpression:

@schemeblock[(@#,tttterm (a a))]

three different ways. One where the first @tt{a} in the @pattern
matches nothing, and the second matches both of the
occurrences of @tt{a}, one where each named @pattern matches a
single @tt{a} and one where the first matches both and the
second matches nothing.

If the ellipses is named (ie, has an underscore and a name
following it, like a variable may), the @pattern matcher
records the length of the list and ensures that any other
occurrences of the same named ellipses must have the same
length. 

As an example, this @|pattern|:

@schemeblock[((name x a) ..._1 (name y a) ..._1)]

only matches this sexpression:

@schemeblock[(@#,tttterm (a a))]

one way, with each named @pattern matching a single a. Unlike
the above, the two @|pattern|s with mismatched lengths is ruled
out, due to the underscores following the ellipses.

Also, like underscore @|pattern|s above, if an underscore
@pattern begins with @tt{..._!_}, then the lengths must be
different.

Thus, with the @|pattern|:

@schemeblock[((name x a) ..._!_1 (name y a) ..._!_1)]

and the expression

@schemeblock[(@#,tttterm (a a))]

two matches occur, one where @tt{x} is bound to @scheme['()] and
@tt{y} is bound to @scheme['(a a)] and one where @tt{x} is bound to
@scheme['(a a)] and @tt{y} is
bound to @scheme['()].

}
]

@defform*[[(redex-match lang @#,ttpattern any)
           (redex-match lang @#,ttpattern)]]{
          
If @scheme[redex-match] receives three arguments, it
matches the pattern (in the language) against its third
argument. If it matches, this returns a list of match
structures describing the matches. If it fails, it returns
@scheme[#f].

If @scheme[redex-match] receives only two arguments, it
builds a procedure for efficiently testing if expressions
match the pattern, using the language @scheme[lang]. The
procedures accepts a single expression and if the expresion
matches, it returns a list of match structures describing the
matches. If the match fails, the procedure returns @scheme[#f].
}

@defproc[(match? [val any/c]) boolean?]{

Determines if a value is a @tt{match} structure.
}

@defproc[(match-bindings [m match?]) (listof bind?)]{

This returns a bindings structure (see below) that
binds the pattern variables in this match.
}

@defstruct[bind ([name symbol?] [exp any/c])]{

Instances of this struct are returned by @scheme[redex-match].
Each @scheme[bind] associates a name with an s-expression from the
language, or a list of such s-expressions, if the @tt{(@pattech[name] ...)}
clause is followed by an ellipsis.  Nested ellipses produce
nested lists.
}

@defparam[caching-enabled? on? boolean?]{
  When this parameter is @scheme[#t] (the default), Redex caches the results of 
  pattern matching and metafunction evaluation. There is a separate cache for
  each pattern and metafunction; when one fills (see @scheme[set-cache-size!]),
  Redex evicts all of the entries in that cache.
                                         
  Caching should be disabled when matching a pattern that depends on values
  other than the in-scope pattern variables or evaluating a metafunction
  that reads or writes mutable external state.
}

@defproc[(set-cache-size! [size positive-integer?]) void?]{                                                           
Changes the size of the per-pattern and per-metafunction caches. The default size is @scheme[350].
}

@section{Terms}

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

Object language expressions in Redex are written using
@scheme[term]. It is similar to Scheme's @scheme[quote] (in
many cases it is identical) in that it constructs lists as
the visible representation of terms. 

The grammar of @deftech{term}s is (note that an ellipsis
stands for repetition unless otherwise indicated):

@(schemegrammar* #:literals (in-hole hole unquote unquote-splicing) 
   [term identifier
         (term-sequence ...)
         ,scheme-expression
         (in-hole term term)
         hole
         #t #f
         string]
   [term-sequence 
     term
     ,@scheme-expression
     (code:line ... (code:comment "literal ellipsis"))])

@itemize[

@item{A term written @scheme[_identifier] is equivalent to the
corresponding symbol, unless the identifier is bound by
@scheme[term-let] (or in a @|pattern| elsewhere) or is
@tt{hole} (as below).  }

@item{A term written @scheme[(_term-sequence ...)] constructs a list of
the terms constructed by the sequence elements.}

@item{A term written @scheme[,_scheme-expression] evaluates the
@scheme[scheme-expression] and substitutes its value into the term at
that point.}

@item{A term written @scheme[,@_scheme-expression] evaluates the
@scheme[scheme-expression], which must produce a list. It then splices
the contents of the list into the expression at that point in the sequence.}

@item{A term written @scheme[(in-hole @|tttterm| @|tttterm|)]
 is the dual to the @pattern @scheme[in-hole] -- it accepts
 a context and an expression and uses @scheme[plug] to combine
them.}

@item{A term written @scheme[hole] produces a hole.}

@item{A term written as a literal boolean or a string
produces the boolean or the string.}
]

@defform[(term @#,tttterm)]{

This form is used for construction of a term.

It behaves similarly to @scheme[quasiquote], except for a few special
forms that are recognized (listed below) and that names bound by
@scheme[term-let] are implicitly substituted with the values that
those names were bound to, expanding ellipses as in-place sublists (in
the same manner as syntax-case patterns).

For example,

@schemeblock[
(term-let ([body '(+ x 1)]
           [(expr ...) '(+ - (values * /))]
           [((id ...) ...) '((a) (b) (c d))])
  (term (let-values ([(id ...) expr] ...) body)))
]

evaluates to

@schemeblock[
'(let-values ([(a) +] 
              [(b) -] 
              [(c d) (values * /)]) 
   (+ x 1))
]

It is an error for a term variable to appear in an
expression with an ellipsis-depth different from the depth
with which it was bound by @scheme[term-let]. It is also an error
for two @scheme[term-let]-bound identifiers bound to lists of
different lengths to appear together inside an ellipsis.
}

@defidform[hole]{ Recognized specially within
  @scheme[term]. A @scheme[hole] form is an
  error elsewhere.  }

@defidform[in-hole]{ Recognized specially within
  @scheme[reduction-relation]. An @scheme[in-hole] form is an
  error elsewhere.  }

@defform/subs[(term-let ([tl-pat expr] ...) body)
              ([tl-pat identifier (tl-pat-ele ...)]
               [tl-pat-ele tl-pat (code:line tl-pat ... (code:comment "a literal ellipsis"))])]{

Matches each given id pattern to the value yielded by
evaluating the corresponding expr and binds each variable in
the id pattern to the appropriate value (described
below). These bindings are then accessible to the @|tttterm|
syntactic form.

Note that each ellipsis should be the literal symbol consisting of
three dots (and the ... elsewhere indicates repetition as usual). If
@scheme[tl-pat] is an identifier, it matches any value and binds it to
the identifier, for use inside @scheme[term]. If it is a list, it
matches only if the value being matched is a list value and only if
every subpattern recursively matches the corresponding list
element. There may be a single ellipsis in any list pattern; if one is
present, the pattern before the ellipses may match multiple adjacent
elements in the list value (possibly none).

This form is a lower-level form in Redex, and not really designed to
be used directly. If you want a @scheme[let]-like form that uses
Redex's full pattern matching facilities, see @scheme[term-match] and
@scheme[term-match/single].

}

@defform[(term-match language [@#,ttpattern expression] ...)]{

This produces a procedure that accepts term (or quoted)
expressions and checks them against each pattern. The
function returns a list of the values of the expression
where the pattern matches. If one of the patterns matches
multiple times, the expression is evaluated multiple times,
once with the bindings in the pattern for each match.

When evaluating a @scheme[term-match] expression, the patterns are
compiled in an effort to speed up matching. Using the procedural
result multiple times to avoid compiling the patterns multiple times.
}

@defform[(term-match/single language [@#,ttpattern expression] ...)]{

This produces a procedure that accepts term (or quoted)
expressions and checks them against each pattern. The
function returns the expression behind the first sucessful
match. If that pattern produces multiple matches, an error
is signaled. If no patterns match, an error is signaled.

Raises an exception recognized by @scheme[exn:fail:redex?] if
no clauses match or if one of the clauses matches multiple ways.

When evaluating a @scheme[term-match/single] expression, the patterns
are compiled in an effort to speed up matching. Using the procedural
result multiple times to avoid compiling the patterns multiple times.
}

@defproc[(plug [context any/c] [expression any/c]) any]{

The first argument to this function is an sexpression to
plug into. The second argument is the sexpression to replace
in the first argument. It returns the replaced term. This is
also used when a @scheme[term] sub-expression contains @tt{in-hole}.
}

@defproc[(variable-not-in [t any/c] [var symbol?]) symbol?]{

This helper function accepts an sexpression and a
variable. It returns a variable not in the sexpression with
a prefix the same as the second argument.

}

@defproc[(variables-not-in [t any/c] [vars (listof symbol?)]) (listof symbol?)]{

This function, like variable-not-in, makes variables that do
no occur in its first argument, but it returns a list of
such variables, one for each variable in its second
argument. 

Does not expect the input symbols to be distinct, but does
produce variables that are always distinct.
}

@defproc[(exn:fail:redex? [v any/c]) boolean?]{
  Returns @scheme[#t] if its argument is a Redex exception record, and
  @scheme[#f] otherwise.
}

@section{Languages}

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

@defform/subs[(define-language lang-name 
                (non-terminal-spec @#,ttpattern ...)
                ...)
              ([non-terminal-spec symbol (symbol ...)])]{

This form defines the grammar of a language. It allows the
definition of recursive @|pattern|s, much like a BNF, but for
regular-tree grammars. It goes beyond their expressive
power, however, because repeated @scheme[name] @|pattern|s and
side-conditions can restrict matches in a context-sensitive
way.

The non-terminal-spec can either by a symbol, indicating a
single name for this non-terminal, or a sequence of symbols,
indicating that all of the symbols refer to these
productions.

As a simple example of a grammar, this is the lambda
calculus:

@schemeblock[
  (define-language lc-lang
    (e (e e ...)
       x
       v)
    (c (v ... c e ...)
       hole)
    (v (lambda (x ...) e))
    (x variable-not-otherwise-mentioned))
]

with non-terminals @scheme[e] for the expression language, @scheme[x] for
variables, @scheme[c] for the evaluation contexts and @scheme[v] for values.
}

@defform[(define-extended-language language language
           (non-terminal @#,ttpattern ...)
           ...)]{

This form extends a language with some new, replaced, or
extended non-terminals. For example, this language:

@schemeblock[
  (define-extended-language lc-num-lang
    lc-lang
    (v ....     (code:comment "extend the previous `v' non-terminal")
       +
       number)
    (x (variable-except lambda +)))
]

extends lc-lang with two new alternatives for the @scheme[v]
non-terminal, carries forward the @scheme[e] and @scheme[c]
non-terminals, and replaces the @scheme[x] non-terminal with a
new one (which happens to be equivalent to the one that would 
have been inherited).

The four-period ellipses indicates that the new language's
non-terminal has all of the alternatives from the original
language's non-terminal, as well as any new ones. If a
non-terminal occurs in both the base language and the
extension, the extension's non-terminal replaces the
originals. If a non-terminal only occurs in either the base
language, then it is carried forward into the
extension. And, of course, extend-language lets you add new
non-terminals to the language.

If a language is has a group of multiple non-terminals
defined together, extending any one of those non-terminals
extends all of them.
}

@defproc[(language-nts [lang compiled-lang?]) (listof symbol?)]{

Returns the list of non-terminals (as symbols) that are
defined by this language.
}

@defproc[(compiled-lang? [l any/c]) boolean?]{

Returns @scheme[#t] if its argument was produced by @scheme[language], @scheme[#f]
otherwise.
}

@section{Reduction Relations}

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

@defform/subs[#:literals (--> fresh side-condition where) 
              (reduction-relation language domain main-arrow reduction-case ...)
              ([domain (code:line) (code:line #:domain @#,ttpattern)]
               [main-arrow (code:line) (code:line #:arrow arrow)]
               [reduction-case (--> @#,ttpattern @#,tttterm extras ...)]
               [extras name
                       (fresh fresh-clause ...)
                       (side-condition scheme-expression)
                       (where tl-pat @#,tttterm)]
               [fresh-clause var ((var1 ...) (var2 ...))]
               [tl-pat identifier (tl-pat-ele ...)]
               [tl-pat-ele tl-pat (code:line tl-pat ... (code:comment "a literal ellipsis"))])]{

Defines a reduction relation casewise, one case for each of the
clauses beginning with @scheme[-->] (or with @scheme[arrow], if
specified). Each of the @scheme[pattern]s
refers to the @scheme[language], and binds variables in the
@|tttterm|. 

Following the @|pattern| and @|tterm| can be the name of the
reduction rule, declarations of some fresh variables, and/or
some side-conditions. The name can either be a literal
name (identifier), or a literal string. 

The fresh variables clause generates variables that do not
occur in the term being matched. If the @scheme[fresh-clause] is a
variable, that variable is used both as a binding in the
rhs-exp and as the prefix for the freshly generated
variable. (The variable does not have to be
a non-terminal in the language of the reduction relation.)

The second case of a @scheme[fresh-clause] is used when you want to
generate a sequence of variables. In that case, the ellipses
are literal ellipses; that is, you must actually write
ellipses in your rule. The variable @scheme[var1] is like the
variable in first case of a @scheme[fresh-clause], namely it is
used to determine the prefix of the generated variables and
it is bound in the right-hand side of the reduction rule,
but unlike the single-variable fresh clause, it is bound to
a sequence of variables. The variable @scheme[var2] is used to
determine the number of variables generated and @scheme[var2] must be
bound by the left-hand side of the rule.

The side-conditions are expected to all hold, and have the
format of the second argument to the @pattech[side-condition] pattern,
described above.

Each @scheme[where] clauses binds a variable and the side-conditions
(and @scheme[where] clauses) that follow the where declaration are in
scope of the where declaration. The bindings are the same as
bindings in a @scheme[term-let] expression.

As an example, this

@schemeblock[
  (reduction-relation
   lc-lang
   (--> (in-hole c_1 ((lambda (variable_i ...) e_body) v_i ...))
        (in-hole c_1 ,(foldl lc-subst 
                             (term e_body) 
                             (term (v_i ...)) 
                             (term (variable_i ...))))
        beta-v))
]

defines a reduction relation for the lambda-calculus above.
}

@defform/none[#:literals (with reduction-relation)
         (reduction-relation 
          language
          (arrow-var @#,ttpattern @#,tttterm) ...
          with
          [(arrow @#,ttpattern @#,tttterm)
           (arrow-var var var)] ...)]{

Defines a reduction relation with shortcuts. As above, the
first section defines clauses of the reduction relation, but
instead of using @scheme[-->], those clauses can use any identifier
for an arrow, as long as the identifier is bound after the
@scheme[with] clause. 

Each of the clauses after the @scheme[with] define new relations
in terms of other definitions after the @scheme[with] clause or in
terms of the main @scheme[-->] relation.

A @scheme[fresh] variable is always fresh with respect to the entire
term, not just with respect to the part that matches the
right-hand-side of the newly defined arrow.

As an example, this

@schemeblock[
  (reduction-relation
   lc-num-lang
   (==> ((lambda (variable_i ...) e_body) v_i ...)
        ,(foldl lc-subst 
                (term e_body) 
                (term (v_i ...)) 
                (term (variable_i ...))))
   (==> (+ number_1 ...)
        ,(apply + (term (number_1 ...))))
   
   with
   [(--> (in-hole c_1 a) (in-hole c_1 b))
    (==> a b)])
]
  
defines reductions for the lambda calculus with numbers,
where the @tt{==>} relation is defined by reducing in the context
@tt{c}.
}

@defform[(extend-reduction-relation reduction-relation language more ...)]{

This form extends the reduction relation in its first
argument with the rules specified in @scheme[more]. They should
have the same shape as the rules (including the @scheme[with]
clause) in an ordinary @scheme[reduction-relation].

If the original reduction-relation has a rule with the same
name as one of the rules specified in the extension, the old
rule is removed.

In addition to adding the rules specified to the existing
relation, this form also reinterprets the rules in the
original reduction, using the new language.
}
@defproc[(union-reduction-relations [r reduction-relation?] ...) 
         reduction-relation?]{

Combines all of the argument reduction relations into a
single reduction relation that steps when any of the
arguments would have stepped.
}

@defproc[(reduction-relation->rule-names [r reduction-relation?])
         (listof (union false/c symbol?))]{

Returns the names of all of the reduction relation's clauses
(or false if there is no name for a given clause).
}

@defform[(compatible-closure reduction-relation lang non-terminal)]{

This accepts a reduction, a language, the name of a
non-terminal in the language and returns the compatible
closure of the reduction for the specified non-terminal.
}

@defform[(context-closure reduction-relation lang pattern)]{

This accepts a reduction, a language, a pattern representing
a context (ie, that can be used as the first argument to
@scheme[in-hole]; often just a non-terminal) in the language and
returns the closure of the reduction in that context.
}

@defproc[(reduction-relation? [v any/c]) boolean?]{
  Returns @scheme[#t] if its argument is a reduction-relation, and
  @scheme[#f] otherwise.
}

@defproc[(apply-reduction-relation [r reduction-relation?] [t any/c]) (listof any/c)]{

This accepts reduction relation, a term, and returns a
list of terms that the term reduces to.
}

@defproc[(apply-reduction-relation/tag-with-names
          [r reduction-relation?]
          [t any/c])
         (listof (list/c (union false/c string?) any/c))]{

Like @scheme[apply-reduction-relation], but the result indicates the
names of the reductions that were used.
}

@defproc[(apply-reduction-relation*
          [r reduction-relation?]
          [t any/c])
         (listof any/c)]{

The function @scheme[apply-reduction-relation*] accepts a reduction relation and a
term. Starting from @scheme[t], it follows every reduction
path and returns all of the terms that do not reduce further.
If there are infinite reduction
sequences that do not repeat, this function will not
terminate (it does terminate if the only infinite reduction paths are cyclic).
}

@defidform[-->]{ Recognized specially within
  @scheme[reduction-relation]. A @scheme[-->] form is an
  error elsewhere.  }

@defidform[fresh]{ Recognized specially within
  @scheme[reduction-relation]. A @scheme[fresh] form is an
  error elsewhere.  }

@defidform[with]{ Recognized specially within
  @scheme[reduction-relation]. A @scheme[with] form is an
  error elsewhere.  }

@section{Metafunctions and Relations}

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

@defform/subs[#:literals (: ->)
             (define-metafunction language
               contract
               [(name @#,ttpattern ...) @#,tttterm extras ...] 
               ...)
             ([contract (code:line) 
                        (code:line id : @#,ttpattern ... -> @#,ttpattern)]
              [extras (side-condition scheme-expression)
                      (where tl-pat @#,tttterm)]
              [tl-pat identifier (tl-pat-ele ...)]
              [tl-pat-ele tl-pat (code:line tl-pat ... (code:comment "a literal ellipsis"))])]{

The @scheme[define-metafunction] form builds a function on
sexpressions according to the pattern and right-hand-side
expressions. The first argument indicates the language used
to resolve non-terminals in the pattern expressions. Each of
the rhs-expressions is implicitly wrapped in @|tttterm|. 

If specified, the side-conditions are collected with 
@scheme[and] and used as guards on the case being matched. The
argument to each side-condition should be a Scheme
expression, and the pattern variables in the @|ttpattern| are
bound in that expression.

Raises an exception recognized by @scheme[exn:fail:redex?] if
no clauses match, if one of the clauses matches multiple ways
(and that leads to different results for the different matches),
or if the contract is violated.

Note that metafunctions are assumed to always return the same results
for the same inputs, and their results are cached, unless
@scheme[caching-enabled?] is set to @scheme[#f]. Accordingly, if a
metafunction is called with the same inputs twice, then its body is
only evaluated a single time.

As an example, these metafunctions finds the free variables in
an expression in the lc-lang above:

@schemeblock[
    (define-metafunction lc-lang
      free-vars : e -> (x ...)
      [(free-vars (e_1 e_2 ...))
       (∪ (free-vars e_1) (free-vars e_2) ...)]
      [(free-vars x) (x)]
      [(free-vars (lambda (x ...) e))
       (- (free-vars e) (x ...))])
]

The first argument to define-metafunction is the grammar
(defined above). Following that are three cases, one for
each variation of expressions (e in lc-lang). The free variables of an
application are the free variables of each of the subterms;
the free variables of a variable is just the variable
itself, and the free variables of a lambda expression are
the free variables of the body, minus the bound parameters.

Here are the helper metafunctions used above.

@schemeblock[
    (define-metafunction lc-lang
      ∪ : (x ...) ... -> (x ...)
      [(∪ (x_1 ...) (x_2 ...) (x_3 ...) ...)
       (∪ (x_1 ... x_2 ...) (x_3 ...) ...)]
      [(∪ (x_1 ...))
       (x_1 ...)]
      [(∪) ()])
    
    (define-metafunction lc-lang
      - : (x ...) (x ...) -> (x ...)
      [(- (x ...) ()) (x ...)]
      [(- (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
       (- (x_1 ... x_3 ...) (x_2 x_4 ...))
       (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
      [(- (x_1 ...) (x_2 x_3 ...))
       (- (x_1 ...) (x_3 ...))])
]

Note the side-condition in the second case of @scheme[-]. It
ensures that there is a unique match for that case. Without
it, @scheme[(term (- (x x) x))] would lead to an ambiguous
match.

}

@defform[(define-metafunction/extension f language 
           contract
           [(g @#,ttpattern ...) @#,tttterm extras ...] 
           ...)]{

Defines a metafunction @scheme[g] as an extension of an existing
metafunction @scheme[f]. The metafunction @scheme[g] behaves as 
if @scheme[f]'s clauses were appended to its definition (with 
occurrences of @scheme[f] changed to @scheme[g] in the inherited
clauses).
}

For example, @scheme[define-metafunction/extension] may be used to extend
the free-vars function above to the forms introduced by the language
lc-num-lang.
                
@schemeblock[
(define-metafunction/extension free-vars lc-num-lang
  free-vars-num : e -> (x ...)
  [(free-vars-num number) 
   ()]
  [(free-vars-num (+ e_1 e_2))
   (∪ (free-vars-num e_1)
      (free-vars-num e_2))])
]
                
@defform[(in-domain? (metafunction-name @#,tttterm ...))]{
Returns @scheme[#t] if the inputs specified to @scheme[metafunction-name] are
legtimate inputs according to @scheme[metafunction-name]'s contract,
and @scheme[#f] otherwise.
}

@defform/subs[#:literals ()
              (define-relation language
               [(name @#,ttpattern ...) @#,tttterm ...] ...)
               ([tl-pat identifier (tl-pat-ele ...)]
                [tl-pat-ele tl-pat (code:line tl-pat ... (code:comment "a literal ellipsis"))])]{

The @scheme[define-relation] form builds a relation on
sexpressions according to the pattern and right-hand-side
expressions. The first argument indicates the language used
to resolve non-terminals in the pattern expressions. Each of
the rhs-expressions is implicitly wrapped in @|tttterm|. 

Relations are like metafunctions in that they are called with
arguments and return results (unlike in, say, prolog, where a relation
definition would be able to synthesize some of the arguments based on
the values of others).

Unlike metafunctions, relations check all possible ways to match each
case, looking for a true result and if none of the clauses match, then
the result is @scheme[#f]. If there are multiple expressions on
the right-hand side of a relation, then all of them must be satisfied
in order for that clause of the relation to be satisfied.

Note that relations are assumed to always return the same results for
the same inputs, and their results are cached, unless
@scheme[caching-enable?] is set to @scheme[#f]. Accordingly, if a
relation is called with the same inputs twice, then its right-hand
sides are evaluated only once.
}

@defparam[current-traced-metafunctions traced-metafunctions (or/c 'all (listof symbol?))]{

Controls which metafunctions are currently being traced. If it is
@scheme['all], all of them are. Otherwise, the elements of the list
name the metafunctions to trace. 

Defaults to @scheme['()].

}

@section{Testing}

All of the exports in this section are provided both by
@schememodname[redex/reduction-semantics] (which includes
all non-GUI portions of Redex) and also exported by
@schememodname[redex] (which includes all of Redex).

@defform[(test-equal e1 e2)]{

Tests to see if @scheme[e1] is equal to @scheme[e2].
}

@defform/subs[(test-->> reduction-relation maybe-cycles e1 e2 ...)
              ([cycles (code:line) #:cycles-ok])]{

Tests to see if the value of @scheme[e1] (which should be a term),
reduces to the @scheme[e2]s under @scheme[reduction-relation]
(using @scheme[apply-reduction-relation*], so it may not terminate).
}

@defform[(test--> reduction-relation e1 e2 ...)]{

Tests to see if the value of @scheme[e1] (which should be a term),
reduces to the @scheme[e2]s in a single step, under @scheme[reduction-relation]
(using @scheme[apply-reduction-relation]).
}

@defform[(test-predicate p? e)]{
Tests to see if the value of @scheme[e] matches the predicate @scheme[p?].
}

@defproc[(test-results) void?]{
Prints out how many tests passed and failed, and resets the
counters so that next time this function is called, it
prints the test results for the next round of tests.
}

@defform/subs[(make-coverage subject)
              ([subject (code:line metafunction)
                        (code:line relation-expr)])]{
Constructs a structure (recognized by @scheme[coverage?])
to contain per-case test coverage of the supplied metafunction
or reduction relation. Use with @scheme[relation-coverage] and 
@scheme[covered-cases].
}

@defproc[(coverage? [v any/c]) boolean?]{
Returns @scheme[#t] for a value produced by @scheme[make-coverage]
and @scheme[#f] for any other.}

@defparam[relation-coverage tracked (listof coverage?)]{
Redex populates the coverage records in @scheme[tracked] (default @scheme[null]),
counting the times that tests exercise each case of the associated metafunction
and relations.}

@defproc[(covered-cases 
          [c coverage?])
         (listof (cons/c string? natural-number/c))]{
Extracts the coverage information recorded in @scheme[c], producing
an association list mapping names (or source locations, in the case of
metafunctions or unnamed reduction-relation cases) to application counts.}

@examples[
#:eval redex-eval
       (define-language empty-lang)
       
       (define-metafunction empty-lang
         [(plus number_1 number_2)
          ,(+ (term number_1) (term number_2))])
       
       (define equals
         (reduction-relation 
          empty-lang
          (--> (+) 0 "zero")
          (--> (+ number) number)
          (--> (+ number_1 number_2 number ...)
               (+ (plus number_1 number_2)
                  number ...)
               "add")))
       (let ([equals-coverage (make-coverage equals)]
             [plus-coverage (make-coverage plus)])
         (parameterize ([relation-coverage (list equals-coverage 
                                                 plus-coverage)])
           (apply-reduction-relation* equals (term (+ 1 2 3)))
           (values (covered-cases equals-coverage)
                   (covered-cases plus-coverage))))]

@defform*/subs[[(generate-term language @#,ttpattern size-expr kw-args ...)
                (generate-term language @#,ttpattern)]
              ([kw-args (code:line #:attempt-num attempts-expr)
                        (code:line #:retries retries-expr)])
              #:contracts ([size-expr natural-number/c]
                           [attempt-num-expr natural-number/c]
                           [retries-expr natural-number/c])]{
                                                             
In its first form, @scheme[generate-term] produces a random term matching
the given pattern (according to the given language). In its second, 
@scheme[generate-term] produces a procedure for constructing the same.
This procedure expects @scheme[size-expr] (below) as its sole positional
argument and allows the same optional keyword arguments as the first form.
The second form may be more efficient when generating many terms.

The argument @scheme[size-expr] bounds the height of the generated term
(measured as the height of its parse tree). 
         
The optional keyword argument @scheme[attempt-num-expr] 
(default @scheme[1]) provides coarse grained control over the random
decisions made during generation; increasing @scheme[attempt-num-expr]
tends to increase the complexity of the result. For example, the expected
length of @pattech[pattern-sequence]s increases with @scheme[attempt-num-expr].

The random generation process does not actively consider the constraints
imposed by @pattech[side-condition] or @tt{_!_} @|pattern|s; instead, 
it uses a ``guess and check'' strategy in which it freely generates 
candidate terms then tests whether they happen to satisfy the constraints,
repeating as necessary. The optional keyword argument @scheme[retries-expr]
(default @scheme[100]) bounds the number of times that 
@scheme[generate-term] retries the generation of any pattern. If 
@scheme[generate-term] is unable to produce a satisfying term after 
@scheme[retries-expr] attempts, it raises an exception recognized by
@scheme[exn:fail:redex:generation-failure?].}
                                                            
@defform/subs[(redex-check language @#,ttpattern property-expr kw-arg ...)
              ([kw-arg (code:line #:attempts attempts-expr)
                       (code:line #:source metafunction)
                       (code:line #:source relation-expr)
                       (code:line #:retries retries-expr)
                       (code:line #:print? print?-expr)])
              #:contracts ([property-expr any/c]
                           [attempts-expr natural-number/c]
                           [relation-expr reduction-relation?]
                           [retries-expr natural-number/c]
                           [print?-expr any/c])]{
Searches for a counterexample to @scheme[property-expr], interpreted
as a predicate universally quantified over the pattern variables
bound by @scheme[pattern]. @scheme[redex-check] constructs and tests 
a candidate counterexample by choosing a random term @math{t} that 
matches @scheme[pattern] then evaluating @scheme[property-expr]
using the @scheme[match-bindings] produced by @scheme[match]ing
@math{t} against @scheme[pattern].

@scheme[redex-check] generates at most @scheme[attempts-expr] (default @scheme[1000])
random terms in its search. The size and complexity of these terms increase with 
each failed attempt. 

When @scheme[print?-expr] produces any non-@scheme[#f] value (the default), 
@scheme[redex-check] prints the test outcome on @scheme[current-output-port].
When @scheme[print?-expr] produces @scheme[#f], @scheme[redex-check] prints
nothing, instead 
@itemlist[
  @item{returning a @scheme[counterexample] structure when the test reveals a counterexample,}
  @item{returning @scheme[#t] when all tests pass, or}
  @item{raising a @scheme[exn:fail:redex:test] when checking the property raises an exception.}
]

When passed a metafunction or reduction relation via the optional @scheme[#:source]
argument, @scheme[redex-check] distributes its attempts across the left-hand sides
of that metafunction/relation by using those patterns, rather than @scheme[pattern],
as the basis of its generation. It is an error if any left-hand side generates a
term that does not match @scheme[pattern].}

@examples[
#:eval redex-eval
       (define-language empty-lang)
       
       (random-seed 0)

       (redex-check 
        empty-lang
        ((number_1 ...)
         (number_2 ...))
        (equal? (reverse (append (term (number_1 ...))
                                 (term (number_2 ...))))
                (append (reverse (term (number_1 ...)))
                        (reverse (term (number_2 ...))))))

       (redex-check 
        empty-lang
        ((number_1 ...)
         (number_2 ...))
        (equal? (reverse (append (term (number_1 ...))
                                 (term (number_2 ...))))
                (append (reverse (term (number_2 ...)))
                        (reverse (term (number_1 ...)))))
        #:attempts 200)
       
       (let ([R (reduction-relation 
                 empty-lang
                 (--> (Σ) 0)
                 (--> (Σ number) number)
                 (--> (Σ number_1 number_2 number_3 ...)
                      (Σ ,(+ (term number_1) (term number_2)) 
                         number_3 ...)))])
         (redex-check
          empty-lang
          (Σ number ...)
          (printf "~s\n" (term (number ...)))
          #:attempts 3
          #:source R))]

@defstruct[counterexample ([term any/c]) #:inspector #f]{
Produced by @scheme[redex-check], @scheme[check-reduction-relation], and 
@scheme[check-metafunction] when testing falsifies a property.}

@defstruct[(exn:fail:redex:test exn:fail:redex) ([source exn:fail?] [term any/c])]{
Raised by @scheme[redex-check], @scheme[check-reduction-relation], and 
@scheme[check-metafunction] when testing a property raises an exception.
The @scheme[exn:fail:redex:test-source] component contains the exception raised by the property,
and the @scheme[exn:fail:redex:test-term] component contains the term that induced the exception.}

@defform/subs[(check-reduction-relation relation property kw-args ...)
              ([kw-arg (code:line #:attempts attempts-expr)
                       (code:line #:retries retries-expr)])
              #:contracts ([property (-> any/c any/c)]
                           [attempts-expr natural-number/c]
                           [retries-expr natural-number/c])]{
Tests @scheme[relation] as follows: for each case of @scheme[relation],
@scheme[check-reduction-relation] generates @scheme[attempts] random
terms that match that case's left-hand side and applies @scheme[property] 
to each random term.

This form provides a more convenient notation for
@schemeblock[(redex-check L any (property (term any)) 
                          #:attempts (* n attempts)
                          #:source relation)]
when @scheme[relation] is a relation on @scheme[L] with @scheme[n] rules.}

@defform/subs[(check-metafunction metafunction property kw-args ...)
              ([kw-arg (code:line #:attempts attempts-expr)
                       (code:line #:retries retries-expr)])
              #:contracts ([property (-> (listof any/c) any/c)]
                           [attempts-expr natural-number/c]
                           [retries-expr natural-number/c])]{
Like @scheme[check-reduction-relation] but for metafunctions. 
@scheme[check-metafunction] calls @scheme[property] with lists
containing arguments to the metafunction.}

@examples[
#:eval redex-eval
       (define-language empty-lang)
       
       (random-seed 0)

       (define-metafunction empty-lang
         Σ : number ... -> number
         [(Σ) 0]
         [(Σ number) number]
         [(Σ number_1 number_2 number_3 ...) 
          (Σ ,(+ (term number_1) (term number_2)) number_3 ...)])
       
       (check-metafunction Σ (λ (args) (printf "~s\n" args)) #:attempts 2)]
                                                            
@defproc[(exn:fail:redex:generation-failure? [v any/c]) boolean?]{
  Recognizes the exceptions raised by @scheme[generate-term], 
  @scheme[redex-check], etc. when those forms are unable to produce
  a term matching some pattern.
}
                                                            
@deftech{Debugging PLT Redex Programs}

It is easy to write grammars and reduction rules that are
subtly wrong and typically such mistakes result in examples
that just get stuck when viewed in a @scheme[traces] window.

The best way to debug such programs is to find an expression
that looks like it should reduce but doesn't and try to find
out what pattern is failing to match. To do so, use the
@scheme[redex-match] special form, described above.

In particular, first ceck to see if the term matches the
main non-terminal for your system (typically the expression
or program nonterminal). If it does not, try to narrow down
the expression to find which part of the term is failing to
match and this will hopefully help you find the problem. If
it does match, figure out which reduction rule should have
matched, presumably by inspecting the term. Once you have
that, extract a pattern from the left-hand side of the
reduction rule and do the same procedure until you find a
small example that shoudl work but doesn't (but this time
you might also try simplifying the pattern as well as
simplifying the expression).


@section{GUI}
@defmodule[redex/gui]

This section describes the GUI tools that Redex provides for
exploring reduction sequences.

@defproc[(traces [reductions reduction-relation?] 
                 [expr (or/c any/c (listof any/c))]
                 [#:multiple? multiple? boolean? #f]
                 [#:pred pred
                         (or/c (-> sexp any)
                               (-> sexp term-node? any))
                         (lambda (x) #t)]
                 [#:pp pp
                       (or/c (any -> string)
                             (any output-port number (is-a?/c text%) -> void))
                       default-pretty-printer]
                 [#:colors colors 
                  (listof 
                   (cons/c string 
                           (and/c (listof (or/c string? (is-a?/c color%)))
                                  (lambda (x) (member (length x) '(2 3 4 6))))))]

                 [#:scheme-colors? scheme-colors? boolean? #t]
                 [#:filter term-filter (-> any/c (or/c #f string?) any/c) (lambda (x y) #t)]
                 [#:x-spacing number? 15]
                 [#:y-spacing number? 15]
                 [#:layout layout (-> (listof term-node?) void) void]
                 [#:edge-labels? edge-label-font boolean? #t]
                 [#:edge-label-font edge-label-font (or/c #f (is-a?/c font%)) #f]
                 [#:graph-pasteboard-mixin graph-pasteboard-mixin (make-mixin-contract graph-pasteboard<%>) values])
         void?]{

This function opens a new window and inserts each expression
in expr (if @scheme[multiple?] is #t -- if
@scheme[multiple?] is #f, then expr is treated as a single
expression). Then, it reduces the terms until at least
@scheme[reduction-steps-cutoff] (see below) different terms are
found, or no more reductions can occur. It inserts each new
term into the gui. Clicking the @onscreen{reduce} button reduces
until reduction-steps-cutoff more terms are found.

The @scheme[pred] function indicates if a term has a particular
property. If it returns @scheme[#f], the term is displayed with a
pink background. If it returns a string or a @scheme[color%] object,
the term is displayed with a background of that color (using
@scheme[the-color-database] to map the string to a color). If it
returns any other value, the term is displayed normally. If
the pred function accepts two arguments, a term-node
corresponding to the term is passed to the predicate. This
lets the predicate function explore the (names of the)
reductions that led to this term, using term-node-children,
term-node-parents, and term-node-labels.

The @scheme[pred] function may be called more than once per node. In
particular, it is called each time an edge is added to a
node. The latest value returned determines the color.

The @scheme[pp] function is used to specially print expressions. It
must either accept one or four arguments. If it accepts one
argument, it will be passed each term and is expected to
return a string to display the term.

If the @scheme[pp] function takes four arguments, it should render
its first argument into the port (its second argument) with
width at most given by the number (its third argument). The
final argument is the text where the port is connected --
characters written to the port go to the end of the editor.

The @scheme[colors] argument, if provided, specifies a list of
reduction-name/color-list pairs. The traces gui will color arrows
drawn because of the given reduction name with the given color instead
of using the default color.

The @scheme[cdr] of each of the elements of @scheme[colors] is a list
of colors, organized in pairs. The first two colors cover the colors
of the line and the border around the arrow head, the first when the
mouse is over a graph node that is connected to that arrow, and the
second for when the mouse is not over that arrow. Similarly, the next
colors are for the text drawn on the arrow and the last two are for
the color that fills the arrow head.  If fewer than six colors are
specified, the colors specified colors are used and then defaults are
filled in for the remaining colors.

The @scheme[scheme-colors?] argument, if @scheme[#t] causes
@scheme[traces] to color the contents of each of the windows according
to DrScheme's Scheme mode color Scheme. If it is @scheme[#f],
@scheme[traces] just uses black for the color scheme. 
In addition, Scheme-mode parenthesis highlighting is
enabled when @scheme[scheme-colors?]
is @scheme[#t] and not when it is @scheme[#f].

The @scheme[term-filter] function is called each time a new node is
about to be inserted into the graph. If the filter returns false, the
node is not inserted into the graph.

The @scheme[x-spacing] and @scheme[y-spacing] control the amount of
space put between the snips in the default layout.

The @scheme[layout] argument is called (with all of the terms) when
new terms is inserted into the window. In general, it is called when
after new terms are inserted in response to the user clicking on the
reduce button, and after the initial set of terms is inserted.
See also @scheme[term-node-set-position!].

If @scheme[edge-labels?] is @scheme[#t] (the default), then edge labels
are drawn; otherwise not.

The @scheme[edge-label-font] argument is used as the font on the edge
labels. If @scheme[#f] is suppled, the @scheme[dc<%>] object's default
font is used.

The traces library an instance of the @schememodname[mrlib/graph]
library's @scheme[graph-pasteboard<%>] interface to layout
the graphs.  Sometimes, overriding one of its methods can
help give finer-grained control over the layout, so the
@scheme[graph-pasteboard-mixin] is applied to the class
before it is instantiated. Also note that all of the snips
inserted into the editor by this library have a
@tt{get-term-node} method which returns the snip's
@scheme[term-node].

}

@defproc[(traces/ps [reductions reduction-relation?] 
                    [expr (or/c any/c (listof any/c))]
                    [file (or/c path-string? path?)]
                    [#:multiple? multiple? boolean? #f]
                    [#:pred pred
                            (or/c (-> sexp any)
                                  (-> sexp term-node? any))
                            (lambda (x) #t)]
                    [#:pp pp
                          (or/c (any -> string)
                                (any output-port number (is-a?/c text%) -> void))
                          default-pretty-printer]
                    [#:colors colors (listof (list string string)) '()]
                    [#:filter term-filter (-> any/c (or/c #f string?) any/c) (lambda (x y) #t)]
                    [#:layout layout (-> (listof term-node?) void) void]
                    [#:x-spacing number? 15]
                    [#:y-spacing number? 15]
                    [#:edge-labels? edge-label-font boolean? #t]
                    [#:edge-label-font edge-label-font (or/c #f (is-a?/c font%)) #f]
                    [#:graph-pasteboard-mixin graph-pasteboard-mixin (make-mixin-contract graph-pasteboard<%>) values]
                    [#:post-process post-process (-> (is-a?/c graph-pasteboard<%>) any/c)])
         void?]{

This function  behaves just like the function @scheme[traces], but
instead of opening a window to show the reduction graph, it just saves
the reduction graph to the specified @scheme[file].

All of the arguments behave like the arguments to @scheme[traces],
with the exception of the @scheme[post-process] argument. It is called
just before the PostScript is created with the graph pasteboard.
}

@defproc[(stepper [reductions reduction-relation?] 
                  [t any/c] 
                  [pp (or/c (any -> string)
                            (any output-port number (is-a?/c text%) -> void))
                      default-pretty-printer])
          void?]{

This function opens a stepper window for exploring the
behavior of its third argument in the reduction system
described by its first two arguments. 

The @scheme[pp] argument is the same as to the
@scheme[traces] functions (above).
}

@defproc[(stepper/seed [reductions reduction-relation?]
                       [seed (cons/c any/c (listof any/c))]
                       [pp (or/c (any -> string)
                                 (any output-port number (is-a?/c text%) -> void))
                           default-pretty-printer])
         void?]{

Like @scheme[stepper], this function opens a stepper window, but it
seeds it with the reduction-sequence supplied in @scheme[seed].
}

@defproc[(term-node-children [tn term-node?]) (listof term-node?)]{

Returns a list of the children (ie, terms that this term
reduces to) of the given node.

Note that this function does not return all terms that this
term reduces to -- only those that are currently in the
graph.
}

@defproc[(term-node-parents [tn term-node?]) (listof term-node?)]{

Returns a list of the parents (ie, terms that reduced to the
current term) of the given node.

Note that this function does not return all terms that
reduce to this one -- only those that are currently in the
graph.
}
@defproc[(term-node-labels [tn term-node]) (listof (or/c false/c string?))]{

Returns a list of the names of the reductions that led to
the given node, in the same order as the result of
term-node-parents. If the list contains @scheme[#f], that means that
the corresponding step does not have a label.
}

@defproc[(term-node-set-color! [tn term-node?] [color (or/c string? (is-a?/c color%) false/c)]) void?]{

Changes the highlighting of the node; if its second argument
is @scheme[#f], the coloring is removed, otherwise the color is set
to the specified @scheme[color%] object or the color named by the
string. The @scheme[color-database<%>] is used to convert the string
to a @scheme[color%] object.
}

@defproc[(term-node-color [tn term-node?]) (or/c string? (is-a?/c color%) false/c)]{

Returns the current highlighting of the node. See also @scheme[term-node-set-color!].
}


@defproc[(term-node-set-red! [tn term-node?] [red? boolean?]) void?]{

Changes the highlighting of the node; if its second argument
is @scheme[#t], the term is colored pink, if it is @scheme[#f], the term is
not colored specially.

}

@defproc[(term-node-expr [tn term-node?]) any]{

Returns the expression in this node.
}

@defproc[(term-node-set-position! [tn term-node?] [x (and/c real? positive?)] [y (and/c real? positive?)]) void?]{

Sets the position of @scheme[tn] in the graph to (@scheme[x],@scheme[y]). 
}

@defproc[(term-node-x [tn term-node?]) real]{
Returns the @tt{x} coordinate of @scheme[tn] in the window.
}
@defproc[(term-node-y [tn term-node?]) real]{
Returns the @tt{y} coordinate of @scheme[tn] in the window.
}
@defproc[(term-node-width [tn term-node?]) real]{
Returns the width of @scheme[tn] in the window.
}
@defproc[(term-node-height [tn term-node?]) real?]{
Returns the height of @scheme[tn] in the window.
}

@defproc[(term-node? [v any/c]) boolean?]{

Recognizes term nodes.
}

@defparam[reduction-steps-cutoff cutoff number?]{

A parameter that controls how many steps the @scheme[traces] function
takes before stopping.
}

@defparam[initial-font-size size number?]{

A parameter that controls the initial font size for the terms shown
in the GUI window.
}

@defparam[initial-char-width width (or/c number? (-> any/c number?))]{

A parameter that determines the initial width of the boxes
where terms are displayed (measured in characters) for both
the stepper and traces.

If its value is a number, then the number is used as the width for 
every term. If its value is a function, then the function is called
with each term and the resulting number is used as the width.
}

@deftogether[[
@defparam[dark-pen-color color (or/c string? (is-a?/c color<%>))]{}
@defparam[dark-brush-color color (or/c string? (is-a?/c color<%>))]{}
@defparam[light-pen-color color (or/c string? (is-a?/c color<%>))]{}
@defparam[light-brush-color color (or/c string? (is-a?/c color<%>))]{}
@defparam[dark-text-color color (or/c string? (is-a?/c color<%>))]{}
@defparam[light-text-color color (or/c string? (is-a?/c color<%>))]{}]]{

These six parameters control the color of the edges in the graph.

The dark colors are used when the mouse is over one of the nodes that
is connected to this edge. The light colors are used when it isn't.

The pen colors control the color of the line. The brush colors control
the color used to fill the arrowhead and the text colors control the
color used to draw the label on the edge.
}

@defproc[(default-pretty-printer [v any] [port output-port] [width number] [text (is-a?/c text%)]) void?]{

This is the default value of @scheme[pp] used by @scheme[traces] and
@scheme[stepper] and it uses
@scheme[pretty-print].

It sets the @scheme[pretty-print-columns] parameter to
@scheme[width], and it sets @scheme[pretty-print-size-hook]
and @scheme[pretty-print-print-hook] to print holes and the
symbol @scheme['hole] to match the way they are input in a
@scheme[term] expression.

}

@section{Typesetting}

@defmodule[redex/pict]

The @schememodname[redex/pict] library provides functions
designed to automatically typeset grammars, reduction
relations, and metafunction written with plt redex.

Each grammar, reduction relation, and metafunction can be
saved in a .ps file (as encapsulated postscript), or can be
turned into a pict for viewing in the REPL or using with
Slideshow (see 
@other-manual['(lib "scribblings/slideshow/slideshow.scrbl")]).

@subsection{Picts & PostScript}

This section documents two classes of operations, one for
direct use of creating postscript figures for use in papers
and for use in DrScheme to easily adjust the typesetting:
@scheme[render-term],
@scheme[render-language],
@scheme[render-reduction-relation], 
@scheme[render-metafunctions], and
@scheme[render-lw], 
and one
for use in combination with other libraries that operate on picts
@scheme[term->pict],
@scheme[language->pict],
@scheme[reduction-relation->pict],
@scheme[metafunction->pict], and
@scheme[lw->pict].
The primary difference between these functions is that the former list
sets @scheme[dc-for-text-size] and the latter does not.

@defproc[(render-term [lang compiled-lang?] [term any/c] [file (or/c #f path-string?)]) 
         (if file void? pict?)]{
  Renders the term @scheme[term]. If @scheme[file] is @scheme[#f],
  it produces a pict; if @scheme[file] is a path,  it saves
  Encapsulated PostScript in the provided filename. See
  @scheme[render-language] for details on the construction of the pict.
  }

                               
@defproc[(term->pict [lang compiled-lang?] [term any/c]) pict?]{
 Produces a pict like @scheme[render-term], but without
 adjusting @scheme[dc-for-text-size].

 This function is primarily designed to be used with
 Slideshow or with other tools that combine picts together.
}

                               
@defproc[(render-language [lang compiled-lang?]
                          [file (or/c false/c path-string?) #f]
                          [#:nts nts (or/c false/c (listof (or/c string? symbol?)))
                           (render-language-nts)])
         (if file void? pict?)]{

Renders a language. If @scheme[file] is @scheme[#f],
it produces a pict; if @scheme[file] is a path, it saves
Encapsulated PostScript in the provided filename. See
@scheme[render-language-nts] for information on the
@scheme[nts] argument.

This function parameterizes @scheme[dc-for-text-size] to install a
relevant dc: a @scheme[bitmap-dc%] or a @scheme[post-script-dc%], depending on
whether @scheme[file] is a path.

See @scheme[language->pict] if you are using Slideshow or
are otherwise setting @scheme[dc-for-text-size].  }

@defproc[(language->pict (lang compiled-lang?)
                         [#:nts nts (or/c false/c (listof (or/c string? symbol?)))
                          (render-language-nts)])
         pict?]{

Produce a pict like @scheme[render-language], but without
adjusting @scheme[dc-for-text-size].

This function is primarily designed to be used with
Slideshow or with other tools that combine picts together.
}

@defproc[(render-reduction-relation [rel reduction-relation?]
                                    [file (or/c false/c path-string?) #f]
                                    [#:style style reduction-rule-style/c (rule-pict-style)])
         (if file void? pict?)]{

Renders a reduction relation. If @scheme[file] is @scheme[#f],
it produces a pict; if @scheme[file] is a path, it saves
Encapsulated PostScript in the provided filename. See
@scheme[rule-pict-style] for information on the
@scheme[style] argument.

This function parameterizes @scheme[dc-for-text-size] to install a
relevant dc: a @scheme[bitmap-dc%] or a @scheme[post-script-dc%], depending on
whether @scheme[file] is a path. See also
@scheme[reduction-relation->pict].

The following forms of arrows can be typeset: 

@arrows[--> -+> ==> -> => ..> >-> ~~> ~> :-> :--> c->
        -->> >-- --< >>-- --<<]

}

@defproc[(reduction-relation->pict (r reduction-relation?)
                                   [#:style style reduction-rule-style/c (rule-pict-style)])
         pict?]{

  Produces a pict like @scheme[render-reduction-relation], but 
  without setting @scheme[dc-for-text-size].

This function is
primarily designed to be used with Slideshow or with
other tools that combine picts together.
}

@deftogether[[
@defform[(render-metafunction metafunction-name)]{}
@defform/none[#:literals (render-metafunction)
              (render-metafunction metafunction-name filename)]{}
@defform[(render-metafunctions metafunction-name ...)]{}
@defform/none[#:literals (render-metafunctions)
              (render-metafunctions metafunction-name ... #:file filename)]{}]]{

If provided with one argument, @scheme[render-metafunction]
produces a pict that renders properly in the definitions
window in DrScheme. If given two arguments, it writes
postscript into the file named by @scheme[filename] (which
may be either a string or bytes).

Similarly, @scheme[render-metafunctions] accepts multiple 
metafunctions and renders them together, lining up all of the
clauses together.

This function sets @scheme[dc-for-text-size]. See also
@scheme[metafunction->pict] and
@scheme[metafunctions->pict].
}

@defform[(metafunction->pict metafunction-name)]{
  This produces a pict, but without setting @scheme[dc-for-text-size].
  It is suitable for use in Slideshow or other libraries that combine
  picts.
}

@defform[(metafunctions->pict metafunction-name ...)]{
  Like @scheme[metafunction->pict], 
  this produces a pict, but without setting @scheme[dc-for-text-size]
  and is suitable for use in Slideshow or other libraries that combine
  picts. Like
  @scheme[render-metafunctions], it accepts multiple metafunctions
  and renders them together.
}

@subsection{Customization}

@defparam[render-language-nts nts (or/c false/c (listof symbol?))]{
  The value of this parameter controls which non-terminals
  @scheme[render-language] and @scheme[language->pict] render by default. If it
  is @scheme[#f] (the default), all non-terminals are rendered.
  If it is a list of symbols, only the listed symbols are rendered.

  See also @scheme[language-nts].
}

@defparam[extend-language-show-union show? boolean?]{

If this is #t, then a language constructed with
extend-language is shown as if the language had been
constructed directly with @scheme[language]. If it is #f, then only
the last extension to the language is shown (with
four-period ellipses, just like in the concrete syntax).

Defaultly @scheme[#f].

Note that the #t variant can look a little bit strange if
@scheme[....] are used and the original version of the language has
multi-line right-hand sides.
}

@defparam[render-reduction-relation-rules rules (or/c false/c (listof (or/c symbol? string?)))]{
  This parameter controls which rules in a reduction relation
  will be rendered.
}

@defparam[rule-pict-style style reduction-rule-style/c]{

This parameter controls the style used by default for the reduction
relation. It can be @scheme['horizontal], where the left and
right-hand sides of the reduction rule are beside each other or
@scheme['vertical], where the left and right-hand sides of the
reduction rule are above each other.  The @scheme['compact-vertical]
style moves the reduction arrow to the second line and uses less space
between lines.  The @scheme['vertical-overlapping-side-conditions]
variant, the side-conditions don't contribute to the width of the
pict, but are just overlaid on the second line of each rule.  The
@scheme['horizontal-left-align] style is like the @scheme['horizontal]
style, but the left-hand sides of the rules are aligned on the left,
instead of on the right.

}

@defthing[reduction-rule-style/c flat-contract?]{

A contract equivalent to

@schemeblock[
(symbols 'vertical 
         'compact-vertical
         'vertical-overlapping-side-conditions
         'horizontal)
]}

@defparam[arrow-space space natural-number/c]{

This parameter controls the amount of extra horizontal space
around the reduction relation arrow. Defaults to 0.
}

@defparam[horizontal-label-space space natural-number/c]{

This parameter controls the amount of extra space before the
label on each rule, but only in horizontal mode. Defaults to
0.
}

@defparam[metafunction-pict-style style 
                                  (or/c 'left-right
                                        'up-down
                                        'left-right/vertical-side-conditions
                                        'up-down/vertical-side-conditions
                                        'left-right/compact-side-conditions
                                        'up-down/compact-side-conditions
                                        'left-right/beside-side-conditions)]{

This parameter controls the style used for typesetting
metafunctions. The @scheme['left-right] style means that the
results of calling the metafunction are displayed to the 
right of the arguments and the @scheme['up-down] style means that
the results are displayed below the arguments.

The @scheme['left-right/vertical-side-conditions] and
@scheme['up-down/vertical-side-conditions] variants format side
conditions each on a separate line, instead of all on the same line.

The @scheme['left-right/compact-side-conditions] and
@scheme['up-down/compact-side-conditions] variants move side
conditions to separate lines to avoid making the rendered form wider
than it would be otherwise.

The @scheme['left-right/beside-side-conditions] variant is like
@scheme['left-right], except it puts the side-conditions on the 
same line, instead of on a new line below the case.}


@defparam[metafunction-cases 
          cases
          (or/c #f (and/c (listof (and/c integer?
                                         (or/c zero? positive?)))
                          pair?))]{

This parameter controls which cases in a metafunction are rendered. If it is @scheme[#f] (the default), then all of the
cases appear. If it is a list of numbers, then only the selected cases appear (counting from @scheme[0]).
}

@deftogether[[
@defparam[label-style style text-style/c]{}
@defparam[literal-style style text-style/c]{}
@defparam[metafunction-style style text-style/c]{}
@defparam[non-terminal-style style text-style/c]{}
@defparam[non-terminal-subscript-style style text-style/c]{}
@defparam[non-terminal-superscript-style style text-style/c]{}
@defparam[default-style style text-style/c]{}]]{

These parameters determine the font used for various text in
the picts. See @scheme[text] in the texpict collection for
documentation explaining @scheme[text-style/c]. One of the more
useful things it can be is one of the symbols @scheme['roman],
@scheme['swiss], or @scheme['modern], which are a serif, sans-serif, and
monospaced font, respectively. (It can also encode style
information, too.)

The @scheme[label-style] is used for the reduction rule label
names. The @scheme[literal-style] is used for names that aren't
non-terminals that appear in patterns. The
@scheme[metafunction-style] is used for the names of
metafunctions. 

The @scheme[non-terminal-style] is used for the names of non-terminals.
Two parameters style the text in the (optional) "underscore" component
of a non-terminal reference. The first, @scheme[non-terminal-subscript-style],
applies to the segment between the underscore and the first caret (@scheme[^]) 
to follow it; the second, @scheme[non-terminal-superscript-style], applies
to the segment following that caret. For example, in the non-terminal 
reference @scheme[x_y_z], @scheme[x] has style @scheme[non-terminal-style],
@scheme[y] has style @scheme[non-terminal-subscript-style], and @scheme[z]
has style @scheme[non-terminal-superscript-style].

The
@scheme[non-terminal-subscript-style] is used for the portion
after the underscore in non-terminal references.

The @scheme[default-style] is used for parenthesis, the dot in dotted
lists, spaces, the separator words in the grammar, the
"where" and "fresh" in side-conditions, and other places
where the other parameters aren't used.
}

@deftogether[[
@defparam[label-font-size size (and/c (between/c 1 255) integer?)]{}
@defparam[metafunction-font-size size (and/c (between/c 1 255) integer?)]{}
@defparam[default-font-size size (and/c (between/c 1 255) integer?)]{}]]{

These parameters control the various font sizes. The
default-font-size is used for all of the font sizes except
labels and metafunctions.
}

@defparam[reduction-relation-rule-separation sep (parameter/c (and/c integer? positive? exact?))]{  

Controls the amount of space between clauses in a reduction
relation. Defaults to 4.
}

@defparam[curly-quotes-for-strings on? boolean?]{

Controls if the open and close quotes for strings are turned
into “ and ” or are left as merely ".

Defaults to #t.
}

@defparam[current-text proc (-> string? text-style/c number? pict?)]{

This parameter's function is called whenever Redex typesets
some part of a grammar, reduction relation, or
metafunction. It defaults to slideshow's @scheme[text] function.
}

@defparam[set-arrow-pict! proc (-> symbol? (-> pict?) void?)]{

This functions sets the pict for a given reduction-relation
symbol. When typesetting a reduction relation that uses the
symbol, the thunk will be invoked to get a pict to render
it. The thunk may be invoked multiple times when rendering a
single reduction relation.
}

@defparam[white-bracket-sizing proc (-> string? number? (values number? number? number? number?))]{

  This parameter is used when typesetting metafunctions to
  determine how to create the @"\u301a\u301b"
  characters. Rather than using those characters directory
  (since glyphs tend not to be available in PostScript
  fonts), they are created by combining two ‘[’ characters
  or two ‘]’ characters together.
  
  The procedure accepts a string that is either @scheme["["]
  or @scheme["]"], and returns four numbers. The first two
  numbers determine the offset (from the left and from the
  right respectively) for the second square bracket, and the
  second two two numbers determine the extra space added (to
  the left and to the right respectively).

  The default value of the parameter is: @schemeblock[
     (λ (str size)
       (let ([inset-amt (floor/even (max 4 (* size 1/2)))])
         (cond
           [(equal? str "[")
            (values inset-amt
                    0
                    0
                    (/ inset-amt 2))]
           [else
            (values 0
                    inset-amt
                    (/ inset-amt 2)
                    0)])))]

 where @scheme[floor/even] returns the nearest even number
 below its argument.  This means that for sizes 9, 10, and
 11, @scheme[inset-amt] will be 4, and for 12, 13, 14, and
 15, @scheme[inset-amt] will be 6.

}

@deftech{Removing the pink background from PLT Redex rendered picts and ps files}

When reduction rules, a metafunction, or a grammar contains
unquoted Scheme code or side-conditions, they are rendered
with a pink background as a guide to help find them and
provide alternative typesettings for them. In general, a
good goal for a PLT Redex program that you intend to typeset
is to only include such things when they correspond to
standard mathematical operations, and the Scheme code is an
implementation of those operations.

To replace the pink code, use:

@defform[(with-unquote-rewriter proc expression)]{

It installs @scheme[proc] the current unqoute rewriter and
evaluates expression. If that expression computes any picts,
the unquote rewriter specified is used to remap them.

The @scheme[proc] should be a function of one argument. It receives
a @scheme[lw] struct as an argument and should return
another @scheme[lw] that contains a rewritten version of the
code.
}

@defform[(with-atomic-rewriter name-symbol string-or-thunk-returning-pict expression)]{

This extends the current set of atomic-rewriters with one
new one that rewrites the value of name-symbol to
string-or-pict-returning-thunk (applied, in the case of a
thunk), during the evaluation of expression.

@scheme[name-symbol] is expected to evaluate to a symbol. The value
of string-or-thunk-returning-pict is used whever the symbol
appears in a pattern.
}

@defform[(with-compound-rewriter name-symbol proc expression)]{

This extends the current set of compound-rewriters with one
new one that rewrites the value of name-symbol via proc,
during the evaluation of expression.

@scheme[name-symbol] is expected to evaluate to a symbol. The value
of proc is called with a @scheme[(listof lw)], and is expected to
return a new @scheme[(listof (or/c lw? string? pict?))],
rewritten appropriately. 

The list passed to the rewriter corresponds to the
@scheme[lw] for the sequence that has name-symbol's value at
its head.

The result list is constrained to have at most 2 adjacent
non-@scheme[lw]s. That list is then transformed by adding
@scheme[lw] structs for each of the non-@scheme[lw]s in the
list (see the description of @scheme[lw] below for an
explanation of logical-space):

@itemize[
@item{
    If there are two adjacent @scheme[lw]s, then the logical
    space between them is filled with whitespace.}

@item{
    If there is a pair of @scheme[lw]s with just a single
    non-@scheme[lw] between them, a @scheme[lw] will be
    created (containing the non-@scheme[lw]) that uses all
    of the available logical space between the @scheme[lw]s.
}

@item{
    If there are two adjacent non-@scheme[lw]s between two
    @scheme[lw]s, the first non-@scheme[lw] is rendered
    right after the first @scheme[lw] with a logical space
    of zero, and the second is rendered right before the
    last @scheme[lw] also with a logical space of zero, and
    the logical space between the two @scheme[lw]s is
    absorbed by a new @scheme[lw] that renders using no
    actual space in the typeset version.
}]
}


@subsection{LW}

@deftogether[[
@defproc[(build-lw [e (or/c string?
                            symbol?
                            pict? 
                            (listof (or/c (symbols 'spring) lw?)))]
                   [line exact-positive-integer?]
                   [line-span exact-positive-integer?]
                   [column exact-positive-integer?]
                   [column-span exact-positive-integer?]) 
         lw?]{}
@defproc[(lw-e (lw lw?)) (or/c string? 
                               symbol?
                               pict?
                               (listof (or/c (symbols 'spring) lw?)))]{}
@defproc[(lw-line (lw lw?)) exact-positive-integer?]{}
@defproc[(lw-line-span (lw lw?)) exact-positive-integer?]{}
@defproc[(lw-column (lw lw?)) exact-positive-integer?]{}
@defproc[(lw-column-span (lw lw?)) exact-positive-integer?]{}
@defproc[(lw? (v any/c)) boolean?]{}
@defidform[lw]{}]]{

The lw data structure corresponds represents a pattern or a Scheme
expression that is to be typeset.  The functions listed above
construct @scheme[lw] structs, select fields out of them, and
recognize them. The @scheme[lw] binding can be used with
@scheme[copy-struct].
}

@defform[(to-lw arg)]{

This form turns its argument into lw structs that
contain all of the spacing information just as it would appear
when being used to typeset.

Each sub-expression corresponds to its own lw, and
the element indicates what kind of subexpression it is. If
the element is a list, then the lw corresponds to a
parenthesized sequence, and the list contains a lw
for the open paren, one lw for each component of the
sequence and then a lw for the close
parenthesis. In the case of a dotted list, there will also
be a lw in the third-to-last position for the dot.

For example, this expression:

@schemeblock[(a)]

becomes this lw (assuming the above expression
appears as the first thing in the file):

@schemeblock[
     (build-lw (list (build-lw "(" 0 0 0 1)
                              (build-lw 'a 0 0 1 1)
                              (build-lw ")" 0 0 2 1))
                        0 0 0 3)
]

If there is some whitespace in the sequence, like this one:

@schemeblock[
  (a b)
]

then there is no lw that corresponds to that
whitespace; instead there is a logical gap between the
lws.

@schemeblock[
     (build-lw (list (build-lw "(" 0 0 0 1)
                     (build-lw 'a 0 0 1 1)
                     (build-lw 'b 0 0 3 1)
                     (build-lw ")" 0 0 4 1))
               0 0 0 5)
]

In general, identifiers are represented with symbols and
parenthesis are represented with strings and picts can be
inserted to render arbitrary pictures.

The line, line-span, column, and column-span correspond to
the logical spacing for the redex program, not the actual
spacing that will be used when they are rendered. The
logical spacing is only used when determining where to place
typeset portions of the program. In the absense of any
rewriters, these numbers correspond to the line and column
numbers in the original program.

The line and column are absolute numbers from the beginning
of the file containing the expression. The column number is
not necessarily the column of the open parenthesis in a
sequence -- it is the leftmost column that is occupied by
anything in the sequence. The line-span is the number of
lines, and the column span is the number of columns on the
last line (not the total width).

When there are multiple lines, lines are aligned based on
the logical space (ie, the line/column &
line-span/column-span) fields of the lws. As an
example, if this is the original pattern:

@schemeblock[
   (all good boys
        deserve fudge)
]

then the leftmost edges of the words "good" and "deserve"
will be lined up underneath each other, but the relative
positions of "boys" and "fudge" will be determined by the
natural size of the words as they rendered in the
appropriate font.

When @scheme['spring] appears in the list in the @scheme[e]
field of a @scheme[lw] struct, then it absorbs all of the
space around it. It is also used by @scheme[to-lw] when
constructing the picts for unquoted strings. For example, this expression

@schemeblock[,x]

corresponds to these structs:

@schemeblock[(build-lw (list (build-lw "" 1 0 9 0)
                             'spring
                             (build-lw x 1 0 10 1))
                       1 0 9 2)]

and the @scheme['spring] causes there to be no space between
the empty string and the @scheme[x] in the typeset output.

}

@defproc[(render-lw (language/nts (or/c (listof symbol?) compiled-lang?))
                    (lw lw?)) pict?]{

  Produces a pict that corresponds to the @scheme[lw] object
  argument, using @scheme[language/nts] to determine which
  of the identifiers in the @scheme[lw] argument are
  non-terminals.

  This function sets @scheme[dc-for-text-size]. See also
  @scheme[lw->pict].
}

@defproc[(lw->pict (language/ntw (or/c (listof symbol?) compiled-lang?))
                   (lw lw?)) pict?]{

  Produces a pict that corresponds to the @scheme[lw] object
  argument, using @scheme[language/nts] to determine which
  of the identifiers in the @scheme[lw] argument are
  non-terminals.

  This does not set the @scheme[dc-for-text-size] parameter. See also
  @scheme[render-lw].
}

@deftogether[[
@defproc[(just-before [stuff (or/c pict? string? symbol?)]
                      [lw lw?])
                     lw?]{}
@defproc[(just-after [stuff (or/c pict? string? symbol?)]
                     [lw lw?])
                    lw?]{}]]{

These two helper functions build new lws whose contents are
the first argument, and whose line and column are based on
the second argument, making the new loc wrapper be either
just before or just after that argument. The line-span and
column-span of the new lw is always zero.
}


@index-section[]

