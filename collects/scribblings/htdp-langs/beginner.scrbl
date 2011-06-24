#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-beginner))


@title[#:style 'toc #:tag "beginner"]{Beginning Student}

@declare-exporting[lang/htdp-beginner #:use-sources (lang/htdp-beginner lang/private/teachprims)]

@racketgrammar*+library[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet
            check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case             
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define id (lambda (id id ...) expr))
            (define-struct id (id ...))]
[expr (code:line (id expr expr ...) (code:comment @#,seclink["beginner-call"]{function call}))
      (code:line (prim-op expr ...) (code:comment @#,seclink["beginner-prim-call"]{primitive operation call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      empty
      id
      (code:line id (code:comment @#,seclink["beginner-id"]{identifier}))
      (code:line @#,elem{@racketvalfont{'}@racket[id]} (code:comment @#,seclink["beginner-quote"]{symbol}))
      number
      true
      false
      string
      character]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-beginner.rkt" "lang") #'here]

@; ----------------------------------------------------------------------

@section{@racket[define]}

@defform[(define (id id id ...) expr)]{

Defines a function. The first @racket[id] inside the parentheses is
the name of the function. All remaining @racket[id]s are the names of
the function's arguments. The @racket[expr] is the body of the
function, evaluated whenever the function is called. The name of the
function cannot be that of a primitive or another definition.}

@defform/none[#:literals (define)
              (define id expr)]{

Defines a constant @racket[id] as a synonym for the value produced by
@racket[expr]. The defined name cannot be that of a primitive or
another definition, and @racket[id] itself must not appear in
@racket[expr].}


@defform/none[#:literals (define lambda)
              (define id (lambda (id id ...) expr))]{

An alternate form for defining functions. The first @racket[id] is the
name of the function. The @racket[id]s in parentheses are the names of
the function's arguments, and the @racket[expr] is the body of the
function, which evaluated whenever the function is called.  The name
of the function cannot be that of a primitive or another definition.}

@defidform[lambda]{

The @racket[lambda] keyword can only be used with @racket[define] in
the alternative function-definition syntax.}

@; ----------------------------------------------------------------------

@section{@racket[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

Define a new type of structure. The structure's fields are named by
the @racket[fieldid]s in parentheses. After evaluation of a
define-struct form, a set of new primitives is available for creation,
extraction, and type-like queries:

@itemize[

 @item{@racketidfont{make-}@racket[structid] : takes a number of
       arguments equal to the number of fields in the structure type,
       and creates a new instance of the structure type.}

 @item{@racket[structid]@racketidfont{-}@racket[fieldid] : takes an
       instance of the structure and returns the field named by
       @racket[structid].}

 @item{@racket[structid]@racketidfont{?} : takes any value, and returns
       @racket[true] if the value is an instance of the structure type.}

 @item{@racket[structid] : an identifier representing the structure
       type, but never used directly.}

]

The created names must not be the same as a primitive or another defined name.}

@; ----------------------------------------------------------------------
@;{   ------- COMMENTED OUT FOR NOW ---------
@section{@racket[define-wish]}

@defform[(define-wish id)]{

Defines a function named @racket[id] that we wish exists but have not implemented yet. 
The name of the function cannot be that of a primitive or another definition.
The wished-for function can be called with one argument: @racket[(id _expr)].

Wished-for functions are reported in the test report for the current program.}

@defform/none[#:literals (define-wish)
              (define-wish id expr)]{
Similar to the above form, defines a wished-for function named @racket[id]. If the 
wished-for function is called with one value, the result of @racket[expr] is
returned as the default value. }
}
@; ----------------------------------------------------------------------

@section[#:tag "beginner-call"]{Function Calls}

@defform/none[(id expr expr ...)]{

Calls a function. The @racket[id] must refer to a defined function,
and the @racket[expr]s are evaluated from left to right to produce the
values that are passed as arguments to the function. The result of the
function call is the result of evaluating the function's body with
every instance of an argument name replaced by the value passed for
that argument. The number of argument @racket[expr]s must be the same
as the number of arguments expected by the function.}

@defform[(#%app id expr expr ...)]{

A function call can be written with @racket[#%app], though it's
practically never written that way.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-prim-call"]{Primitive Calls}

@defform/none[(prim-op expr ...)]{

Like a @seclink["beginner-call"]{function call}, but for a primitive
operation. The @racket[expr]s are evaluated from left to right, and
passed as arguments to the primitive operation named by
@racket[prim-op]. A @racket[define-struct] form creates new
primitives.}

@; ----------------------------------------------------------------------

@section{@racket[cond]}

@defform[(cond [expr expr] ... [expr expr])]{

A @racket[cond] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains two
@racket[expr]s: a question @racket[expr] and an answer
@racket[expr].

The lines are considered in order. To evaluate a line, first evaluate
the question @racket[expr]. If the result is @racket[true], then the
result of the whole @racket[cond] expression is the result of
evaluating the answer @racket[expr] of the same line. If the result of
evaluating the question @racket[expr] is @racket[false], the line is
discarded and evaluation proceeds with the next line.

If the result of a question @racket[expr] is neither @racket[true] nor
@racket[false], it is an error. If none of the question @racket[expr]s
evaluates to @racket[true], it is also an error.}

@defform/none[#:literals (cond else)
              (cond [expr expr] ... [else expr])]{

This form of @racket[cond] is similar to the prior one, except that
the final @racket[else] clause is always taken if no prior line's test
expression evaluates to @racket[true]. In other words, @racket[else]
acts like @racket[true], so there is no possibility to ``fall off the
end'' of the @racket[cond] form.}

@defidform[else]{

The @racket[else] keyword can be used only with @racket[cond].}

@; ----------------------------------------------------------------------

@section{@racket[if]}

@defform[(if expr expr expr)]{

The first @racket[expr] (known as the ``test'' @racket[expr]) is
evaluated. If it evaluates to @racket[true], the result of the
@racket[if] expression is the result of evaluating the second
@racket[expr] (often called the ``then'' @racket[expr]). If the text
@racket[expr] evaluates to @racket[false], the result of the
@racket[if] expression is the result of evaluating the third
@racket[expr] (known as the ``else'' @racket[expr]). If the
result of evaluating the test @racket[expr] is neither @racket[true]
nor @racket[false], it is an error.}

@; ----------------------------------------------------------------------

@section{@racket[and]}

@defform[(and expr expr expr ...)]{

The @racket[expr]s are evaluated from left to right. If the first
@racket[expr] evaluates to @racket[false], the @racket[and] expression
immediately evaluates to @racket[false]. If the first @racket[expr]
evaluates to @racket[true], the next expression is considered. If all
@racket[expr]s evaluate to @racket[true], the @racket[and] expression
evaluates to @racket[true]. If any of the expressions evaluate to a
value other than @racket[true] or @racket[false], it is an error.}

@; ----------------------------------------------------------------------

@section{@racket[or]}

@defform[(or expr expr expr ...)]{

The @racket[expr]s are evaluated from left to right. If the first
@racket[expr] evaluates to @racket[true], the @racket[or] expression
immediately evaluates to @racket[true]. If the first @racket[expr]
evaluates to @racket[false], the next expression is considered. If all
@racket[expr]s evaluate to @racket[false], the @racket[or] expression
evaluates to @racket[false]. If any of the expressions evaluate to a
value other than @racket[true] or @racket[false], it is an error.}

@; ----------------------------------------------------------------------

@section{Test Cases}

@defform[(check-expect expr expr)]{

A test case to check that the first @racket[expr] produces the same
value as the second @racket[expr], where the latter is normally an
immediate value.}

@defform[(check-within expr expr expr)]{

Like @racket[check-expect], but with an extra expression that produces
a number @racket[_delta]. The test case checks that each number in the
result of the first @racket[expr] is within @racket[_delta] of each
corresponding number from the second @racket[expr].}

@defform*[[(check-error expr expr)
           (check-error expr)]]{

A test case to check that the first @racket[expr] signals an error,
where the error messages matches the string produced by the second
@racket[expr], if it is present.}

@defform[(check-member-of expr expr expr ...)]{

A test case to check that the first @racket[expr] produces an element 
that is equivalent to one of the following @racket[expr]s.}

@defform[(check-range expr expr expr)]{

A test case to check that the first @racket[expr] produces a number
inbetween the numbers produced by the second and third @racket[expr]s,
inclusive.}

@; ----------------------------------------------------------------------

@section{@racket[empty]}

@defthing[empty empty?]{

The empty list.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-id"]{Identifiers}

@defform/none[id]{

An @racket[id] refers to a defined constant or argument within a
function body. If no definition or argument matches the @racket[id]
name, an error is reported. Similarly, if @racket[id] matches the name
of a defined function or primitive operation, an error is reported.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-quote"]{Symbols}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[id]})]
@defform[(quote id)]
)]{

A quoted @racket[id] is a symbol. A symbol is a constant, like
@racket[0] and @racket[empty].

Normally, a symbol is written with a @litchar{'}, like
@racket['apple], but it can also be written with @racket[quote], like
@racket[(@#,racket[quote] apple)].

The @racket[id] for a symbol is a sequence of characters not including
a space or one of the following:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}


@; ----------------------------------------------------------------------

@section{@racket[true] and @racket[false]}

@defthing[true boolean?]{

The true value.}

@defthing[false boolean?]{

The false value.}

@; ----------------------------------------------------------------------

@section{@racket[require]}

@defform[(require string)]{

Makes the definitions of the module specified by @racket[string]
available in the current module (i.e., current file), where @racket[string]
refers to a file relative to the enclosing file.

The @racket[string] is constrained in several ways to avoid problems
with different path conventions on different platforms: a @litchar{/}
is a directory separator, @litchar{.} always means the current
directory, @litchar{..} always means the parent directory, path
elements can use only @litchar{a} through @litchar{z} (uppercase or
lowercase), @litchar{0} through @litchar{9}, @litchar{-}, @litchar{_},
and @litchar{.}, and the string cannot be empty or contain a leading
or trailing @litchar{/}.}

@defform/none[#:literals (require)
              (require module-id)]{

Accesses a file in an installed library. The library name is an
identifier with the same constraints as for a relative-path string,
with the additional constraint that it must not contain a
@litchar{.}.}

@defform/none[#:literals (require lib)
              (require (lib string string ...))]{

Accesses a file in an installed library, making its definitions
available in the current module (i.e., current file). The first
@racket[string] names the library file, and the remaining
@racket[string]s name the collection (and sub-collection, and so on)
where the file is installed. Each string is constrained in the same
way as for the @racket[(require string)] form.}


@defform/none[#:literals (require planet)
              (require (planet string (string string number number)))]{


Accesses a library that is distributed on the internet via the PLaneT
server, making it definitions available in the current module (i.e.,
current file).}

@; ----------------------------------------

@section[#:tag "beginner-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-beginner.rkt" "lang") #'here '()]
