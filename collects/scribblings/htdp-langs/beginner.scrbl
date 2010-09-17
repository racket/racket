#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-beginner))


@title[#:style 'toc #:tag "beginner"]{Beginning Student}

@declare-exporting[lang/htdp-beginner]

@schemegrammar*+library[
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
      (code:line @#,elem{@schemevalfont{'}@scheme[id]} (code:comment @#,seclink["beginner-quote"]{symbol}))
      number
      true
      false
      string
      character]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-beginner.ss" "lang") #'here]

@; ----------------------------------------------------------------------

@section{@scheme[define]}

@defform[(define (id id id ...) expr)]{

Defines a function. The first @scheme[id] inside the parentheses is
the name of the function. All remaining @scheme[id]s are the names of
the function's arguments. The @scheme[expr] is the body of the
function, evaluated whenever the function is called. The name of the
function cannot be that of a primitive or another definition.}

@defform/none[#:literals (define)
              (define id expr)]{

Defines a constant @scheme[id] as a synonym for the value produced by
@scheme[expr]. The defined name cannot be that of a primitive or
another definition, and @scheme[id] itself must not appear in
@scheme[expr].}


@defform/none[#:literals (define lambda)
              (define id (lambda (id id ...) expr))]{

An alternate form for defining functions. The first @scheme[id] is the
name of the function. The @scheme[id]s in parentheses are the names of
the function's arguments, and the @scheme[expr] is the body of the
function, which evaluated whenever the function is called.  The name
of the function cannot be that of a primitive or another definition.}

@defidform[lambda]{

The @scheme[lambda] keyword can only be used with @scheme[define] in
the alternative function-definition syntax.}

@; ----------------------------------------------------------------------

@section{@scheme[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

Define a new type of structure. The structure's fields are named by
the @scheme[fieldid]s in parentheses. After evaluation of a
define-struct form, a set of new primitives is available for creation,
extraction, and type-like queries:

@itemize[

 @item{@schemeidfont{make-}@scheme[structid] : takes a number of
       arguments equal to the number of fields in the structure type,
       and creates a new instance of the structure type.}

 @item{@scheme[structid]@schemeidfont{-}@scheme[fieldid] : takes an
       instance of the structure and returns the field named by
       @scheme[structid].}

 @item{@scheme[structid]@schemeidfont{?} : takes any value, and returns
       @scheme[true] if the value is an instance of the structure type.}

 @item{@scheme[structid] : an identifier representing the structure
       type, but never used directly.}

]

The created names must not be the same as a primitive or another defined name.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-call"]{Function Calls}

@defform/none[(id expr expr ...)]{

Calls a function. The @scheme[id] must refer to a defined function,
and the @scheme[expr]s are evaluated from left to right to produce the
values that are passed as arguments to the function. The result of the
function call is the result of evaluating the function's body with
every instance of an argument name replaced by the value passed for
that argument. The number of argument @scheme[expr]s must be the same
as the number of arguments expected by the function.}

@defform[(#%app id expr expr ...)]{

A function call can be written with @scheme[#%app], though it's
practically never written that way.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-prim-call"]{Primitive Calls}

@defform/none[(prim-op expr ...)]{

Like a @seclink["beginner-call"]{function call}, but for a primitive
operation. The @scheme[expr]s are evaluated from left to right, and
passed as arguments to the primitive operation named by
@scheme[prim-op]. A @scheme[define-struct] form creates new
primitives.}

@; ----------------------------------------------------------------------

@section{@scheme[cond]}

@defform[(cond [expr expr] ... [expr expr])]{

A @scheme[cond] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains two
@scheme[expr]s: a question @scheme[expr] and an answer
@scheme[expr].

The lines are considered in order. To evaluate a line, first evaluate
the question @scheme[expr]. If the result is @scheme[true], then the
result of the whole @scheme[cond] expression is the result of
evaluating the answer @scheme[expr] of the same line. If the result of
evaluating the question @scheme[expr] is @scheme[false], the line is
discarded and evaluation proceeds with the next line.

If the result of a question @scheme[expr] is neither @scheme[true] nor
@scheme[false], it is an error. If none of the question @scheme[expr]s
evaluates to @scheme[true], it is also an error.}

@defform/none[#:literals (cond else)
              (cond [expr expr] ... [else expr])]{

This form of @scheme[cond] is similar to the prior one, except that
the final @scheme[else] clause is always taken if no prior line's test
expression evaluates to @scheme[true]. In other words, @scheme[else]
acts like @scheme[true], so there is no possibility to ``fall off the
end'' of the @scheme[cond] form.}

@defidform[else]{

The @scheme[else] keyword can be used only with @scheme[cond].}

@; ----------------------------------------------------------------------

@section{@scheme[if]}

@defform[(if expr expr expr)]{

The first @scheme[expr] (known as the ``test'' @scheme[expr]) is
evaluated. If it evaluates to @scheme[true], the result of the
@scheme[if] expression is the result of evaluating the second
@scheme[expr] (often called the ``then'' @scheme[expr]). If the text
@scheme[expr] evaluates to @scheme[false], the result of the
@scheme[if] expression is the result of evaluating the third
@scheme[expr] (known as the ``else'' @scheme[expr]). If the
result of evaluating the test @scheme[expr] is neither @scheme[true]
nor @scheme[false], it is an error.}

@; ----------------------------------------------------------------------

@section{@scheme[and]}

@defform[(and expr expr expr ...)]{

The @scheme[expr]s are evaluated from left to right. If the first
@scheme[expr] evaluates to @scheme[false], the @scheme[and] expression
immediately evaluates to @scheme[false]. If the first @scheme[expr]
evaluates to @scheme[true], the next expression is considered. If all
@scheme[expr]s evaluate to @scheme[true], the @scheme[and] expression
evaluates to @scheme[true]. If any of the expressions evaluate to a
value other than @scheme[true] or @scheme[false], it is an error.}

@; ----------------------------------------------------------------------

@section{@scheme[or]}

@defform[(or expr expr expr ...)]{

The @scheme[expr]s are evaluated from left to right. If the first
@scheme[expr] evaluates to @scheme[true], the @scheme[or] expression
immediately evaluates to @scheme[true]. If the first @scheme[expr]
evaluates to @scheme[false], the next expression is considered. If all
@scheme[expr]s evaluate to @scheme[false], the @scheme[or] expression
evaluates to @scheme[false]. If any of the expressions evaluate to a
value other than @scheme[true] or @scheme[false], it is an error.}

@; ----------------------------------------------------------------------

@section{Test Cases}

@defform[(check-expect expr expr)]{

A test case to check that the first @scheme[expr] produces the same
value as the second @scheme[expr], where the latter is normally an
immediate value.}

@defform[(check-within expr expr expr)]{

Like @scheme[check-expect], but with an extra expression that produces
a number @scheme[_delta]. The test case checks that each number in the
result of the first @scheme[expr] is within @scheme[_delta] of each
corresponding number from the second @scheme[expr].}

@defform*[[(check-error expr expr)
           (check-error expr)]]{

A test case to check that the first @scheme[expr] signals an error,
where the error messages matches the string produced by the second
@scheme[expr], if it is present.}

@defform[(check-member-of expr expr expr ...)]{

A test case to check that the first @scheme[expr] produces an element 
that is equivalent to one of the following @scheme[expr]s.}

@defform[(check-range expr expr expr)]{

A test case to check that the first @scheme[expr] produces a number
inbetween the numbers produced by the second and third @scheme[expr]s,
inclusive.}
                                                               

@; ----------------------------------------------------------------------

@section{@scheme[empty]}

@defthing[empty empty?]{

The empty list.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-id"]{Identifiers}

@defform/none[id]{

An @scheme[id] refers to a defined constant or argument within a
function body. If no definition or argument matches the @scheme[id]
name, an error is reported. Similarly, if @scheme[id] matches the name
of a defined function or primitive operation, an error is reported.}

@; ----------------------------------------------------------------------

@section[#:tag "beginner-quote"]{Symbols}

@deftogether[(
@defform/none[(unsyntax @elem{@schemevalfont{'}@scheme[id]})]
@defform[(quote id)]
)]{

A quoted @scheme[id] is a symbol. A symbol is a constant, like
@scheme[0] and @scheme[empty].

Normally, a symbol is written with a @litchar{'}, like
@scheme['apple], but it can also be written with @scheme[quote], like
@scheme[(@#,scheme[quote] apple)].

The @scheme[id] for a symbol is a sequence of characters not including
a space or one of the following:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}


@; ----------------------------------------------------------------------

@section{@scheme[true] and @scheme[false]}

@defthing[true boolean?]{

The true value.}

@defthing[false boolean?]{

The false value.}

@; ----------------------------------------------------------------------

@section{@scheme[require]}

@defform[(require string)]{

Makes the definitions of the module specified by @scheme[string]
available in the current module (i.e., current file), where @scheme[string]
refers to a file relative to the enclosing file.

The @scheme[string] is constrained in several ways to avoid problems
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
@scheme[string] names the library file, and the remaining
@scheme[string]s name the collection (and sub-collection, and so on)
where the file is installed. Each string is constrained in the same
way as for the @scheme[(require string)] form.}


@defform/none[#:literals (require planet)
              (require (planet string (string string number number)))]{


Accesses a library that is distributed on the internet via the PLaneT
server, making it definitions available in the current module (i.e.,
current file).}

@; ----------------------------------------

@section[#:tag "beginner-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-beginner.ss" "lang") #'here '()]
