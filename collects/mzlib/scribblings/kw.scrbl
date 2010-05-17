#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label mzlib/kw
                     mzlib/etc))

@(define kw-eval (make-base-eval))

@interaction-eval[#:eval kw-eval (require mzscheme)]
@interaction-eval[#:eval kw-eval (require mzlib/kw)]

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require (for-label scheme/base))
     (define id (scheme lambda))))
  (bind base-lambda))

@mzlib[#:mode title kw]

@margin-note{The @base-lambda and procedure-application forms of
             @scheme[scheme/base] support keyword arguments, and it is
             @emph{not} compatible with the @scheme[mzlib/kw]
             library.}

@deftogether[(
@defform[(lambda/kw kw-formals body ...+)]
@defform/subs[
(define/kw (head args) body ...+)
([kw-formals id
             (id ... [#:optional optional-spec ...]
                     [#:key key-spec ...]
                     [rest/mode-spec ...])
             (id ... . id)]
 [optional-spec id
                (id default-expr)]
 [key-spec id
           (id default-expr)
           (id keyword default-expr)]
 [rest/mode-spec (code:line #:rest id)
                 (code:line #:other-keys id)
                 (code:line #:other-keys+body id)
                 (code:line #:all-keys id)
                 (code:line #:body kw-formals)
                 (code:line #:allow-other-keys)
                 (code:line #:forbid-other-keys)
                 (code:line #:allow-duplicate-keys)
                 (code:line #:forbid-duplicate-keys)
                 (code:line #:allow-body)
                 (code:line #:forbid-body)
                 (code:line #:allow-anything)
                 (code:line #:forbid-anything)]
  [head id
        (head . kw-formals)])
])]{

Like @scheme[lambda], but with optional and keyword-based argument
processing.  This form is similar to an extended version of Common
Lisp procedure arguments (but note the differences below).  When used
with plain variable names, @scheme[lambda/kw] expands to a plain
@scheme[lambda], so @scheme[lambda/kw] is suitable for a language
module that will use it to replace @scheme[lambda].  Also, when used
with only optionals, the resulting procedure is similar to
@scheme[opt-lambda] (but a bit faster).

In addition to @scheme[lambda/kw], @scheme[define/kw] is similar to
@scheme[define], except that the @scheme[formals] are as in
@scheme[lambda/kw].  Like @scheme[define], this form can be used with
nested parenthesis for curried functions (the MIT-style generalization
of @scheme[define]).

The syntax of @scheme[lambda/kw] is the same as @scheme[lambda],
except for the list of formal argument specifications.  These
specifications can hold (zero or more) plain argument names, then an
optionals (and defaults) section that begins after an
@scheme[#:optional] marker, then a keyword section that is marked by
@scheme[#:keyword], and finally a section holding rest and
``rest''-like arguments which are described below, together with
argument processing flag directives.  Each section is optional, but
the order of the sections must be as listed. Of course, all binding
@scheme[id]s must be unique.

The following sections describe each part of the @scheme[kw-formals].}

@; ----------------------------------------

@section{Required Arguments}

Required arguments correspond to @scheme[id]s that appear before any
keyword marker in the argument list. They determine the minimum arity
of the resulting procedure.

@; ----------------------------------------

@section{Optional Arguments}

The optional-arguments section follows an
@as-index{@scheme[#:optional]} marker in the @scheme[_kw-formals].
Each optional argument can take the form of a parenthesized variable
and a default expression; the latter is used if a value is not given
at the call site.  The default expression can be omitted (along with
the parentheses), in which case @scheme[#f] is the default.

The default expression's environment includes all previous arguments,
both required and optional names.  With @math{k} optionals after
@math{n} required arguments, and with no keyword arguments or
rest-like arguments, the resulting procedure accept between @math{n}
and @math{n+k} arguments, inclusive.

The treatment of optionals is efficient, with an important caveat:
default expressions appear multiple times in the resulting
@scheme[case-lambda]. For example, the default expression for the last
optional argument appears @math{k-1} times (but no expression is ever
evaluated more than once in a procedure call). This expansion risks
exponential blow-up is if @scheme[lambda/kw] is used in a default
expression of a @scheme[lambda/kw], etc.  The bottom line, however, is
that @scheme[lambda/kw] is a sensible choice, due to its enhanced
efficiency, even when you need only optional arguments.

Using both optional and keyword arguments is possible, but note that
the resulting behavior differs from traditional keyword facilities
(including the one in Common Lisp).  See the following section for
details.

@; ----------------------------------------

@section{Keyword Arguments}

A keyword argument section is marked by a @as-index{@scheme[#:key]}.
If it is used with optional arguments, then the keyword specifications
must follow the optional arguments (which mirrors the use in call
sites; where optionals are given before keywords).

When a procedure accepts both optional and keyword arguments, the
argument-handling convention is slightly different than in traditional
keyword-argument facilities: a keyword after required arguments marks
the beginning of keyword arguments, no matter how many optional
arguments have been provided before the keyword.  This convention
restricts the procedure's non-keyword optional arguments to
non-keyword values, but it also avoids confusion when mixing optional
arguments and keywords.  For example, when a procedure that takes two
optional arguments and a keyword argument @scheme[#:x] is called with
@scheme[#:x 1], then the optional arguments get their default values
and the keyword argument is bound to @scheme[1]. (The traditional
behavior would bind @scheme[#:x] and @scheme[1] to the two optional
arguments.) When the same procedure is called with @scheme[1 #:x 2],
the first optional argument is bound to @scheme[1], the second
optional argument is bound to its default, and the keyword argument is
bound to @scheme[2]. (The traditional behavior would report an error,
because @scheme[2] is provided where @scheme[#:x] is expected.)

Like optional arguments, each keyword argument is specified as a
parenthesized variable name and a default expression.  The default
expression can be omitted (with the parentheses), in which case
@scheme[#f] is the default value. The keyword used at a call site for
the corresponding variable has the same name as the variable; a third
form of keyword arguments has three parts---a variable name, a
keyword, and a default expression---to allow the name of the locally
bound variable to differ from the keyword used at call sites.

When calling a procedure with keyword arguments, the required argument
(and all optional arguments, if specified) must be followed by an even
number of arguments, where the first argument is a keyword that
determines which variable should get the following value, etc.  If the
same keyword appears multiple times (and if multiple instances of the
keyword are allowed; see @secref["mode-keywords"]), the value after
the first occurrence is used for the variable:

@examples[
#:eval kw-eval
((lambda/kw (#:key x [y 2] [z #:zz 3] #:allow-duplicate-keys)
   (list x y z))
 #:x 'x #:zz 'z #:x "foo")
]

Default expressions are evaluated only for keyword arguments that do
not receive a value for a particular call. Like optional arguments,
each default expression is evaluated in an environment that includes
all previous bindings (required, optional, and keywords that were
specified on its left).

See @secref["mode-keywords"] for information on when duplicate or
unknown keywords are allowed at a call site.

@; ----------------------------------------

@section{Rest and Rest-like Arguments}

The last @scheme[_kw-formals] section---after the required, optional,
and keyword arguments---may contain specifications for rest-like
arguments and/or mode keywords.  Up to five rest-like arguments can be
declared, each with an @scheme[_id] to bind:

@itemize[

 @item{@as-index{@scheme[#:rest]} --- The variable is bound to the
  list of ``rest'' arguments, which is the list of all values after
  the required and the optional values.  This list includes all
  keyword-value pairs, exactly as they are specified at the call site.

  Scheme's usual dot-notation is accepted in @scheme[_kw-formals] only
  if no other meta-keywords are specified, since it is not clear
  whether it should specify the same binding as a @scheme[#:rest] or
  as a @scheme[#:body].  The dot notation is allowed without
  meta-keywords to make the @scheme[lambda/kw] syntax compatible with
  @scheme[lambda].}

 @item{@as-index{@scheme[#:body]} --- The variable is bound to all
  arguments after keyword--value pairs.  (This is different from
  Common Lisp's @scheme[&body], which is a synonym for
  @scheme[&rest].)  More generally, a @scheme[#:body] specification
  can be followed by another @scheme[_kw-formals], not just a single
  @scheme[_id]; see @secref["kw-body"] for more information.}

 @item{@as-index{@scheme[#:all-keys]} --- the variable is bound to the
  list of all keyword-values from the call site, which is always a
  proper prefix of a @scheme[#:rest] argument.  (If no @scheme[#:body]
  arguments are declared, then @scheme[#:all-keys] binds the same as
  @scheme[#:rest].) See also @scheme[keyword-get].}

 @item{@scheme[#:other-keys] --- The variable is bound like an
  @scheme[#:all-keys] variable, except that all keywords specified in
  the @scheme[kw-formals] are removed from the list.  When a keyword
  is used multiple times at a call cite (and this is allowed), only
  the first instances is removed for the @scheme[#:other-keys]
  binding.}

 @item{@scheme[#:other-keys+body] --- the variable is bound like a
  @scheme[#:rest] variable, except that all keywords specified in the
  @scheme[_kw-formals] are removed from the list.  When a keyword is
  used multiple times at a call site (and this is allowed), only the
  first instance us removed for the @scheme[#:other-keys+body]
  binding.  (When no @scheme[#:body] variables are specified, then
  @scheme[#:other-keys+body] is the same as @scheme[#:other-keys].)}

]

In the following example, all rest-like arguments are used and have different
bindings:

@examples[
#:eval kw-eval
((lambda/kw (#:key x y
             #:rest r
             #:other-keys+body rk
             #:all-keys ak
             #:other-keys ok
             #:body b)
   (list r rk b ak ok))
 #:z 1 #:x 2 2 3 4)
]

Note that the following invariants always hold:

@itemize[
@item{@scheme[_rest] = @scheme[(append _all-keys _body)]}
@item{@scheme[_other-keys+body] = @scheme[(append _other-keys _body)]}
]

To write a procedure that uses a few keyword argument values, and that
also calls another procedure with the same list of arguments
(including all keywords), use @scheme[#:other-keys] (or
@scheme[#:other-keys+body]).  The Common Lisp approach is to specify
@scheme[:allow-other-keys], so that the second procedure call will not
cause an error due to unknown keywords, but the
@scheme[:allow-other-keys] approach risks confusing the two layers of
keywords.

@; ----------------------------------------

@section[#:tag "kw-body"]{Body Argument}

The most notable divergence from Common Lisp in @scheme[lambda/kw] is
the @scheme[#:body] argument, and the fact that it is possible at a
call site to pass plain values after the keyword-value pairs.  The
@scheme[#:body] binding is useful for procedure calls that use
keyword-value pairs as sort of an attribute list before the actual
arguments to the procedure.  For example, consider a procedure that
accepts any number of numeric arguments and will apply a procedure to
them, but the procedure can be specified as an optional keyword
argument. It is easily implemented with a @scheme[#:body] argument:

@examples[
#:eval kw-eval
(define/kw (mathop #:key [op +] #:body b)
  (apply op b))
(mathop 1 2 3)
(mathop #:op max 1 2 3)
]

(Note that the first body value cannot itself be a keyword.)

A @scheme[#:body] declaration works as an arbitrary
@scheme[kw-formals], not just a single variable like @scheme[b] in the
above example. For example, to make the above @scheme[mathop] work
only on three arguments that follow the keyword, use @scheme[(x y z)]
instead of @scheme[b]:

@examples[
#:eval kw-eval
(define/kw (mathop #:key [op +] #:body (x y z))
  (op x y z))
]

In general, @scheme[#:body] handling is compiled to a sub procedure
using @scheme[lambda/kw], so that a procedure can use more then one
level of keyword arguments. For example:

@examples[
#:eval kw-eval
(define/kw (mathop #:key [op +]
                   #:body (x y z #:key [convert values]))
  (op (convert x) (convert y) (convert z)))
(mathop #:op * 2 4 6 #:convert exact->inexact)
]

Obviously, nested keyword arguments works only when non-keyword
arguments separate the sets.

Run-time errors during such calls report a mismatch for a procedure
with a name that is based on the original name plus a @schemeidfont{~body}
suffix:

@examples[
#:eval kw-eval
(mathop #:op * 2 4)
]

@; ----------------------------------------

@section[#:tag "mode-keywords"]{Mode Keywords}

Finally, the argument list of a @scheme[lambda/kw] can contain
keywords that serve as mode flags to control error reporting.

@itemize[

 @item{@as-index{@scheme[#:allow-other-keys]} --- The keyword-value
  sequence at the call site @italic{can} include keywords that are not
  listed in the keyword part of the @scheme[lambda/kw] form.}

 @item{@as-index{@scheme[#:forbid-other-keys]} --- The keyword-value
  sequence at the call site @italic{cannot} include keywords that are
  not listed in the keyword part of the @scheme[lambda/kw] form,
  otherwise the @scheme[exn:fail:contract] exception is raised.}

 @item{@as-index{@scheme[#:allow-duplicate-keys]} --- The
  keyword-value list at the call site @emph{can} include duplicate
  values associated with same keyword, the first one is used.}

 @item{@as-index{@scheme[#:forbid-duplicate-keys]} --- The
  keyword-value list at the call site @italic{cannot} include
  duplicate values for keywords, otherwise the
  @scheme[exn:fail:contract] exception is raised. This restriction
  applies only to keywords that are listed in the keyword part of the
  @scheme[lambda/kw] form --- if other keys are allowed, this
  restriction does not apply to them.}

 @item{@as-index{@scheme[#:allow-body]} --- Body arguments
  @italic{can} be specified at the call site after all keyword-value
  pairs.}

 @item{@as-index{@scheme[#:forbid-body]} --- Body arguments
  @italic{cannot} be specified at the call site after all
  keyword-value pairs.}

 @item{@as-index{@scheme[#:allow-anything]} --- Allows all of the
  above, and treat a single keyword at the end of an argument list as
  a @scheme[#:body], a situation that is usually an error.  When this
  is used and no rest-like arguments are used except @scheme[#:rest],
  an extra loop is saved and calling the procedures is faster (around
  20%).}

 @item{@as-index{@scheme[#:forbid-anything]} --- Forbids all of the
  above, ensuring that calls are as restricted as possible.}

]

These above mode markers are rarely needed, because the default modes
are determined by the declared rest-like arguments:

@itemize[

 @item{The default is to allow other keys if a @scheme[#:rest],
  @scheme[#:other-keys+body], @scheme[#:all-keys], or
  @scheme[#:other-keys] variable is declared (and an
  @scheme[#:other-keys] declaration requires allowing other keys).}

 @item{The default is to allow duplicate keys if a @scheme[#:rest] or
  @scheme[#:all-keys] variable is declared.}

 @item{The default is to allow body arguments if a @scheme[#:rest],
  @scheme[#:body], or @scheme[#:other-keys+body] variable is declared
  (and a @scheme[#:body] argument requires allowing them).}

]

Here's an alternate specification, which maps rest-like arguments to
the behavior that they imply:

@itemize[

 @item{@scheme[#:rest]: Everything is allowed (a body, other keys,
  and duplicate keys);}

 @item{@scheme[#:other-keys+body]: Other keys and body are allowed,
  but duplicates are not;}

 @item{@scheme[#:all-keys]: Other keys and duplicate keys are allowed,
  but a body is not;}

 @item{@scheme[#:other-keys]: Other keys must be allowed (on by
   default, cannot use with @scheme[#:forbid-other-keys]), and
   duplicate keys and body are not allowed;}

 @item{@scheme[#:body]: Body must be allowed (on by default, cannot use
   with @scheme[#:forbid-body]) and other keys and duplicate keys and
   body are not allowed;}

 @item{Except for the previous two ``must''s, defaults can be
   overridden by an explicit @scheme[#:allow-...] or a
   @scheme[#:forbid-...] mode.}

]

@; ----------------------------------------

@section[#:tag "keyword-get"]{Property Lists}

@defproc[(keyword-get [args (listof (cons/c keyword? any/c))] [kw keyword?]
                      [not-found (-> any)])
         any]{

Searches a list of keyword arguments (a ``property list'' or ``plist''
in Lisp jargon) for the given keyword, and returns the associated
value.  It is the facility that is used by @scheme[lambda/kw] to
search for keyword values.

The @scheme[args] list is scanned from left to right, if the keyword
is found, then the next value is returned.  If the @scheme[kw] was not
found, then the @scheme[not-found] thunk is used to produce a value by
applying it.  If the @scheme[kw] was not found, and @scheme[not-found]
thunk is not given, @scheme[#f] is returned.  (No exception is raised
if the @scheme[args] list is imbalanced, and the search stops at a
non-keyword value.)}


