#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{Syntax}

The syntax of a Scheme program is defined by

@itemize{

 @item{a @defterm{read} phase that processes a character stream into a
       Scheme value, especially one composed of pairs and symbols,
       and}

 @item{an @defterm{expand} phase that processes the value to finish
       parsing the code.}

}

For details on the read phase, see @secref["mz:reader"]. Source code is
normally read in @scheme[read-syntax] mode, otherwise it must be
converted to syntax using @scheme[datum->syntax-object]; the expand
phase is defined in terms of syntax objects.

Expansion recursively processes a syntax-wrapped datum; for details,
see @secref["mz:expansion"]. Ultimately, expansion leads to the
synactic forms described in this section.

A syntactic form is described by a BNF-like notation that describes a
combination of (syntax-wrapped) pairs, symbols, and other data (not a
sequence of characters). In this notation, @scheme[...] indicates zero
or more repetitions of the preceding datum, @scheme[...+] indicates
one or more repetitions, and @scheme[?] means zero or one
instance. Italic sequences of characters play the role of
non-terminals. In particular:

@itemize{

 @item{A sequence that ends in @scheme[_id] refers to a syntax-wrapped
       symbol.}

 @item{A sequence that ends in @scheme[_keyword] refers to a
       syntax-wrapped keyword.}

 @item{A sequence that end with @scheme[_expr] refers to a sub-form
       that is expanded as an expression.}
}

@;------------------------------------------------------------------------
@section{Variable Reference}

@defform/none[id]{

A reference to a module-level or local binding, when @scheme[id] is
not bound as a transformer (see @secref["mz:expansion"]). At run-time,
the reference evaluates to the value in the location associated with
the binding.

When the expander encounters an @scheme[id] that is not bound by a
module or local binding, it converts the expression to @scheme[(#%top
. id)].}

@defform[(#%top . id)]{

A reference to a top-level definition that could bind @scheme[id],
even if @scheme[id] has a local binding in its context. Such
references are disallowed anywhere within a @scheme[module] form.}

@;------------------------------------------------------------------------
@section{Procedure Application}

@defform/none[(proc-expr arg ...)]{

A procedure application, normally, when @scheme[proc-expr] is not an
identifier that has a transformer binding (see
@secref["mz:expansion"]).

More precisely, the expander converts this form to @scheme[(#,
@schemeidfont{#%app} proc-expr arg ...)] using the lexical
context for @schemeidfont{#%app} that is associated with the original
form (i.e., the pair that combines @scheme[proc-expr] and its
arguments). Typically, the lexical context of the pair indicates the
procedure-application @scheme[#%app] that is described next.}

@defform[(#%app proc-expr arg ...)]{

A procedure application. Each @scheme[arg] is one of the following:

 @specsubform[arg-expr]{The resulting value is a non-keyword
                        argument.}

 @specsubform[(code:line keyword arg-expr)]{The resulting value is a
              keyword argument using @scheme[keyword]. Each
              @scheme[keyword] in the application must be distinct.}

The @scheme[proc-expr] and @scheme[_arg-expr]s are evaluated in order,
left to right. If the result of @scheme[proc-expr] is a procedure that
accepts as many arguments as non-@scheme[_keyword]
@scheme[_arg-expr]s, if it accepts arguments for all of the
e@scheme[_keyword]s in the application, and if all required
keyword-based arguments are represented among the @scheme[_keyword]s
in the application, then the procedure is called with the values of
the @scheme[arg-expr]s. Otherwise, the @exnraise[exn:fail:contract].

The continuation of the procedure call is the same as the continuation
of the application expression, so the result(s) of the application
expression is(are) the result(s) of the procedure.

The relative order of @scheme[_keyword]-based arguments matters only
for the order of @scheme[_arg-expr] evaluations; the arguments are
regonized by the applied procedure based on the @scheme[_keyword], and
not their positions. The other @scheme[_arg-expr] values, in contrast,
are recognized by the applied procedure according to their order in
the application form.}


@;------------------------------------------------------------------------
@section{Procedure Expression: @scheme[lambda] and @scheme[case-lambda]}

@defform[(lambda formals* body-expr ...+)]{

An expression that procedures a procedure. The @scheme[formals*]
determines the number of arguments that the procedure accepts. It is
either a simple @scheme[_formals], or one of the extended forms.

A simple @scheme[_formals] has one of the following three forms:

@specsubform[(id ... )]{ The procedure accepts as many non-keyword
       argument values as the number of @scheme[id]s. Each @scheme[id]
       is associated with an argument value by position.}

@specsubform[(id ...+ . rest-id)]{ The procedure accepts any number of
       non-keyword arguments greater or equal to the number of
       @scheme[id]s. When the procedure is applied, the @scheme[id]s
       are associated with argument values by position, and all
       leftover arguments are placed into a list that is associated to
       @scheme[rest-id].}

@specsubform[rest-id]{ The procedure accepts any number of non-keyword
       arguments. All arguments are placed into a list that is
       associated with @scheme[rest-id].}

In addition to the form of a @scheme[_formals], a @scheme[formals*]
can be a sequence of @scheme[_formal*]s optionally ending with a
@scheme[rest-id]:

@specsubform[(formal* ...)]{ Each @scheme[formal*] has the following
       four forms:

        @specsubform[id]{Adds one to both the minimum and maximum
        number of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by
        position.}

        @specsubform[[id default-expr]]{Adds one to the maximum number
        of non-keyword arguments accepted by the procedure. The
        @scheme[id] is associated with an actual argument by position,
        and if no such argument is provided, the @scheme[default-expr]
        is evaluated to produce a value associated with @scheme[id].
        No @scheme[formal*] with a @scheme[default-expr] can appear
        before an @scheme[id] without a @scheme[default-expr] and
        without a @scheme[keyword].}

       @specsubform[(code:line keyword id)]{The procedure requires a
       keyword-based argument using @scheme[keyword]. The @scheme[id]
       is associated with a keyword-based actual argument using
       @scheme[keyword].}

       @specsubform[(code:line keyword [id default-expr])]{The
       procedure accepts a keyword-based using @scheme[keyword]. The
       @scheme[id] is associated with a keyword-based actual argument
       using @scheme[keyword], if supplied in an application;
       otherwise, the @scheme[default-expr] is evaluated to obtain a
       value to associate with @scheme[id].}

      The position of a @scheme[_keyword] @scheme[formal*] in
      @scheme[formals*] does not matter, but each specified
      @scheme[_keyword] must be distinct.}

@specsubform[(formal* ...+ . rest-id)]{ Like the previous case, but
       the procedure accepts any number of non-keyword arguments
       beyond its minimum number of arguments. When more arguments are 
       provided than non-@scheme[_keyword] arguments among the @scheme[formal*]s, 
       the extra arguments are placed into a list that is associated to
       @scheme[rest-id].}

The @scheme[formals*] identifiers are bound in the
@scheme[body-expr]s. When the procedure is applied, a new location is
created for each identifier, and the location is filled with the
associated argument value.

If any identifier appears in @scheme[body-expr]s that is not one of
the identifiers in @scheme[formals*], then it refers to the same
location that it would if it appears in place of the @scheme[lambda]
expression. (In other words, variable reference is lexically scoped.)

When multiple identifiers appear in a @scheme[formals*], they must be
distinct according to @scheme[bound-identifier=?].

If the procedure procedure by @scheme[lambda] is applied to fewer or
more arguments than it accepts, the @exnraise[exn:fail:contract].  If
@scheme[formals*] includes @scheme[keyword]s and an application
includes too few arguments before the keyword section, the same
keyword in multiple odd positions of the keyword section, or a keyword
that is not among the @scheme[formals*] @scheme[keyword]s in an odd
position of the keyword section, then the
@exnraise[exn:fail:contract].}

@defform[(case-lambda [formals body-expr ...+] ...)]{

An expression that procedure. Each @scheme[[forms body-expr ...+]]
clause is analogous to a single @scheme[lambda] procedure; applying
the @scheme[case-lambda]-generated procedure is the same as applying a
procedure that corresponds to one of the clauses---the first procedure
that accepts the given number of arguments. If no corresponding
procedure accepts the given number of arguments, the
@exnraise[exn:fail:contract].

Note that a @scheme[case-lambda] clause supports only
@scheme[formals], not the more general @scheme[_formals*] of
@scheme[lambda]. That is, @scheme[case-lambda] does not directly
support keyword and optional arguments for an inidvidual clause.}
