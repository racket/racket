#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/manual))

@title[#:tag "reference-style"]{Style Guide}

In the descriptive body of @scheme[defform], @scheme[defproc], etc.,
do not start with ``This ...'' Instead, start with a sentence whose
implicit subject is the form or value being described. Thus, the
description will often start with ``Produces.'' Refer to arguments and
sub-forms by name.

Do not use the word ``argument'' to describe a sub-form in a syntactic
form; use the term ``sub-form'' instead, reserving ``argument'' for
values or expressions in a function call.  Refer to libraries and
languages as such, rather than as ``modules'' (even though the form to
typeset a library or language name is called @scheme[schememodname]).
Do not call an identifier (i.e., a syntactic element) a ``variable''
or a ``symbol.''

Use @schemeidfont{id} or a name that ends @schemeidfont{-id} in
@scheme[defform] to mean an identifier, not @schemeidfont{identifier},
@schemeidfont{variable}, @schemeidfont{name}, or
@schemeidfont{symbol}. Similarly, use @schemeidfont{expr} or something
that ends @schemeidfont{-expr} for an expression position within a
syntactic form. Use @schemeidfont{body} for a form (definition or
expression) in an internal-definition position. Do not use
@schemeidfont{expr} for something that isn't exactly an expression,
@scheme[id] for something that isn't exactly an identifier, etc.;
instead, use @scheme[defform/subs] to define a new non-terminal.

Beware of using @scheme[deftogether] to define multiple variants of a
syntactic form or procedure, because each @scheme[defform] or
@scheme[defproc] creates a definition point, but each form or
procedure should have a single definition point. (Scribble issues a
warning when a binding has multiple definition points.) Instead, use
@scheme[defproc*] or @scheme[defform*].

Pay attention to the difference between identifiers and meta-variables
when using @scheme[scheme], especially outside of @scheme[defproc] or
@scheme[defform]. Prefix a meta-variable with @litchar{_}; for
example,

@verbatim["   @scheme[(rator-expr rand-expr ...)]"]

would be the wrong way to refer to the grammar of a function call,
because it produces @scheme[(rator-expr rand-expr ...)], where
@schemeidfont{rator-expr} and @schemeidfont{rand-expr} are
typeset as variables. The correct description is

@verbatim["   @scheme[(_rator-expr _rand-expr ...)]"]

which produces @scheme[(_rator-expr _rand-expr ...)], where
@schemeidfont{rator-expr} @schemeidfont{rand-expr} are typeset as
meta-variables. The @scheme[defproc], @scheme[defform], @|etc| forms
greatly reduce this burden in descriptions, since they automatically
set up meta-variable typesetting for non-literal identifiers. In
@scheme[defform], be sure to include literal identifiers (i.e., those
not meant as variables, other than the form name being defined) in a
@scheme[#:literals] clause.

To typeset an identifier with no particular interpretation---syntax,
variable, meta-variable, etc.---use @scheme[schemeidfont] (e.g., as in
@schemeidfont{rand-expr} above).  Otherwise, use @scheme[litchar],
not merely @scheme[schemefont] or @scheme[verbatim], to refer to a
specific sequence of characters.

Refrain from referring to documentation ``above'' or ``below,'' and
instead have a hyperlink point to the right place.

Use American style for quotation marks and punctuation at the end of
quotation marks (i.e., a sentence-terminating period goes inside the
quotation marks). Of course, this rule does not apply for quotation
marks that are part of code.

Do not use a citation reference (as created by @scheme[cite]) as a
noun. Use it as an annotation.
