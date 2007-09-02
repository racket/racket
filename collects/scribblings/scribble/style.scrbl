#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "reference-style"]{Style Guide}

In the descriptive body of @scheme[defform], @scheme[defproc], etc.,
do not start with ``This ...'' Instead, start with a sentence whose
implicit subject is the form or value being described. Thus, the
description will often start with ``Produces.'' Refer to arguments by
name.

Use @schemeidfont{id} or something that ends @schemeidfont{-id} in a
syntactic form to mean an identifier, not @schemeidfont{identifier},
@schemeidfont{name}, or @schemeidfont{symbol}. Similarly, use
@schemeidfont{expr} or something that ends @schemeidfont{-expr} for an
expression position within a syntactic form. Use @schemeidfont{body}
for a form (definition or expression) in an internal-definition
position.

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
greatly reduce this burden in description, since they automatically
set up meta-variable typesetting for non-literal identifiers.

To typeset an identifier with no particular interpretation---syntax,
variable, meta-variable, etc.---use @scheme[schemeidfont] (e.g., as in
@schemeidfont{rand-expr} above).  Otherwise, use @scheme[litchar],
not merely @scheme[schemefont] or @scheme[verbatim], to refer to a
specific sequence of characters.

Use American style for quotation marks and punctuation at the end of
quotation marks (i.e., a sentence-terminating period goes inside the
quotation marks). Of course, this rule does not apply for quotation
marks that are part of code.
