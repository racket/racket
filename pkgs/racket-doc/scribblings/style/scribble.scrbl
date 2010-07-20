#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "utils.ss"
          (for-label scribble/manual))

@title[#:tag "reference-style"]{Style Guide}

@section{Prose and Terminology}

In the descriptive body of @racket[defform], @racket[defproc], etc.,
do not start with ``This ...'' Instead, start with a sentence whose
implicit subject is the form or value being described. Capitalize the
first word. Thus, the description will often start with ``Returns'' or
``Produces.''  Refer to arguments and sub-forms by name.

Do not use the word ``argument'' to describe a sub-form in a syntactic
form; use the term ``sub-form'' instead, reserving ``argument'' for
values or expressions in a function call.  Refer to libraries and
languages as such, rather than as ``modules'' (even though the form to
typeset a library or language name is called @racket[racketmodname]).
Do not call an identifier (i.e., a syntactic element) a ``variable''
or a ``symbol.'' Do not use the word ``expression'' for a form that is
a definition or might be a definition; use the word ``form,'' instead.
Prefer ``function'' to ``procedure.''

Avoid cut-and-paste for descriptive text. If two functions are
similar, consider documenting them together with
@racket[deftogether]. To abstract a description, consider using
explicit prose abstraction, such as ``@racket[x] is like @racket[y],
except that ...,'' instead of abstracting the source and instantiating
it multiple times; often, a prose abstraction is clearer to the reader
than a hidden abstraction in the document implementation.

@section{Typesetting Code}

Use @racketidfont{id} or a name that ends @racketidfont{-id} in
@racket[defform] to mean an identifier, not @racketidfont{identifier},
@racketidfont{variable}, @racketidfont{name}, or
@racketidfont{symbol}. Similarly, use @racketidfont{expr} or something
that ends @racketidfont{-expr} for an expression position within a
syntactic form. Use @racketidfont{body} for a form (definition or
expression) in an internal-definition position. Do not use
@racketidfont{expr} for something that isn't exactly an expression,
@racket[id] for something that isn't exactly an identifier, etc.;
instead, use @racket[defform/subs] to define a new non-terminal.

Beware of using @racket[deftogether] to define multiple variants of a
syntactic form or procedure, because each @racket[defform] or
@racket[defproc] creates a definition point, but each form or
procedure should have a single definition point. (Scribble issues a
warning when a binding has multiple definition points.) Instead, use
@racket[defproc*] or @racket[defform*].

Pay attention to the difference between identifiers and meta-variables
when using @racket[racket], especially outside of @racket[defproc] or
@racket[defform]. Prefix a meta-variable with @litchar{_}; for
example,

@verbatim[#:indent 2]|{@racket[(rator-expr rand-expr ...)]}|

would be the wrong way to refer to the grammar of a function call,
because it produces @racket[(rator-expr rand-expr ...)], where
@racketidfont{rator-expr} and @racketidfont{rand-expr} are
typeset as variables. The correct description is

@verbatim[#:indent 2]|{@racket[(_rator-expr _rand-expr ...)]}|

which produces @racket[(_rator-expr _rand-expr ...)], where
@racketidfont{rator-expr} and @racketidfont{rand-expr} are typeset as
meta-variables. The @racket[defproc], @racket[defform], @|etc| forms
greatly reduce this burden in descriptions, since they automatically
set up meta-variable typesetting for non-literal identifiers. In
@racket[defform], be sure to include literal identifiers (i.e., those
not meant as variables, other than the form name being defined) in a
@racket[#:literals] clause.

To typeset an identifier with no particular interpretation---syntax,
variable, meta-variable, etc.---use @racket[racketidfont] (e.g., as in
@racketidfont{rand-expr} above).  Otherwise, use @racket[litchar],
not merely @racket[racketfont] or @racket[verbatim], to refer to a
specific sequence of characters.

When showing example evaluations, use the REPL-snapshot style:

@verbatim[#:indent 2]|{
  @interaction[
  (+ 1 2)
  ]
}|

See also the @racket[scribble/eval] library.

Use four dots, @litchar{....}, in place of omitted code, since
@litchar{...} means repetition.

@section{Typesetting Prose}

Refrain from referring to documentation ``above'' or ``below,'' and
instead have a hyperlink point to the right place.

In prose, use @litchar{``} and @litchar{''} quotation marks instead of
@litchar{"}. Use @litchar{---} for an em-dash, and do not include
spaces on either side, though it will typeset as an en-dash and spaces
in HTML output. Use American style for quotation marks and punctuation
@; [Eli] BTW, I've asked several people about this, and the general
@;   agreement that I've seen is that this is a rather arbitrary rule
@;   and there's no harm in doing the more logical thing of putting
@;   the punctuations outside quotations and parens.  Just like you
@;   did at the end of this sentence...
at the end of quotation marks (i.e., a sentence-terminating period
goes inside the quotation marks). Of course, this rule does not apply
for quotation marks that are part of code.

Do not use a citation reference (as created by @racket[cite]) as a
noun; use it as an annotation.

Do not start a sentence with a Racket variable name, since it is
normally lowercase. For example, use ``The @racket[_thing] argument
is...'' instead of ``@racket[_thing] is...''

@section{Section Titles}

Capitalize all words except articles (``the,'' ``a,'' etc.),
prepositions, and conjunctions that are not at the start of the title.

A manual title should normally start with a suitable keyword or key
phrase (such as ``Scribble'' for this manual) that is in boldface. If
the key word is primarily an executable name, use @racket[exec]
instead of @racket[bold]. Optionally add further descriptive text in
the title after a colon, where the text starting with the colon is not
in boldface.
