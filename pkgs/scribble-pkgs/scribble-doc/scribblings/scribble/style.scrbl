#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label scribble/manual scribble/eval))

@title[#:tag "reference-style"]{Style Guide}

Consistent style---for terms, typesetting, and prose---makes
documentation clearer. As much as possible, follow the rules listed in
this section. Many of the rules are arbitrary in the sense that a
different choice of rule could work fine, but the only way to make our
documentation consistent is to pick one of the choices.

There are too many rules to absorb easily on a first reading. Re-read
this section after writing documentation for a library or two, and
revisit the section periodically to refresh your memory and check for
new rules.

@section{Prose and Terminology}

In the descriptive body of @racket[defform], @racket[defproc], etc.,
do not start with ``This ...'' Instead, start with a sentence whose
implicit subject is the form or value being described (but only start
the first sentence that way). Capitalize the first word. Thus, the
description will often start with ``Returns'' or ``Produces.''  Refer
to arguments and sub-forms by name.

Do not use the word ``argument'' to describe a sub-form in a syntactic
form; use the term ``sub-form'' instead, reserving ``argument'' for
values or expressions in a function call.  Refer to libraries and
languages as such, rather than as ``modules'' (even though the form to
typeset a library or language name is called @racket[racketmodname]).
Do not call an identifier (i.e., a syntactic element) a ``variable''
or a ``symbol.'' Do not use the word ``expression'' for a form that is
a definition or might be a definition; use the word ``form,'' instead.
Prefer ``function'' to ``procedure.''

Use the word ``list'' only when you mean a run-time value consisting
of the empty list and cons cells; use the word ``sequence'' in other
cases, if you must use any word. For example, do not write that
@racket[begin] has a ``list of sub-forms;'' instead, it has a
``sequence of sub-forms.'' Similarly, do not refer to a ``list of
arguments'' in a function call; just write ``arguments'' if possible,
or write ``sequence of argument expressions.''  (Unfortunately,
``@tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{sequence}'' has acquired a
specific run-time meaning, too, but the collision is less severe than
the historical confusion between lists and other entities in Lisp.)

Avoid cut-and-paste for descriptive text. If two functions are
similar, consider documenting them together with
@racket[deftogether]. To abstract a description, consider using
explicit prose abstraction, such as ``@racket[x] is like @racket[y],
except that ...,'' instead of abstracting the source and instantiating
it multiple times; often, a prose abstraction is clearer to the reader
than a hidden abstraction in the document implementation.

Hyphenate the words ``sub-form'' and ``sub-expression.''

Use ``Windows,'' ``Mac OS X,'' and ``Unix'' for the three
``platforms'' (as opposed to ``systems'') on which Racket runs. Use
``Unix'' as a generic term for Unix-like operating systems---notably
including Linux---other than Mac OS X. Use ``Unix'' even when ``Gtk''
or ``the X11 windowing system'' would be more precisely correct, but
use ``X11'' as adjective when necessary, such as ``X11 display.''
Racket runs ``on'' a platform, as opposed to ``under'' a platform.


@section{Typesetting Code}

Use @racketidfont{id} or a name that ends @racketidfont{-id} in
@racket[defform] to mean an identifier, not @racketidfont{identifier},
@racketidfont{variable}, @racketidfont{name}, or
@racketidfont{symbol}. Similarly, use @racketidfont{expr} or something
that ends @racketidfont{-expr} for an expression position within a
syntactic form. Use @racketidfont{body} for a form (definition or
expression) in an internal-definition position---always followed by 
@racket[...+] in a grammar description. Do not use
@racketidfont{expr} for something that isn't exactly an expression,
@racket[id] for something that isn't exactly an identifier, etc.;
instead, use @racket[defform/subs] to define a new non-terminal.

Beware of using @racket[deftogether] to define multiple variants of a
syntactic form or procedure, because each @racket[defform] or
@racket[defproc] creates a definition point, but each form or
procedure should have a single definition point. (Scribble issues a
warning when a binding has multiple definition points.) Instead, use
@racket[defproc*] or @racket[defform*].

For function arguments, use @racket[v] as the meta-variable for ``any
value.'' Use @racket[x] as a meta-variable only for numerical
values. Other conventions include @racket[lst] for a list and
@racket[proc] for a procedure.

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

When a syntactic form synthesizes an identifier from a given
identifier, use a combination of @racket[racketidfont] and
@racket[racket] to describe the identifiers. For example, if
@racket[_id] is combined with @racketidfont{is-} and @racketidfont{?}
to form @racketidfont{is-}@racket[_id]@racketidfont{?}, then implement
that identifier as
@code[#:lang "at-exp racket"]|{@racketidfont{is-}@racket[id]@racketidfont{?}}|.

When using @racket[defform] to describe a syntactic form, don't
confuse the @racket[#:contracts] clause with a grammar
specification. Use @racket[#:contracts] only for expressions within the
syntactic form, and the contract is a run-time constraint---not a
syntactic constraint, such as requiring a sub-form to be an identifier.
Use @racket[defform/subs] for syntactic constraints.

When showing example evaluations, use the REPL-snapshot style:

@verbatim[#:indent 2]|{
  @interaction[
  (+ 1 2)
  ]
}|

See also the @racket[scribble/eval] library and @secref["examples-style"].

Use four dots, @litchar{....}, in place of omitted code, since
@litchar{...} means repetition.


@section{Typesetting Prose}

Refrain from referring to documentation ``above'' or ``below,'' and
instead have a hyperlink point to the right place.

In prose, use @litchar{``} and @litchar{''} quotation marks instead of
@litchar{"}. Use @litchar{---} for an em dash, and do not include
spaces on either side. Use American style for quotation marks and punctuation
@; [Eli] BTW, I've asked several people about this, and the general
@;   agreement that I've seen is that this is a rather arbitrary rule
@;   and there's no harm in doing the more logical thing of putting
@;   the punctuations outside quotations and parens.  Just like you
@;   did at the end of this sentence...
@; [Matthew] See intro of this section.
at the end of quotation marks (i.e., a sentence-terminating period
goes inside the quotation marks). Of course, this rule does not apply
for quotation marks that are part of code.

Do not use a citation reference (as created by @racket[cite]) as a
noun; use it as an annotation.

Do not start a sentence with a Racket variable name, since it is
normally lowercase. For example, use ``The @racket[_thing] argument
is...'' instead of ``@racket[_thing] is...''

Use @racket[etc] for ``@|etc|'' when it does not end a sentence, and
include a comma after ``@|etc|'' unless it ends a sentence of is
followed by other punctuation (such as a parenthesis).

@section{Section Titles}

Capitalize all words except articles (``the,'' ``a,'' etc.),
prepositions, and conjunctions that are not at the start of the title.

A manual title should normally start with a suitable keyword or key
phrase (such as ``Scribble'' for this manual) that is in boldface. If
the key word is primarily an executable name, use @racket[exec]
instead of @racket[bold]. Optionally add further descriptive text in
the title after a colon, where the text starting with the colon is not
in boldface.

@section{Indexing}

Document and section titles, identifiers that are documented with
@racket[defproc], @racket[defform], etc. are automatically indexed, as
are terms defined with @racket[deftech].

Symbols are not indexed automatically.  Use @racket[indexed-racket]
instead of @racket[racket] for the instance of a symbol that roughly
defines the use. For an example, try searching for ``truncate'' to
find @racket['truncate] as used with @racket[open-output-file]. Do no
use something like @racket[(index "'truncate")] to index a symbol,
because it will not typeset correctly (i.e., in a fixed-width font
with the color of a literal).

Use @racket[index], @racket[as-index], and @racket[section-index] as a
last resort. Create index entries for terms that are completely
different from terms otherwise indexed. Do not try to index minor
variations of a term or phrase in an attempt to improve search
results; if search fails to find a word or phrase due to a minor
variation, then the search algorithm should be fixed, not the index
entry.

@section[#:tag "examples-style"]{Examples}

Strive to include examples (using @racket[examples]) with the
documentation of every function and syntactic form. When writing
examples, refrain from using nonsense words like ``foo'' and ``bar.''
For example, when documenting @racket[member], resist the temptation
to write

@interaction[
(member "foo" '("bar" "foo" "baz"))
]

and instead write something like

@interaction[
(member "Groucho" '("Harpo" "Groucho" "Zeppo"))
]
