#lang scribble/manual
@(require "utils.ss"
          (for-label scribble/sigplan))

@title{SIGPLAN Paper Format}

@defmodulelang[scribble/sigplan]{The @schememodname[scribble/sigplan]
language is like @schememodname[scribble/manual], but configured with
Latex style defaults to use the @filepath{sigplanconf.cls} class
file that is included with Scribble.}

@defidform[preprint]{

Enables the @tt{preprint} option. Use @scheme[preprint] only on the
same line as @hash-lang[], with only whitespace between
@schememodname[scribble/sigplan] and @scheme[preprint]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @preprint
}|}

@defidform[10pt]{

Enables the @tt{10pt} option. Use @scheme[10pt] only on the
same line as @hash-lang[], with only whitespace between
@schememodname[scribble/sigplan] and @scheme[10pt]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @10pt
}|

The @scheme[10pt] and @scheme[preprint] options can be
used together and may appear in any order.
}


@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @scheme[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(authorinfo [name pre-content?]
                     [affiliation pre-content?]
                     [email pre-content?])
         block?]{

A replacement for @scheme[author] that associates an affiliation and
e-mail address with the author name.}

@deftogether[(
@defproc[(conferenceinfo [conference pre-content?] [location pre-content?]) block?]
@defproc[(copyrightyear [content pre-content?] ...) block?]
@defproc[(copyrightdata [content pre-content?] ...) block?]
)]{

Declares information that is collected into the copyright region of the paper.}


@deftogether[(
@defproc[(category [CR-number pre-content?] 
                   [subcategory pre-content?]
                   [third-level pre-content?]
                   [fourth-level (or/c #f pre-content?) #f]) block?]
@defproc[(terms [content pre-content?] ...) block?]
@defproc[(keywords [content pre-content?] ...) block?]
)]{

Typesets category, term, and keyword information for the paper, which
is normally placed immediately after an @scheme[abstract] form.}
