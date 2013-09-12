#lang scribble/manual
@(require "utils.rkt" (for-label scribble/sigplan))

@title{SIGPLAN Paper Format}

@defmodulelang[scribble/sigplan]{The @racketmodname[scribble/sigplan]
language is like @racketmodname[scribble/base], but configured with
Latex style defaults to use the @filepath{sigplanconf.cls} class
file that is included with Scribble.}

@defidform[preprint]{

Enables the @tt{preprint} option. Use @racket[preprint] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[preprint]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @preprint
}|}

@defidform[10pt]{

Enables the @tt{10pt} option. Use @racket[10pt] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[10pt]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @10pt
}|

@defidform[nocopyright]{

Enables the @tt{nocopyright} option. Use @racket[nocopyright] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[nocopyright]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @nocopyright
}|}

@defidform[onecolumn]{

Enables the @tt{onecolumn} option. Use @racket[onecolumn] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[onecolumn]:

@codeblock|{
  #lang scribble/sigplan @onecolumn
}|}


@defidform[notimes]{

Disables the use of @tt{\usepackage@"{"times@"}"} in the generated LaTeX output.
Use @racket[onecolumn] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[notimes]:

@codeblock|{
  #lang scribble/sigplan @notimes
}|}

@defidform[noqcourier]{

Disables the use of @tt{\usepackage@"{"qcourier@"}"} in the generated LaTeX output.
Use @racket[onecolumn] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/sigplan] and @racket[noqcourier]:

@codeblock|{
  #lang scribble/sigplan @noqcourier
}|}

The @racket[10pt], @racket[preprint], @racket[nocopyright], 
@racket[onecolumn], @racket[notimes], and @racket[noqcourier]
options can be used together and may appear in any order.  
}


@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @racket[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(subtitle [pre-content pre-content?] ...) element?]{

Use as the last argument to @racket[title] to specify a subtitle.}

@defproc[(authorinfo [name pre-content?]
                     [affiliation pre-content?]
                     [email pre-content?])
         block?]{

A replacement for @racket[author] that associates an affiliation and
e-mail address with the author name.}

@deftogether[(
@defproc[(conferenceinfo [conference pre-content?] [location pre-content?]) block?]
@defproc[(copyrightyear [content pre-content?] ...) block?]
@defproc[(copyrightdata [content pre-content?] ...) block?]
@defproc[(doi [content pre-content?] ...) block?]
@defproc[(exclusive-license) block?]
)]{

Declares information that is collected into the copyright region of the paper.}


@deftogether[(
@defproc[(category [CR-number pre-content?] 
                   [subcategory pre-content?]
                   [third-level pre-content?]
                   [fourth-level (or/c #f pre-content?) #f]) content?]
@defproc[(terms [content pre-content?] ...) content?]
@defproc[(keywords [content pre-content?] ...) content?]
)]{

Typesets category, term, and keyword information for the paper, which
is normally placed immediately after an @racket[abstract] form.
See also @url["http://www.acm.org/about/class/how-to-use"].

For @racket[category], the @racket[subcategory] argument should be in
titlecase (i.e., capitalize the first letter of each word) and a
phrase at the level of ``Programming Languages'' or ``Software
Engineering'' (as opposed to a category like ``Software'' or a
third-level name like ``Concurrent Programming'' or ``Processors''). A
@racket[third-level] phrase should be in titlecase.  A
@racket[fourth-level] phrase, if any, should not be capitalized.

For @racket[terms], each general term should be in titlecase. Terms
are usually drawn from a fixed list, and they are usually optional.

For @racket[keywords], capitalize only the first letter of the first
word, separate phrases by commas, and do not include ``and'' before
the last one. Keywords should be noun phrases, not adjectives.}
