#lang scribble/base
@(require scribble/manual
          scribble/eval
          (for-syntax racket/base)
          (for-label racket/base
                     racket/contract/base
                     scribble/manual))

@(define css tt)

@(define-syntax (opt-example stx)
  ;; A #\? 'paren-shape value triggers RktOpt:
   #`@racket[#,(syntax-property #'(in-example) 'paren-shape '#\?)])

@title{Manual All-Styles Document}

@table-of-contents[]

@section{Code Styles}

@itemlist[

 @item{@css{RktSym} (identifier without @racket[for-label] binding): 
       @racket[unbound] or @racketidfont{example}}

 @item{@css{RktValLink} (link to variable form): @racket[cons]}

 @item{@css{RktValDef} (definition of variable, normally combined with @css{RktValLink}):
       @racket[list] in 

       @defproc[#:link-target? #f (list) any/c]
       @defthing[#:link-target? #f list any/c]}

 @item{@css{RktStxLink} (link to syntactic form): @racket[lambda]}

 @item{@css{RktStxDef} (definition of syntactic form, normally combined with @css{RktStxLink}):
       @racket[lambda] in

       @defform[#:link-target? #f (lambda ...)]}

 @item{@css{RktSymDef} (definition without binding, normally a mistake, combined with @css{RktSym}):
       @racket[unbound-identifier] in

       @defform[#:link-target? #f (unbound-identifier)]}

 @item{@css{RktVar} (local variable or meta-variable): @racket[_variable] or @racketvarfont{example}}

 @item{@css{RktRes} (REPL result): @racketresult['(1 2 3)] or @racketresultfont{example}}

 @item{@css{RktOut} (as written to the current output port): @racketoutput{example}}

 @item{@css{RktErr} (errors): @racketerror{example} or the error message in

       @interaction[(+ 1 'a)]}

 @item{@css{RktCmt} (comments): @racketcommentfont{example} or

       @racketblock[(code:comment "comment")]}

 @item{@css{RktVal} (values): @racket['(1 2 3)] or @racketvalfont{example}}

 @item{@css{highlighted} (highlight via background): @racket[(not-this (code:hilite example) nor-this)]}

 @item{@css{RktIn} on a @css{RktInBG}: @litchar{example}}

 @item{@css{RktPn} (parentheses, etc.): @racket[([{}])] or @racketparenfont{example}}

 @item{@css{RktRdr} (reader shorthands): non-parentheses in @racket[(#`() ,@())]}

 @item{@css{RktMeta} (the @racket[unquote] comma):
       @racket[,1] or @racketmetafont{example} or ``#reader'' below.

       @defmodule[@schemeidfont{module} #:module-paths (racket/base) #:reader #:no-declare #:link-target? #f]}

 @item{@css{RktMod} (module name; normally @css{RktModLink} instead): @racketmodfont{example}}

 @item{@css{RktModLink} (a linked module reference): @racketmodname[racket/base]}

 @item{@css{RktOpt} (option-argument brackets): brackets in @opt-example[]}

 @item{@css{RktKw} (not normally used): @racketkeywordfont{example}}

]

The @css{RktBlk} style class is used for a table of multiple lines (more
than 1) of Racket code:

@racketblock[
 (define x (+ 1 2))
 (+ x 3)
]

@section{Definition Blocks}

@defmodule[racket/base #:link-target? #f]

The module-declaration box above is in a @css{defmodule} table. The
package-specification part is in an @css{RpackageSpec} wrapper.

The definitions below are marked so that they are not link targets. If
they were link targets, the table-of-contents panel on the left would
have entries for them.

@defproc[#:link-target? #f (cons [really-long-name-for-the-first-argument
                                  any/c]
                                 [really-long-name-for-the-second-argument
                                  (or/c any/c
                                        any/c)])
         pair?]{

This definition box starts with a @css{SVInsetFlow} wrapper, which is a
@racketmodname[scribble/base] style class for the
@racket['vertical-inset] style name on a block; it should give the
block suitable vertical space before and after.

The next layer is a @css{boxed} plus @css{RBoxed} table. The @css{boxed}
style class is from @racketmodname[scribble/base] and the
@racket['boxed] style name on a table. The @css{RBoxed} style class is
from the @racket[scribble/manual] layer. Both @css{boxed} and @css{RBoxed}
are used for all definition boxes by @racket[scribble/manual] forms.

The initial content of the table includes a @css{SubFlow} (a
@racket[scribble/base] style class for non-indented flow) to combine
blocks for the background label with the first line of the table. The
background label ``procedure'' has an @css{RBackgroundLabel} outer
wrapper, which makes the label float right. (The wrapper also has the
@css{SIEHidden} style class, which built-in for all Scribble HTML output
and makes the label hidden on Internet Explorer 6 and earlier.)  The
background label has an @css{RBackgroundLabelInner} inner wrapper, which
makes the label suitably faint. The content part of the first line is
wrapped in @css{RForeground}, which ensures that it is in front of the
background label.

In a procedure definition box:

@itemlist[

 @item{When the initial ``prototype'' call in the definition box spans
       multiple lines, the table that contains the call has the
       @css{prototype} style class in addition to @css{RForeground}.}

 @item{When the contract or default value for an argument spans
       multiple lines, then the contract, the ``='' for a value (if
       any), and value (if any) are wrapped in an table with the
       @css{argcontract} style class.}

]

Finally, the definition box and all of the associated explanation text
are wrapped in @css{SIntrapara} blocks and grouped into a single
@tt{<p>}.}

@defform[#:link-target? #f 
         (lambda ...)
         #:grammar ([example good
                             bad])]

When a syntactic-from specification has a grammar, the grammar is in a
table with the @css{specgrammar} style class.

Since no explanation flow is attached to the above @racket[defform] use,
there's no @css{SIntrapara} block around the table (just a @tt{<p>}).

@deftogether[(
@defproc[#:link-target? #f (cons [a any/c] [d any/c]) pair?]
@defform[#:link-target? #f (lambda ...)]
)]{

Putting definitions together with @racket[deftogether] converts the
@css{RBoxed} and @css{boxed} tables that would be generated for the
individual definitions into tables with the @css{together} style
class. The tables are then combined as rows in a new table with the
@css{RBoxed} and @css{boxed}  style classes.}

A @racket[defsubform], @racket[specsubform], etc., such as

@specsubform[(lambda ...)]

is indented though a wrapper with a @css{leftindent} style class.

@include-section["demo-class.scrbl"]

@section{Miscellaneous}

In @racket[filebox] rendering,

@(filebox "example.rkt" "This is a file box")

a @css{Rfilebox} wrapper surrounds the file name in a @css{Rfiletitle}
outer wrapper and an @css{Rfilename} inner wrapper, plus the file
content in an @css{Rfilecontent} wrapper.

@inset-flow{The @racket[inset-flow] form generates a
@racket[nested-flow] with style class @css{insetpara}.}

@history[#:changed "1.0" @elem{History paragraphs have the @css{SHistory} style class.}]

@section{Bibliography}

The bibliography table for the citation @cite["Example"] as the
@css{RBibliography} style class.

@bibliography[(bib-entry #:key "Example" #:title "Example bibliography entry")]
