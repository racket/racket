#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          "utils.ss"
          (for-label scheme/base))

@(define (nested . str)
   (make-blockquote #f (flow-paragraphs (decode-flow str))))
@(define (fake-title . str) (apply bold str))

@title[#:tag "config"]{Extending and Configuring Scribble Output}

Sometimes, Scribble's primitives and built-in styles are insufficient
to produce the output that you need. The cases in which you need to
extend or configure Scribble fall into two groups:

@itemize[

 @item{You may need to drop into the back-end ``language'' of CSS or
       Tex to create a specific output effect. For this kind of
       extension, you will mostly likely attach a @scheme[`(css
       ,_file)] or @scheme[`(tex ,_file)] style to a @scheme[section]
       and then use a string defined in the @scheme[_file] as an
       @scheme[element] or @tech{block} style. This kind of extension
       is described in @secref["extra-style"].}

 @item{You may need to produce a document whose page layout is
       different from the PLT Scheme documentation style. For that
       kind of configuration, you will most likely run the
       @exec{scribble} command-line tool and supply flags like
       @DFlag{prefix} or @DPFlag{style}. This kind of configuration
       is described in @secref["config-style"].}

]

@; ------------------------------------------------------------

@section[#:tag "extra-style" 
         #:style `((css "inbox.css") (tex "inbox.tex"))]{Adding a Style}

When a string is uses as a style in an @scheme[element],
@scheme[styled-paragraph], @scheme[table],
@scheme[styled-itemization], @scheme[blockquote], or @scheme[compound-paragraph], it corresponds to
a CSS class for HTML output or a Tex macro/environment for Latex
output. In Latex output, the string is used as a command name for a
@scheme[styled-paragraph] and an environment name for a
@scheme[table], @scheme[itemization], @scheme[blockquote], or @scheme[compound-paragraph], except
that a @scheme[blockquote] or @scheme[compound-paragraph] style name that starts with @litchar{\} is
used (sans @litchar{\}) as a command instead of an environment.
In addition, for an itemization, the style string is
suffixed with @scheme["Item"] and used as a CSS class or Tex macro
name to use for the itemization's items (in place of @tt{item} in the
case of Latex).

Scribble includes a number of predefined styles that are used by the
exports of @scheme[scribble/manual], but they are not generally
intended for direct use. For now, use them or redefine them at your
own risk. 

To add a mapping from your own style name to a CSS configuration, add
a @scheme[`(css ,_file)] style (in a list of styles) to an enclosing
@scheme[part]. To map a style name to a Tex macro (or Latex
environment), add a @scheme[`(tex ,_file)] style to an enclosing part.

To avoid collisions with future additions to Scribble, start your
style name with an uppercase letter that is not @litchar{S}. An
uppercase letter helps to avoid collisions with macros defined by
Latex packages, and future styles needed by @scheme[scribble/manual]
will start with @litchar{S}.

For example, a Scribble document

@verbatim[#:indent 2]|{
 #lang scribble/doc
 @(require manual)

 @title[#:style `((css "inbox.css") (tex "inbox.tex"))]{Quantum Pet}

 Do not open: @elem[#:style "InBox"]{Cat}
}|

combined with an @filepath{inbox.css} that contains

@verbatim[#:indent 2]|{
  .inbox {
    padding: 0.2em;
    border: 1px solid #000000;
  }
}|

and an @filepath{inbox.tex} that contains

@verbatim[#:indent 2]|{
  \newcommand{\InBox}[1]{\fbox{#1}}
}|

generates

@nested{
 @fake-title{Quantum Pet}

 Do not open: @elem[#:style "InBox"]{Cat}
}

@; ------------------------------------------------------------

@section[#:tag "config-style"]{Configuring Output}

Scribble's output is configured in two layers:

@itemize[

 @item{A prefix determines the @tt{DOCTYPE} line for HTML output or
       the @tt{documentclass} configuration (and perhaps some addition
       package uses or other configuration) for Latex output. The
       default prefix is @filepath{scribble-prefix.html} or
       @filepath{scribble-prefix.tex} in the @filepath{scribble}
       collection.}

 @item{Style definitions for all of the ``built-in'' styles used by
        @scheme[scribble/manual] (as described in
        @secref["extra-style"]).  The default style definitions are
        @filepath{scribble.css} or @filepath{scribble.tex} in the
        @filepath{scribble} collection.}

]

When using the @exec{scribble} command-line utility:

@itemize[

 @item{Replace the prefix using the @as-index{@DFlag{prefix}} flag.}

 @item{Replace the style definitions using the
       @as-index{@DFlag{style}} flag.}

 @item{Add style definitions (that can override earlier ones)
       using the @as-index{@DPFlag{style}} flag.}

]

For now, reading the default files is the best way to understand how
they interact.
