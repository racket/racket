#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/eval
          "utils.ss"
          (for-syntax scheme/base)
          (for-label (only-in scribble/reader
                              use-at-readtable)))

@(define read-eval (make-base-eval))
@(interaction-eval #:eval read-eval (require (for-syntax scheme/base)))

@title[#:tag "reader"]{@"@"-Reader}

The Scribble @"@"-reader is designed to be a convenient facility for
using free-form text in Scheme code, where ``@"@"'' is chosen as one of
the least-used characters in Scheme code.

You can use the reader via Scheme's @schemefont{#reader} form:

@schemeblock[
 @#,schemefont|{
     #reader scribble/reader @foo{This is free-form text!}
}|]

or use the @scheme[at-exp] meta-language as described in
@secref["at-exp-lang"].

Note that the Scribble reader reads @"@"-forms as S-expressions.  This
means that it is up to you to give meanings for these expressions in
the usual way: use Scheme functions, define your functions, or require
functions.  For example, typing the above into @exec{mzscheme} is likely
going to produce a ``reference to undefined identifier'' error, unless
@scheme[foo] is defined. You can use @scheme[string-append] instead,
or you can define @scheme[foo] as a function (with variable arity).

A common use of the Scribble @"@"-reader is when using Scribble as a
documentation system for producing manuals.  In this case, the manual
text is likely to start with

@schememod[scribble/doc]

which installs the @"@" reader starting in ``text mode,'' wraps the
file content afterward into a Scheme module where many useful Scheme
and documentation related functions are available, and parses the body
into a document using @schememodname[scribble/decode].  See
@secref["docreader"] for more information.

Another way to use the reader is to use the @scheme[use-at-readtable]
function to switch the current readtable to a readtable that parses
@"@"-forms.  You can do this in a single command line:

@commandline{mzscheme -ile scribble/reader "(use-at-readtable)"}

@;--------------------------------------------------------------------
@section{Concrete Syntax}

@subsection{The Scribble Syntax at a Glance}

Informally, the concrete syntax of @"@"-forms is

@schemeblock[
 @#,BNF-seq[@litchar["@"]
            @nonterm{cmd}
            @litchar{[} @kleenestar{@nonterm{datum}} @litchar{]}
            @litchar["{"] @kleenestar{@nonterm{text-body}} @litchar["}"]]
]

where all three parts after @litchar["@"] are optional, but at least
one should be present.  (Note that spaces are not allowed between the
three parts.)  Roughly, a form matching the above grammar is read as

@schemeblock[
  (@#,nonterm{cmd} @#,kleenestar{@nonterm{datum}} @#,kleenestar{@nonterm{parsed-body}})
]

where @nonterm{parsed-body} is the translation of each
@nonterm{text-body} in the input.  Thus, the initial @nonterm{cmd}
determines the Scheme code that the input is translated into.  The
common case is when @nonterm{cmd} is a Scheme identifier, which reads
as a plain Scheme form, with datum arguments and/or string arguments.

@scribble-examples|==={
  @foo{blah blah blah}
  @foo{blah "blah" (`blah'?)}
  @foo[1 2]{3 4}
  @foo[1 2 3 4]
  @foo[#:width 2]{blah blah}
  @foo{blah blah
       yada yada}
  @foo{
    blah blah
    yada yada
  }
}===|

(Note that these examples show how an input syntax is read as Scheme
syntax, not what it evaluates to.)

As seen in the last example, multiple lines and the newlines that
separate them are parsed to multiple Scheme strings.  More generally,
a @nonterm{text-body} is made of text, newlines, and nested
@"@"-forms, where the syntax for @"@"-forms is the same whether it's
in a @nonterm{text-body} context as in a Scheme context.  A
@nonterm{text-body} that isn't an @"@"-form is converted to a string
expression for its @nonterm{parsed-body}; newlines and following
indentations are converted to @scheme["\n"] and all-space string
expressions.

@scribble-examples|==={
  @foo{bar @baz{3}
       blah}
  @foo{@b{@u[3] @u{4}}
       blah}
  @C{while (*(p++))
       *p = '\n';}
}===|

The command part of an @"@"-form is optional as well, which is read as
a list, usually a function application, but also useful when quoted
with the usual Scheme @scheme[quote]:

@scribble-examples|==={
  @{blah blah}
  @{blah @[3]}
  '@{foo
     bar
     baz}
}===|

But we can also drop the datum and text parts, which leaves us with
only the command --- which is read as is, not within a parenthesized
form.  This is not useful when reading Scheme code, but it can be used
inside a text block to escape a Scheme identifier.  A vertical bar
(@litchar{|}) can be used to delimit the escaped identifier when
needed.

@scribble-examples|==={
  @foo
  @{blah @foo blah}
  @{blah @foo: blah}
  @{blah @|foo|: blah}
}===|

Actually, the command part can be any Scheme expression, which is
particularly useful with such escapes since they can be used with any
expression.

@scribble-examples|==={
  @foo{(+ 1 2) -> @(+ 1 2)!}
  @foo{A @"string" escape}
}===|

Note that an escaped Scheme string is merged with the surrounding text
as a special case.  This is useful if you want to use the special
characters in your string (but note that escaping braces is not
necessary if they are balanced).

@scribble-examples|==={
  @foo{eli@"@"barzilay.org}
  @foo{A @"{" begins a block}
  @C{while (*(p++)) {
       *p = '\n';
     }}
}===|

In some cases a @"@"-rich text can become cumbersome to quote.  For
this, the braces have an alternative syntax --- a block of text can
begin with a ``@litchar["|{"]'' and terminated accordingly with a
``@litchar["}|"]''.  Furthermore, any nested @"@" forms must begin
with a ``@litchar["|@"]''.

@scribble-examples|==={
  @foo|{bar}@{baz}|
  @foo|{bar |@x{X} baz}|
  @foo|{bar |@x|{@}| baz}|
}===|

In cases when even this is not convenient enough, punctuation
characters can be added between the @litchar{|} and the braces and the
@"@" in nested forms.  (The punctuation is mirrored for parentheses
and @litchar{<>}s.)  With this, the Scribble syntax can be used as a
here-string replacement.

@scribble-examples|==={
  @foo|--{bar}@|{baz}--|
  @foo|<<{bar}@|{baz}>>|
}===|

The flip side of this is: how can an @"@" sign be used in Scheme code?
This is almost never an issue, because Scheme strings and characters
are still read the same, and because @litchar["@"] is set as a
non-terminating reader macro so it can be used in Scheme identifiers
as usual, except when it is the first character of an identifier.  In
this case, you need to quote the identifier like other non-standard
characters --- with a backslash or with vertical bars:

@scribble-examples|==={
  (define \@email "foo@bar.com")
  (define |@atchar| #\@)
}===|

Note that spaces are not allowed before a @litchar{[} or a
@litchar["{"], or they will be part of the following text (or Scheme
code).  (More on using braces in body texts below.)

@scribble-examples|==={
  @foo{bar @baz[2 3] {4 5}}
}===|

Finally, remember that the Scribble is just an alternate for
S-expressions --- identifiers still get their meaning, as in any
Scheme code, through the lexical context in which they appear.
Specifically, when the above @"@"-form appears in a Scheme expression
context, the lexical environment must provide bindings for
@scheme[foo] as a procedure or a macro; it can be defined, required,
or bound locally (with @scheme[let], for example).

@; FIXME: unfortunate code duplication
@interaction[
(eval:alts
  (let* ([formatter (lambda (fmt)
          (lambda args (format fmt (apply string-append args))))]
         [bf (formatter "*~a*")]
         [it (formatter "/~a/")]
         [ul (formatter "_~a_")]
         [text string-append])
    #,(tt "@text{@it{Note}: @bf{This is @ul{not} a pipe}.}"))
  (let* ([formatter (lambda (fmt)
          (lambda args (format fmt (apply string-append args))))]
         [bf (formatter "*~a*")]
         [it (formatter "/~a/")]
         [ul (formatter "_~a_")]
         [text string-append])
    @text{@it{Note}: @bf{This is @ul{not} a pipe}.}))
]

When you first experiment with the Scribble syntax, it is often useful
to use Scheme's @scheme[quote] to inspect how some concrete syntax is
being read.

@; FIXME: unfortunate code duplication
@interaction[
(eval:alts
  #,(tt "'@foo{bar}")
  '@foo{bar})
]

@;--------------------------------------------------------------------
@subsection{The Command Part}

Besides being a Scheme identifier, the @nonterm{cmd} part of an
@"@"-form can have Scheme punctuation prefixes, which will end up
wrapping the @italic{whole} expression.

@scribble-examples|==={
  @`',@foo{blah}
  @#`#'#,@foo{blah}
}===|

When writing Scheme code, this means that @litchar|{@`',@foo{blah}}|
is exactly the same as @litchar|{`@',@foo{blah}}| and
@litchar|{`',@@foo{blah}}|, but unlike the latter two, the first
construct can appear in body texts with the same meaning, whereas the
other two would not work (see below).

After the optional punctuation prefix, the @nonterm{cmd} itself is not
limited to identifiers; it can be @italic{any} Scheme expression.

@scribble-examples|==={
  @(lambda (x) x){blah}
  @`(unquote foo){blah}
}===|

In addition, the command can be omitted altogether, which will omit it
from the translation, resulting in an S-expression that usually
contains, say, just strings:

@scribble-examples|==={
  @{foo bar
    baz}
  @'{foo bar
     baz}
}===|

If the command part begins with a @litchar{;} (with no newline between
the @litchar["@"] and the @litchar{;}), then the construct is a
comment.  There are two comment forms, one for arbitrary-text and
possibly nested comments, and another one for line comments:

@schemeblock[
@#,BNF-seq[@litchar["@;{"] @kleenestar{@nonterm{any}} @litchar["}"]]

@#,BNF-seq[@litchar["@;"] @kleenestar{@nonterm{anything-else-without-newline}}]
]

In the first form, the commented body must still parse correctly; see
the description of the body syntax below.  In the second form, all
text from the @litchar["@;"] to the end of the line @italic{and} all
following spaces (or tabs) are part of the comment (similar to
@litchar{%} comments in TeX).

@scribble-examples|==={
  @foo{bar @; comment
       baz@;
       blah}
}===|

Tip: if you're editing in a Scheme-aware editor (like DrScheme or
Emacs), it is useful to comment out blocks like this:

@verbatim[#:indent 2]|==={
  @;{
    ...
  ;}
}===|

so the editor does not treat the file as having unbalanced
parenthesis.

If only the @nonterm{cmd} part of an @"@"-form is specified, then the
result is the command part only, without an extra set of parenthesis.
This makes it suitable for Scheme escapes in body texts.  (More on this
below, in the description of the body part.)

@scribble-examples|==={
  @foo{x @y z}
  @foo{x @(* y 2) z}
  @{@foo bar}
}===|

Finally, note that there are currently no special rules for using
@litchar["@"] in the command itself, which can lead to things like:

@scribble-examples|==={
  @@foo{bar}{baz}
}===|

@;--------------------------------------------------------------------
@subsection{The Datum Part}

The datum part can contains arbitrary Scheme expressions, which
are simply stacked before the body text arguments:

@scribble-examples|==={
  @foo[1 (* 2 3)]{bar}
  @foo[@bar{...}]{blah}
}===|

The body part can still be omitted, which is essentially an
alternative syntax for plain (non-textual) S-expressions:

@scribble-examples|==={
  @foo[bar]
  @foo{bar @f[x] baz}
}===|

The datum part can be empty, which makes no difference, except when
the body is omitted.  It is more common, however, to use an empty body
for the same purpose.

@scribble-examples|==={
  @foo[]{bar}
  @foo[]
  @foo
  @foo{}
}===|

The most common use of the datum part is for Scheme forms that expect
keyword-value arguments that precede the body of text arguments.

@scribble-examples|==={
  @foo[#:style 'big]{bar}
}===|

@;--------------------------------------------------------------------
@subsection{The Body Part}

The syntax of the body part is intended to be as convenient as
possible for free text.  It can contain almost any text---the only
characters with special meaning is @litchar["@"] for sub-@"@"-forms,
and @litchar["}"] for the end of the text.  In addition, a
@litchar["{"] is allowed as part of the text, and it makes the
matching @litchar["}"] be part of the text too---so balanced braces
are valid text.

@scribble-examples|==={
  @foo{f{o}o}
  @foo{{{}}{}}
}===|

As described above, the text turns to a sequence of string arguments
for the resulting form.  Spaces at the beginning and end of lines are
discarded, and newlines turn to individual @scheme["\n"] strings
(i.e., they are not merged with other body parts); see also the
information about newlines and indentation below. Spaces are
@italic{not} discarded if they appear after the open @litchar["{"]
(before the closing @litchar["}"]) when there is also text that
follows (precedes) it; specifically, they are preserved in a
single-line body.

@scribble-examples|==={
  @foo{bar}
  @foo{ bar }
  @foo[1]{ bar }
}===|

If @litchar["@"] appears in a body, then it is interpreted as Scheme
code, which means that the @"@"-reader is applied recursively, and the
resulting syntax appears as part of the S-expression, among other
string contents.

@scribble-examples|==={
  @foo{a @bar{b} c}
}===|

If the nested @"@" construct has only a command---no body or datum
parts---it will not appear in a subform.  Given that the command part
can be any Scheme expression, this makes @"@" a general escape to
arbitrary Scheme code.

@scribble-examples|==={
  @foo{a @bar c}
  @foo{a @(bar 2) c}
}===|

This is particularly useful with strings, which can be used to include
arbitrary text.

@scribble-examples|==={
  @foo{A @"}" marks the end}
}===|

Note that the escaped string is (intentionally) merged with the rest
of the text.  This works for @litchar["@"] too:

@scribble-examples|==={
  @foo{The prefix: @"@".}
  @foo{@"@x{y}" --> (x "y")}
}===|

@;--------------------------------------------------------------------
@subsubsub*section{Alternative Body Syntax}

In addition to the above, there is an alternative syntax for the body,
one that specifies a new marker for its end: use @litchar["|{"] for
the opening marker to have the text terminated by a @litchar["}|"].

@scribble-examples|==={
  @foo|{...}|
  @foo|{"}" follows "{"}|
  @foo|{Nesting |{is}| ok}|
}===|

This applies to sub-@"@"-forms too---the @litchar["@"] must be
prefixed with a @litchar{|}:

@scribble-examples|==={
  @foo|{Maze
        |@bar{is}
        Life!}|
  @t|{In |@i|{sub|@"@"s}| too}|
}===|

Note that the subform uses its own delimiters, @litchar{{...}} or
@litchar{|{...}|}.  This means that you can copy and paste Scribble
text with @"@"-forms freely, just prefix the @litchar["@"] if the
immediate surrounding text has a prefix.

For even better control, you can add characters in the opening
delimiter, between the @litchar{|} and the @litchar["{"].
Characters that are put there (non alphanumeric ASCII characters only,
excluding @litchar["{"] and @litchar["@"]) should also be used for
sub-@"@"-forms, and the end-of-body marker should have these characters
in reverse order with paren-like characters (@litchar{(},
@litchar{[}, @litchar{<}) mirrored.

@scribble-examples|==={
  @foo|<<<{@x{foo} |@{bar}|.}>>>|
  @foo|!!{X |!!@b{Y}...}!!|
}===|

Finally, remember that you can use an expression escape with a Scheme
string for confusing situations.  This works well when you only need
to quote short pieces, and the above works well when you have larger
multi-line body texts.

@;--------------------------------------------------------------------
@subsubsub*section{Scheme Expression Escapes}

In some cases, you may want to use a Scheme identifier (or a number or
a boolean etc.) in a position that touches the following text; in
these situations you should surround the escaped Scheme expression by
a pair of @litchar{|} characters.  The text inside the bars is
parsed as a Scheme expression.

@scribble-examples|==={
  @foo{foo@bar.}
  @foo{foo@|bar|.}
  @foo{foo@3.}
  @foo{foo@|3|.}
}===|

This form is a generic Scheme expression escape, there is no body text
or datum part when you use this form.

@scribble-examples|==={
  @foo{foo@|(f 1)|{bar}}
  @foo{foo@|bar|[1]{baz}}
}===|

This works for string expressions too, but note that unlike the above,
the string is (intentionally) not merged with the rest of the text:

@scribble-examples|==={
  @foo{x@"y"z}
  @foo{x@|"y"|z}
}===|

Expression escapes also work with @italic{any} number of expressions,

@scribble-examples|==={
  @foo{x@|1 (+ 2 3) 4|y}
  @foo{x@|*
          *|y}
}===|

It seems that @litchar["@||"] has no purpose---but remember that these escapes
are never merged with the surrounding text, which can be useful when
you want to control the sub expressions in the form.

@scribble-examples|==={
  @foo{Alice@||Bob@|
       |Carol}
}===|

Note that @litchar["@|{...}|"] can be parsed as either an escape expression or
as the Scheme command part of a @"@"-form.  The latter is used in this case
(since there is little point in Scheme code that uses braces.

@scribble-examples|==={
  @|{blah}|
}===|

@;--------------------------------------------------------------------
@subsubsub*section{Comments}

As noted above, there are two kinds of Scribble comments: @litchar|{@;{...}}| is
a (nestable) comment for a whole body of text (following the same
rules for @"@"-forms), and @litchar|{@;...}| is a line-comment.

@scribble-examples|==={
  @foo{First line@;{there is still a
                    newline here;}
       Second line}
}===|

One useful property of line-comments is that they continue to the end
of the line @italic{and} all following spaces (or tabs).  Using this,
you can get further control of the subforms.

@scribble-examples|==={
  @foo{A long @;
       single-@;
       string arg.}
}===|

Note how this is different from using @litchar["@||"]s in that strings
around it are not merged.

@;--------------------------------------------------------------------
@subsubsub*section{Spaces, Newlines, and Indentation}

The Scribble syntax treats spaces and newlines in a special way is
meant to be sensible for dealing with text.  As mentioned above,
spaces at the beginning and end of body lines are discarded, except
for spaces between a @litchar["{"] and text, or between text and a
@litchar["}"].

@scribble-examples|==={
  @foo{bar}
  @foo{ bar }
  @foo{ bar
       baz }
}===|

A single newline that follows an open brace or precedes a closing
brace is discarded, unless there are only newlines in the body; other
newlines are read as a @scheme["\n"] string

@scribble-examples|==={
  @foo{bar
  }
  @foo{
    bar
  }
  @foo{

    bar

  }
  @foo{
    bar

    baz
  }
  @foo{
  }
  @foo{

  }
  @foo{ bar
       baz }
}===|

In the parsed S-expression syntax, a single newline string is used for
all newlines; you can use @scheme[eq?] to identify this line.  This
can be used to identify newlines in the original @nonterm{text-body}.

@; FIXME: unfortunate code duplication (again):
@interaction[
(eval:alts
  (let ([nl (car @#,tt["@'{"]
                 @#,tt["  }"])])
    (for-each (lambda (x) (display (if (eq? x nl) "\n... " x)))
              @#,tt["@`{foo"]
              @#,elem[@tt["   @"] @scheme[,@(list "bar" "\n" "baz")]]
              @#,tt["   blah}}"])
    (newline))
  (let ([nl (car @'{
                   })])
    (for-each (lambda (x) (display (if (eq? x nl) "\n... " x)))
              @`{foo
                 @,@(list "bar" "\n" "baz")
                 blah})
    (newline)))
]

Spaces at the beginning of body lines do not appear in the resulting
S-expressions, but the column of each line is noticed, and all-space
indentation strings are added so the result has the same indentation.
A indentation string is added to each line according to its distance
from the leftmost syntax object (except for empty lines).  (Note: if
you try these examples on a mzscheme REPL, you should be aware that
the reader does not know about the ``@litchar{> }'' prompt.)

@scribble-examples|==={
  @foo{
    bar
    baz
    blah
  }
  @foo{
    begin
      x++;
    end}
  @foo{
      a
     b
    c}
}===|

If the first string came from the opening @litchar["{"] line, it is
not prepended with an indentation (but it can affect the leftmost
syntax object used for indentation).  This makes sense when formatting
structured code as well as text (see the last example in the following
block).

@scribble-examples|==={
  @foo{bar
         baz
       bbb}
  @foo{ bar
          baz
        bbb}
  @foo{bar
     baz
     bbb}
  @foo{ bar
     baz
     bbb}
  @foo{ bar
     baz
       bbb}
  @text{Some @b{bold
    text}, and
    more text.}
}===|

Note that each @"@"-form is parsed to an S-expression that has its own
indentation.  This means that Scribble source can be indented like
code, but if indentation matters then you may need to apply
indentation of the outer item to all lines of the inner one.  For
example, in

@litchar/lines|==={
  @code{
    begin
      i = 1, r = 1
      @bold{while i < n do
              r *= i++
            done}
    end
  }
}===|

a formatter will need to apply the 2-space indentation to the
rendering of the @scheme[bold] body.

Note that to get a first-line text to be counted as a leftmost line,
line and column accounting should be on for the input port
(@scheme[use-at-readtable] turns them on for the current input port).
Without this,

@litchar/lines|==={
  @foo{x1
         x2
         x3}
}===|

will not have 2-space indentations in the parsed S-expression if
source accounting is not on, but

@litchar/lines|==={
  @foo{x1
         x2
       x3}
}===|

will (due to the last line).  Pay attention to this, as it can be a
problem with Scheme code, for example:

@litchar/lines|==={
  @code{(define (foo x)
          (+ x 1))}
}===|

For rare situations where spaces at the beginning (or end) of lines
matter, you can begin (or end) a line with a "@||".

@scribble-examples|==={
  @foo{
    @|| bar @||
    @|| baz}
}===|

@;--------------------------------------------------------------------
@section{Syntax Properties}

The Scribble reader attaches properties to syntax objects.  These
properties might be useful in some rare situations.

Forms that Scribble reads are marked with a @scheme['scribble]
property, and a value of a list of three elements: the first is
@scheme['form], the second is the number of items that were read from
the datum part, and the third is the number of items in the body part
(strings, sub-forms, and escapes).  In both cases, a @scheme[0] means
an empty datum/body part, and @scheme[#f] means that the corresponding
part was omitted.  If the form has neither parts, the property is not
attached to the result.  This property can be used to give different
meanings to expressions from the datum and the body parts, for
example, implicitly quoted keywords:

@; FIXME: a bit of code duplication here
@def+int[
  #:eval read-eval
  (define-syntax (foo stx)
    (let ([p (syntax-property stx 'scribble)])
      (printf ">>> ~s\n" (syntax->datum stx))
      (syntax-case stx ()
        [(_ x ...)
         (and (pair? p) (eq? (car p) 'form) (even? (cadr p)))
         (let loop ([n (/ (cadr p) 2)]
                    [as '()]
                    [xs (syntax->list #'(x ...))])
           (if (zero? n)
             (with-syntax ([attrs (reverse as)]
                           [(x ...) xs])
               #'(list 'foo `attrs x ...))
             (loop (sub1 n)
                   (cons (with-syntax ([key (car xs)]
                                       [val (cadr xs)])
                           #'(key ,val))
                         as)
                   (cddr xs))))])))
  (eval:alts
   (code:line
    @#,tt["@foo[x 1 y (* 2 3)]{blah}"])
    ;; Unfortunately, expressions are preserved by `def+int'
    ;; using `quote', not `quote-syntax' (which would create all sorts
    ;; or binding trouble), so we manually re-attach the property:
    (eval (syntax-property #'@foo[x 1 y (* 2 3)]{blah}
                           'scribble '(form 4 1))))
]

In addition, the Scribble parser uses syntax properties to mark syntax
items that are not physically in the original source --- indentation
spaces and newlines.  Both of these will have a @scheme['scribble]
property; an indentation string of spaces will have
@scheme['indentation] as the value of the property, and a newline will
have a @scheme['(newline S)] value where @scheme[S] is the original
newline string including spaces that precede and follow it (which
includes the indentation for the following item).  This can be used to
implement a verbatim environment: drop indentation strings, and use
the original source strings instead of the single-newline string.  Here
is an example of this.

@; FIXME: a bit of code duplication here
@def+int[
  #:eval read-eval
  (define-syntax (verb stx)
    (syntax-case stx ()
      [(_ cmd item ...)
       #`(cmd
          #,@(let loop ([items (syntax->list #'(item ...))])
               (if (null? items)
                 '()
                 (let* ([fst  (car items)]
                        [prop (syntax-property fst 'scribble)]
                        [rst  (loop (cdr items))])
                   (cond [(eq? prop 'indentation) rst]
                         [(not (and (pair? prop)
                                    (eq? (car prop) 'newline)))
                          (cons fst rst)]
                         [else (cons (datum->syntax-object
                                      fst (cadr prop) fst)
                                     rst)])))))]))
  (eval:alts
   (code:line
    @#,tt["@verb[string-append]{"]
    @#,tt["  foo"]
    @#,tt["    bar"]
    @#,tt["}"])
   @verb[string-append]{
     foo
       bar
   })
]

@;--------------------------------------------------------------------
@section[#:tag "at-exp-lang"]{Adding @"@"-expressions to a Language}

@defmodulelang[at-exp]{The @schememodname[at-exp] language installs
@"@"-reader support in the readtable, and then chains to the reader of
another language that is specified immediate after
@schememodname[at-exp].}

For example, @scheme[@#,hash-lang[] at-exp scheme/base] adds @"@"-reader
support to @scheme[scheme/base], so that

@schememod[
at-exp scheme/base

(define (greet who) @#,elem{@tt["@"]@scheme[string-append]@schemeparenfont["{"]@schemevalfont{Hello, }@tt["@|"]@scheme[who]@tt["|"]@schemevalfont{.}@schemeparenfont["}"]})
(greet "friend")]

reports @scheme["Hello, friend."].

@;--------------------------------------------------------------------
@section{Interface}

@defmodule[scribble/reader]{The @schememodname[scribble/reader] module
provides direct Scribble reader functionality for advanced needs.}

@; The `with-scribble-read' trick below shadows `read' and
@;  `read-syntax' with for-label bindings from the Scribble reader

@(define-syntax with-scribble-read
   (syntax-rules ()
     [(_)
      (...
       (begin
         (require (for-label scribble/reader))

@; *** Start reader-import section ***
@defproc[(read [in input-port? (current-input-port)]) any]{}
@defproc[(read-syntax [source-name any/c (object-name in)]
                      [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{
These procedures implement the Scribble reader.  They do so by
constructing a reader table based on the current one, and using that
for reading.
}

@defproc[(read-inside [in input-port? (current-input-port)]) any]{}
@defproc[(read-syntax-inside [source-name any/c (object-name in)]
                             [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{
These @schemeid[-inside] variants parse as if starting inside a
@litchar["@{"]...@litchar["}"], and they return a (syntactic) list.
Useful for implementing languages that are textual by default (see
@filepath{docreader.ss} for example).
}

@defproc[(make-at-readtable
          [#:readtable readtable readtable? (current-readtable)]
          [#:command-char command-char character? #\@]
          [#:datum-readtable datum-readtable
                             (or/c readtable? boolean?
                                              (readtable? . -> . readtable?))
                             #t]
          [#:syntax-post-processor syntax-post-proc
                                   (syntax? . -> . syntax?)
                                   values])
          readtable?]{

Constructs an @"@"-readtable.  The keyword arguments can customize the
resulting reader in several ways:

@itemize[

@item{@scheme[readtable] --- a readtable to base the @"@"-readtable
  on.}

@item{@scheme[command-char] --- the character used for @"@"-forms.}

@item{@scheme[datum-readtable] --- determines the readtable used for
  reading the datum part.  A @scheme[#t] values uses the
  @"@"-readtable, otherwise it can be a readtable, or a
  readtable-to-readtable function that will construct one from the
  @"@"-readtable.  The idea is that you may want to have completely
  different uses for the datum part, for example, introducing a
  convenient @litchar{key=val} syntax for attributes.}

@item{@scheme[syntax-post-proc] --- function that is applied on
  each resulting syntax value after it has been parsed (but before it
  is wrapped quoting punctuations).  You can use this to further
  control uses of @"@"-forms, for example, making the command be the
  head of a list:

  @schemeblock[
    (use-at-readtable
      #:syntax-post-processor
      (lambda (stx)
        (syntax-case stx ()
          [(cmd rest ...) #'(list 'cmd rest ...)]
          [_else (error "@ forms must have a body")])))
  ]}

]}

@defproc[(make-at-reader [#:syntax? syntax? #t] [#:inside? inside? #f] ...)
          procedure?]{
Constructs a variant of a @"@"-readtable.  The arguments are the same
as in @scheme[make-at-readtable], with two more that determine the
kind of reader function that will be created: @scheme[syntax?] chooses
between a @scheme[read]- or @scheme[read-syntax]-like function, and
@scheme[inside?] chooses a plain reader or an @schemeid[-inside]
variant.

Note that the resulting function has a different contract and action
based on these inputs.  The expected inputs are as in @scheme[read] or
@scheme[read-syntax] depending on @scheme[syntax?]; the function will
read a single expression or, if @scheme[inside?] is true, the whole
input; it will return a syntactic list of expressions rather than a
single one in this case.

Note also that @scheme[syntax] defaults to @scheme[#t].}

@defproc[(use-at-readtable ...) void?]{

Passes all arguments to @scheme[make-at-readtable], and installs the
resulting readtable using @scheme[current-readtable]. It also enables
line counting for the current input-port via @scheme[port-count-lines!].

This is mostly useful for playing with the Scribble syntax on the REPL.}

@; *** End reader-import section ***
))]))
@with-scribble-read[]

 
