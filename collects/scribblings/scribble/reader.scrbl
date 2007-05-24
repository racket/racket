#reader"../docreader.ss"
@require["../manual.ss"]
@require["../bnf.ss"]
@require["utils.ss"]

@title[#:tag "reader"]{Scribble Reader}

The Scribble @|at|-reader is designed to be a convenient facility for
using free-form text in Scheme code, where ``@at'' is chosen as one of
the least-used characters in Scheme code.

You can use the reader via MzScheme's @schemefont{#reader} form:

@schemeblock[
 #, @schemefont[#<<EOS
#reader(lib "reader.ss" "scribble")@{This is free-form text!}
EOS
]
]

Note that the reader will only perform a translation from @at
forms to S-expressions. It not give you any bindings to give meaning
to the S-expression.

A PLT Scheme manual more likely starts with

@schemeblock[
 #, @schemefont{#reader(lib "docreader.ss" "scribble")}
]

which installs a reader, wraps the file content afterward into a
MzScheme module, and parses the body into a document using
@file{decode.ss}. See @secref["docreader"] for more information.

Another way to use the reader is to use the @scheme[use-at-readtable]
function to switch the current readtable to a readtable that parses
@at forms.  You can do this in a single command line:

@commandline{mzscheme -Le reader.ss scribble "(use-at-readtable)"}

In addition to @scheme[read] and @scheme[read-syntax], which are used
by @schemefont{#reader}, the @file{reader.ss} library provides the
procedures @scheme[read-inside] and @scheme[read-inside-syntax]; these
@schemeid[-inner] variants parse as if starting inside a
@litchar["@{"]...@litchar["}"], and they return a (syntactic) list.

@section{Concrete Syntax}

Informally, the concrete syntax of @|at|-commands is

@schemeblock[
 #, @BNF-seq[@litchar["@"] @nonterm{cmd}
                           @litchar{[} @kleenestar{@nonterm{datum}} @litchar{]}
                           @litchar["{"] @kleenestar{@nonterm{text-body}} @litchar["}"]]
]

where all three parts after @litchar["@"] are optional, but at least one
should be present.  (Since the reader will try to see if there is a
"{...body...}" in the input, it can be awkward to use body-less
constructs on an interactive REPL since reading an expression succeeds
only when there is a new expression available.)  In the readtable,
@litchar["@"] is set as a terminating reader macro, so if you want to
use it in Scheme code, you need to quote it as @scheme{\@} or the whole
identifier with @scheme[|ba@rs|]. Of course, @litchar["@"] is not treated 
specially in Scheme strings, character constants, etc.

Roughly, a form matching the grammar above is read as

@schemeblock[
(#, @nonterm{cmd} #, @kleenestar{@nonterm{datum}} #, @kleenestar{@nonterm{parsed-body}})
]

where @nonterm{parsed-body} is the translation of each
@nonterm{text-body} in the input.

Thus, the initial @nonterm{cmd} determines the Scheme code that
the input is translated into.  The common case is when @nonterm{cmd} is a
Scheme identifier, which generates a plain Scheme form.

A @nonterm{text-body} is either a sequence of characters without
@litchar["@"] or newlines, a newline by itself, or the translation of a
@at form. Note that the syntax for @at forms is the same in a
@nonterm{text-body} context as in a Scheme context. A
@nonterm{text-body} that isn't a @at form is converted to a string for
its @nonterm{parsed-body}:

@scribble-examples[
#<<EOS
@foo{bar baz
     blah}
EOS

#f

#<<EOS
@foo{bar @baz[3]
     blah}
EOS

#f

#<<EOS
@foo{bar @baz{3}
     blah}
EOS

#f

#<<EOS
@foo{bar @baz[2 3]{4 5}
     blah}
EOS

]

When the above @at forms appear in a Scheme expression context,
the surrounding context must provide a binding for @scheme[foo]
(either as a procedure or macro). To just see the read result for a
@at form, you can always use Scheme's @scheme[quote]:

@scribble-examples[(list @litchar["'@foo{bar}"] @scheme['(foo "bar")])]

@; - - -  - - -  - - -  - - -  - - -  - - -  - - -  - - - 
@subsection{The Command Part}

Besides being a Scheme identifier, the @nonterm{cmd} part of an @at
form can have Scheme punctuation prefixes, which will end up wrapping
the @italic{whole} expression.

@scribble-examples[
 "@`',@foo{blah}"
]

When writing Scheme code, this means that @litchar["@`',@foo{blah}"]
is exactly the same as @litchar["`@',@foo{blah}"] and
@litchar["`',@@foo{blah}"], but unlike the latter two, the first
construct can appear in body texts with the same meaning, whereas the
other two would not work (see below).

Even after Scheme punctuation, the @nonterm{cmd} itself is not limited
to a Scheme identifier; it can be any Scheme expression.

@scribble-examples[
  "@(lambda (x) x){blah}"
]

In addition, the command can be omitted altogether, which will omit it
from the translation, resulting in an S-expression that usually
contains, say, just strings:

@scribble-examples[
#<<EOS
  @{foo bar
    baz}
EOS

#f

#<<EOS
  @'{foo bar
     baz}
EOS
]

If the command part begins with a @litchar{;} (with no newline between
the @litchar["@"] and the @litchar{;}), then the construct is a
comment.  There are two comment forms, one for arbitrary-text and
possibly nested comments, and another one for line comments:

@schemeblock[
#, @BNF-seq[@litchar["@;"] @kleenestar{@nonterm{whitespace}} @litchar["{"] @kleenestar{@nonterm{any}} @litchar["@"]]

#, @BNF-seq[@litchar["@;"] @kleenestar{@nonterm{anythign-else-without-newline}}]
]

In the first form, the commented body must still parse correctly; see
the description of the body syntax below.

Tip: if you're editing in some Scheme-mode, it is useful to comment out
blocks like this:

@verbatim[#<<EOS
  @;
  {
    ...
  }
EOS
]

or

@verbatim[#<<EOS
  @;{
    ...
  ;}
EOS
]

otherwise you will probably confuse the editor into treating the file as
having imbalanced parenthesis.

If only the @nonterm{cmd} part is specified of an @at form, then the
result is the command part only, without an extra set of parenthesis.
This makes it suitable for Scheme escapes in body texts.  More below,
in the description of the body part.

Finally, note that there are currently no special rules for using
@litchar["@"] in the command itself, which can lead to things like:

@scribble-examples[
 "@@foo{bar}{baz}"
]

You should not rely on such behavior, since @litchar["@@"] might be used
differently in the future (e.g., making @litchar["@@"] be ``@at'' in a
body text).

@subsection{The Datum Part}

The datum part can contains arbitrary Scheme expressions, which
are simply stacked before the body text arguments:

@scribble-examples[
  "@foo[1 (* 2 3)]{bar}"
  "@foo[@bar{...}]{blah}"
]

@italic{This following is going to be removed, I think...}

But there is one change that makes it easy to use for keyword/values:
@litchar{=} is a terminating character in the textual scope, and it if
there is a @BNF-seq[@nonterm{identifier} @litchar{=} @nonterm{expr}]
sequence (spaces optional), then it is converted to
@schemefont{#:}@nonterm{identifier} @nonterm{expr}.

@scribble-examples[
  "@foo[(* 2 3) a=b]{bar}"
]

@subsection{The Body Part}

The syntax of the body part is intended to be as convenient as
possible for writing free text.  It can contain almost any text---the
only character with special meaning is @litchar["@"]. In addition,
@litchar["{"], @litchar["}"], @litchar["|"], and @litchar["\\"] can
have special meanings, but only in a few contexts.  As described
above, the text turns to a sequence of string arguments for the
resulting form.  Spaces at the beginning of lines are discarded (but
see the information about indentation below), and newlines turn to
individual @scheme["\n"] strings.  (Spcaces are preserved on a
single-line text.)  As part of trying to do the ``right thing,'' an
empty line at the beginning and at the end are discarded, so:

@scribble-examples[
#<<EOS
  @foo{
    bar
  }
EOS

#f

 "@foo{bar}"
 "@foo{ bar }"
]

If @litchar["@"] appears in a body, then it is interpreted as Scheme
code, which means that the @|at|-reader will be applied recursively,
and the resulting syntax will appear as an argument, among other
string contents.

@scribble-examples[
  "@foo{a @bar{b} c}"
]

If the nested @at construct has only a command---no body part---then
it does not appear in a subform.  Given that the command part can be
any Scheme expression, this makes @at a general escape to arbitrary
Scheme code.

@scribble-examples[
 "@foo{a @bar c}"
 "@foo{a @(bar 2) c}"
]

In some cases, you may want to use a Scheme identifier (or a number or
a boolean) in a position that touches other text that can make an
identifier; in these situations you should surround the Scheme
identifier (or number or boolean) by a pair of @litchar["|"].  The
text inside the bars is parsed as a Scheme expression, but if that
fails, it is used as a quoted identifier; do not rely on this
behavior, and avoid using whitespace inside the bars.  Also, if bars
are used, then no body text is used even if they are followed by
braces (see the next paragraph).

@scribble-examples[
  "@foo{foo @bar foo}"
  "@foo{foo@bar.}"
  "@foo{foo@|bar|.}"
  "@foo{foo@3.}"
  "@foo{foo@|3|.}"
  "@foo{foo@|(f 1)|{bar}.}"
]

Braces are only problematic because a @litchar["}"] is used to mark
the end of the text.  They are therefore allowed, as long as they are
balanced.

@scribble-examples[
  "@foo{f{o}o}"]
]

There is also an alternative syntax for the body, one that specifies a
new marker for the end: use @litchar["|{"] for the openning marker,
optionally with additional characters between them (excluding
@litchar["{"], whitespace, and alphanumerics); the matching closing
marker should be the mirrored form of the openning marker (reverse the
characters and swap round, square, curly, and angle parentheses).

@scribble-examples[
  "@foo|{...}|"
  "@foo|{foo{{{bar}|"
  "@foo|<{{foo{{{bar}}>|"
]

More simply, if you get into too much trouble with special characters
in a body, then it's often a good idea to use the Scheme part,
instead.

@scribble-examples[
  "@foo[\"}\"]"
  "@foo[\"@literally{}\"]"
]

@; - - -  - - -  - - -  - - -  - - -  - - -  - - -  - - - 
@subsubsub*section{Quoting in Body Texts}

To quote braces or @at, precede them with a backslash.  Note that this
is an irregular use of backslash quoting!  To use @litchar["\\@"] in
your text, simply precede it with a backslash.  The general rule is
that to use N backslashes-and-a-special-character, you should precede
it with one extra backslash.  Any other use of a backslash (one that
is not followed by more back-slashes and a special character) is
preserved in the text as usual.

@scribble-examples[
  "@foo{b\\@ar}"
  "@foo{b\\\\@ar}"
  "@foo{b\\\\\\@ar}"
  "@foo{b\\{\\@\\@ar}"
  "@foo{b\\ar}"
  "@foo{b\\\\ar}"
]

@; - - -  - - -  - - -  - - -  - - -  - - -  - - -  - - - 
@subsubsub*section{Newlines and Indentation}

When indentation is used, all-space indentation string syntaxes are
perpended to the beginning of each line.  The rule for adding these
string is:

@itemize{

 @item{A spaces-string is added to each line according to its distance from
  the leftmost syntax object;}

 @item{The first string is not prepended with indentation if it appears on
  the first line of output.}

}

@scribble-examples[
#<<EOS
@foo{
  bar
    baz
  bbb}
EOS

#f

#<<EOS
@foo{bar
       baz
     bbb}
EOS

#f

#<<EOS
@foo{ bar
        baz
      bbb}
EOS

#f

#<<EOS
@foo{bar
   baz
   bbb}
EOS

#f

#<<EOS
  @foo{ bar
     baz
     bbb}
EOS

#f

#<<EOS
@foo{ bar
   baz
     bbb}
EOS
]

Additional notes:

@itemize{

 @item{You can identify indentation strings at the syntax level by the fact
  that they have the same location information as the following syntax
  object.}

 @item{This mechanism depends on line and column number information
  (@scheme[use-at-readtable] turns them on for the current input port);}

 @item{To use this mechanism with nested commands that should preserve
  indentation, you will need to do some additional work since the nested
  use will have only its own indentation;}

 @item{When using it on a command-line, you note that the reader is not aware
  of the ``> '' prompt, which might lead to confusing results.}

}

@italic{The following is likely to change.}

For situations where spaces at the beginning of lines matter (various
verbatim environments), you should begin a line with a @litchar["|"].
It has no other special meaning -- so to use a @litchar["|"] as the
first character in the text, simply use another before it.

@scribble-examples[
#<<EOS
@code{
  |(define (foo x)
  |  |error|)
}
EOS
]

In other situations, newlines matter; you might want to avoid a
newline token in some place.  To avoid a newline and still break the
source line, use a line comment.  As in TeX, these will consume text
up-to and including the end of the line and all following whitespace.

@bold{@italic{The following examples from the original docs didn't
work. They have been changed!}}

@scribble-examples[
#<<EOS
@foo{bar @;
     baz@;
     !}
EOS
] @bold{The "!" above used to be a "."}

A @litchar["|"] that follows this is still used for marking the
beginning of the text:

@scribble-examples[
#<<EOS
@foo{bar @;
     baz@;
     ? .}
EOS
]  @bold{The "?" above used to be a "|", which is surely part of the point.}


@; ------------------------------------------------------------------------
@subsection{How To Use the Reader}

The reader can be used in any way you want.  All you need is to use
function names that you bind.  You can even use quasi-quotes, skipping
the need for functions, for example:

@verbatim[
#<<EOS
  > (define (important . text) @`b{@u{@big{@,@text}}})
  > (important @`p{This is an important announcement!
                   Read it!})
  (b (u (big (p "This is an important announcement!" "\n" "Read it!"))))
EOS
]
