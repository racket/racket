#lang scribble/doc
@(require "mz.ss"
          scribble/bnf)

@title[#:tag "pretty-print"]{Pretty Printing}

@note-lib[racket/pretty]

@defproc[(pretty-print [v any/c] [port output-port? (current-output-port)])
         void?]{

Pretty-prints the value @scheme[v] using the same printed form as the
default @scheme[print] mode, but with newlines and whitespace inserted
to avoid lines longer than @scheme[(pretty-print-columns)], as
controlled by @scheme[(pretty-print-current-style-table)]. The printed
form ends in a newline, unless the @scheme[pretty-print-columns]
parameter is set to @scheme['infinity].

In addition to the parameters defined in this section,
@scheme[pretty-print] conforms to the @scheme[print-graph],
@scheme[print-struct], @scheme[print-hash-table],
@scheme[print-vector-length], @scheme[print-box], and
@scheme[print-as-expression] parameters.

The pretty printer detects structures that have the
@scheme[prop:custom-write] property and it calls the corresponding
custom-write procedure. The custom-write procedure can check the
parameter @scheme[pretty-printing] to cooperate with the
pretty-printer. Recursive printing to the port automatically uses
pretty printing, but if the structure has multiple recursively printed
sub-expressions, a custom-write procedure may need to cooperate more
to insert explicit newlines. Use @scheme[port-next-location] to
determine the current output column, use @scheme[pretty-print-columns]
to determine the target printing width, and use
@scheme[pretty-print-newline] to insert a newline (so that the
function in the @scheme[pretty-print-print-line] parameter can be
called appropriately). Use
@scheme[make-tentative-pretty-print-output-port] to obtain a port for
tentative recursive prints (e.g., to check the length of the output).}

@defproc[(pretty-write [v any/c] [port output-port? (current-output-port)])
         void?]{

Same as @scheme[pretty-print], but @scheme[v] is printed like
@scheme[write] instead of like @scheme[print].}

@defproc[(pretty-display [v any/c] [port output-port? (current-output-port)])
         void?]{

Same as @scheme[pretty-print], but @scheme[v] is printed like
@scheme[display] instead of like @scheme[print].}


@defproc[(pretty-format [v any/c] [columns exact-nonnegative-integer? (pretty-print-columns)])
         string?]{

Like @scheme[pretty-print], except that it returns a string containing
the pretty-printed value, rather than sending the output to a port.

The optional argument @scheme[columns] argument is used to
parameterize @scheme[pretty-print-columns].}


@defproc[(pretty-print-handler [v any/c]) void?]{

Pretty-prints @scheme[v] if @scheme[v] is not @|void-const|, or prints
nothing if @scheme[v] is @|void-const|. Pass this procedure to
@scheme[current-print] to install the pretty printer into the REPL run
by @scheme[read-eval-print-loop].}


@; ----------------------------------------------------------------------

@section{Basic Pretty-Print Options}

@defparam[pretty-print-columns width (or/c exact-positive-integer? 'infinity)]{

A parameter that determines the default width for pretty printing.

If the display width is @scheme['infinity], then pretty-printed output
is never broken into lines, and a newline is not added to the end of
the output.}


@defparam[pretty-print-depth depth (or/c exact-nonnegative-integer? #f)]{

Parameter that controls the default depth for recursive pretty
printing. Printing to @scheme[depth] means that elements nested more
deeply than @scheme[depth] are replaced with ``...''; in particular, a
depth of 0 indicates that only simple values are printed. A depth of
@scheme[#f] (the default) allows printing to arbitrary
depths.}


@defboolparam[pretty-print-exact-as-decimal as-decimal?]{

A parameter that determines how exact non-integers are printed.  If
the parameter's value is @scheme[#t], then an exact non-integer with a
decimal representation is printed as a decimal number instead of a
fraction. The initial value is @scheme[#f].}

@defboolparam[pretty-print-.-symbol-without-bars on?]{

A parameter that controls the printing of the symbol whose print name
is just a period. If set to a true value, then such a symbol is
printed as only the period.  If set to a false value, it is printed as
a period with vertical bars surrounding it.}


@defboolparam[pretty-print-show-inexactness show?]{

A parameter that determines how inexact numbers are printed.  If the
parameter's value is @scheme[#t], then inexact numbers are always
printed with a leading @litchar{#i}. The initial value is @scheme[#f].}

@; ----------------------------------------------------------------------

@section{Per-Symbol Special Printing}


@defboolparam[pretty-print-abbreviate-read-macros abbrev?]{

A parameter that controls whether or not @schemeidfont{quote},
@schemeidfont{unquote}, @schemeidfont{unquote-splicing}, @|etc| are
abbreviated with @litchar{'}, @litchar{,}, @litchar[",@"], etc. 
By default, the abbreviations are enabled.

See also @scheme[pretty-print-remap-stylable].
}


@defproc[(pretty-print-style-table? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a style table for use with
@scheme[pretty-print-current-style-table], @scheme[#f] otherwise.}


@defparam[pretty-print-current-style-table style-table pretty-print-style-table?]{

A parameter that holds a table of style mappings. See
@scheme[pretty-print-extend-style-table].}


@defproc[(pretty-print-extend-style-table [style-table pretty-print-style-table?]
                                          [symbol-list (listof symbol?)]
                                          [like-symbol-list (listof symbol?)])
         pretty-print-style-table?]{

Creates a new style table by extending an existing
@scheme[style-table], so that the style mapping for each symbol of
@scheme[like-symbol-list] in the original table is used for the
corresponding symbol of @scheme[symbol-list] in the new table. The
@scheme[symbol-list] and @scheme[like-symbol-list] lists must have the
same length. The @scheme[style-table] argument can be @scheme[#f], in
which case with default mappings are used for the original table (see
below).

The style mapping for a symbol controls the way that whitespace is
inserted when printing a list that starts with the symbol. In the
absence of any mapping, when a list is broken across multiple lines,
each element of the list is printed on its own line, each with the
same indentation.

The default style mapping includes mappings for the following symbols,
so that the output follows popular code-formatting rules:

@schemeblock[
'lambda 'case-lambda
'define 'define-macro 'define-syntax
'let 'letrec 'let*
'let-syntax 'letrec-syntax
'let-values 'letrec-values 'let*-values
'let-syntaxes 'letrec-syntaxes
'begin 'begin0 'do
'if 'set! 'set!-values
'unless 'when
'cond 'case 'and 'or
'module
'syntax-rules 'syntax-case 'letrec-syntaxes+values
'import 'export 'link
'require 'require-for-syntax 'require-for-template 'provide
'public 'private 'override 'rename 'inherit 'field 'init
'shared 'send 'class 'instantiate 'make-object
]}


@defparam[pretty-print-remap-stylable
          proc 
          (any/c . -> . (or/c symbol? #f))]{

A parameter that controls remapping for styles and for the determination of 
the reader shorthands.

This procedure is
called with each subexpression that appears as the first element in a
sequence. If it returns a symbol, the style table is used, as if that
symbol were at the head of the sequence. If it returns @scheme[#f],
the style table is treated normally.
Similarly, when determining whether or not to abbreviate reader macros,
the parameter is consulted.
}


@; ----------------------------------------------------------------------

@section{Line-Output Hook}

@defproc[(pretty-print-newline [port out-port?] [width exact-nonnegative-integer?]) void?]{

Calls the procedure associated with the
@scheme[pretty-print-print-line] parameter to print a newline to
@scheme[port], if @scheme[port] is the output port that is redirected
to the original output port for printing, otherwise a plain newline is
printed to @scheme[port]. The @scheme[width] argument should be the
target column width, typically obtained from
@scheme[pretty-print-columns].}


@defparam[pretty-print-print-line proc
          ((or/c exact-nonnegative-integer? #f)
           output-port?
           exact-nonnegative-integer?
           (or/c exact-nonnegative-integer? 'infinity)
           . -> .
           exact-nonnegative-integer?)]{

A parameter that determines a procedure for printing the newline
separator between lines of a pretty-printed value. The procedure is
called with four arguments: a new line number, an output port, the old
line's length, and the number of destination columns. The return value
from @scheme[proc] is the number of extra characters it printed at the
beginning of the new line.

The @scheme[proc] procedure is called before any characters are
printed with @scheme[0] as the line number and @scheme[0] as the old
line length; @scheme[proc] is called after the last character for a
value is printed with @scheme[#f] as the line number and with the
length of the last line. Whenever the pretty-printer starts a new
line, @scheme[proc] is called with the new line's number (where the
first new line is numbered @scheme[1]) and the just-finished line's
length. The destination-columns argument to @scheme[proc] is always
the total width of the destination printing area, or
@scheme['infinity] if pretty-printed values are not broken into lines.

The default @scheme[proc] procedure prints a newline whenever the line
number is not @scheme[0] and the column count is not
@scheme['infinity], always returning @scheme[0]. A custom
@scheme[proc] procedure can be used to print extra text before each
line of pretty-printed output; the number of characters printed before
each line should be returned by @scheme[proc] so that the next line
break can be chosen correctly.

The destination port supplied to @scheme[proc] is generally not the
port supplied to @scheme[pretty-print] or @scheme[pretty-display] (or
the current output port), but output to this port is ultimately
redirected to the port supplied to @scheme[pretty-print] or
@scheme[pretty-display].}


@; ----------------------------------------------------------------------

@section{Value Output Hook}


@defparam[pretty-print-size-hook proc
          (any/c boolean? output-port?
           . -> . 
           (or/c #f exact-nonnegative-integer?))]{

A parameter that determines a sizing hook for pretty-printing.

The sizing hook is applied to each value to be printed. If the hook
returns @scheme[#f], then printing is handled internally by the
pretty-printer. Otherwise, the value should be an integer specifying
the length of the printed value in characters; the print hook will be
called to actually print the value (see
@scheme[pretty-print-print-hook]).

The sizing hook receives three arguments. The first argument is the
value to print.  The second argument is a Boolean: @scheme[#t] for
printing like @scheme[display] and @scheme[#f] for printing like
@scheme[write]. The third argument is the destination port; the port
is the one supplied to @scheme[pretty-print] or
@scheme[pretty-display] (or the current output port).  The sizing hook
may be applied to a single value multiple times during
pretty-printing.}


@defparam[pretty-print-print-hook proc
          (any/c boolean? output-port? . -> . void?)]{

A parameter that determines a print hook for pretty-printing.  The
print-hook procedure is applied to a value for printing when the
sizing hook (see @scheme[pretty-print-size-hook]) returns an integer
size for the value.

The print hook receives three arguments. The first argument is the
value to print.  The second argument is a boolean: @scheme[#t] for
printing like @scheme[display] and @scheme[#f] for printing like
@scheme[write]. The third argument is the destination port; this port
is generally not the port supplied to @scheme[pretty-print] or
@scheme[pretty-display] (or the current output port), but output to
this port is ultimately redirected to the port supplied to
@scheme[pretty-print] or @scheme[pretty-display].}


@defparam[pretty-print-pre-print-hook proc
          (any/c output-port? . -> . void)]{

A parameter that determines a hook procedure to be called just before
an object is printed. The hook receives two arguments: the object and
the output port. The port is the one supplied to @scheme[pretty-print]
or @scheme[pretty-display] (or the current output port).}


@defparam[pretty-print-post-print-hook proc
          (any/c output-port? . -> . void)]{

A parameter that determines a hook procedure to be called just after
an object is printed. The hook receives two arguments: the object and
the output port. The port is the one supplied to @scheme[pretty-print]
or @scheme[pretty-display] (or the current output port).}

@; ----------------------------------------------------------------------

@section{Additional Custom-Output Support}

@defboolparam[pretty-printing on?]{

A parameter that is set to @scheme[#t] when the pretty printer calls a
custom-write procedure (see @scheme[prop:custom-write]) for output.
When pretty printer calls a custom-write procedure merely to detect
cycles, it sets this parameter to @scheme[#f].}


@defproc[(make-tentative-pretty-print-output-port 
          [out output-port?]
          [width exact-nonnegative-integer?]
          [overflow-thunk (-> any)])
         output-port?]{

Produces an output port that is suitable for recursive pretty printing
without actually producing output. Use such a port to tentatively
print when proper output depends on the size of recursive
prints. After printing, determine the size of the tentative output
using @scheme[file-position].

The @scheme[out] argument should be a pretty-printing port, such as
the one supplied to a custom-write procedure when
@scheme[pretty-printing] is set to true, or another tentative output
port. The @scheme[width] argument should be a target column width,
usually obtained from @scheme[pretty-print-columns], possibly
decremented to leave room for a terminator. The
@scheme[overflow-thunk] procedure is called if more than
@scheme[width] items are printed to the port; it can escape from the
recursive print through a continuation as a short cut, but
@scheme[overflow-thunk] can also return, in which case it is called
every time afterward that additional output is written to the port.

After tentative printing, either accept the result with
@scheme[tentative-pretty-print-port-transfer] or reject it with
@scheme[tentative-pretty-print-port-cancel]. Failure to accept or
cancel properly interferes with graph-structure printing, calls to
hook procedures, etc.  Explicitly cancel the tentative print even when
@scheme[overflow-thunk] escapes from a recursive print.}

 
@defproc[(tentative-pretty-print-port-transfer 
          [tentative-out output-port?] [orig-out output-port?])
         void?]{

Causes the data written to @scheme[tentative-out] to be transferred as
if written to @scheme[orig-out]. The @scheme[tentative-out] argument
should be a port produced by
@scheme[make-tentative-pretty-print-output-port], and
@scheme[orig-out] should be either a pretty-printing port (provided to
a custom-write procedure) or another tentative output port.}


@defproc[(tentative-pretty-print-port-cancel [tentative-out output-port?]) void?]{

Cancels the content of @scheme[tentative-out], which was produced by
@scheme[make-tentative-pretty-print-output-port]. The main effect of
canceling is that graph-reference definitions are undone, so that a
future print of a graph-referenced object includes the defining
@litchar{#}@nonterm{n}@litchar{=}.}
