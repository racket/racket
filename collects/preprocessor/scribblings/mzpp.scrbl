#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     scheme/port
                     preprocessor/mzpp))

@title[#:tag "mzpp"]{@exec{mzpp}}

@exec{mzpp} is a simple preprocessor that allows mixing Racket code with text
files in a similar way to PHP or BRL.  Processing of input files works
by translating the input file to Racket code that prints the contents,
except for marked portions that contain Racket code.  The Racket parts
of a file are marked with @litchar{<<} and @litchar{>>} tokens by default.  The
Racket code is then passed through a read-eval-print loop that is similar to a
normal REPL with a few differences in how values are printed.

@section{Invoking mzpp}

Use the @Flag{-h} flag to get the available flags.  See above for an
explanation of the @DFlag{run} flag.

@section{mzpp files}

Here is a sample file that @exec{mzpp} can process, using the default beginning
and ending markers:

@verbatim[#:indent 2]|{
  << (define bar "BAR") >>
  foo1
  foo2 << bar newline* bar >> baz
  foo3
}|

First, this file is converted to the following Racket code:

@verbatim[#:indent 2]|{
  (thunk (cd "tmp/") (current-file "foo"))
  (thunk (push-indentation ""))
   (define bar "BAR") (thunk (pop-indentation))
  newline*
  "foo1"
  newline*
  "foo2 "
  (thunk (push-indentation "     "))
   bar newline* bar (thunk (pop-indentation))
  " baz"
  newline*
  "foo3"
  newline*
  (thunk (cd "/home/eli") (current-file #f))
}|

which is then fed to the REPL, resulting in the following output:

@verbatim[#:indent 2]|{
  foo1
  foo2 BAR
       BAR baz
  foo3
}|

To see the processed input that the REPL receives, use the @DFlag{debug}
flag.  Note that the processed code contains expressions that have no
side-effects, only values---see below for an explanation of the REPL
printing behavior.  Some expressions produce values that change the REPL
environment, for example, the indentation commands are used to keep
track of the column where the Racket marker was found, and @exec{cd} is used
to switch to the directory where the file is (here it was in
@filepath["/home/foo/tmp"]) so including a relative file works.  Also, note that
the first @racket[newline*] did not generate a newline, and that the one in the
embedded Racket code added the appropriate spaces for indentation.

It is possible to temporarily switch from Racket to text-mode and back
in a way that does not respect a complete Racket expression, but you
should be aware that text is converted to a @italic{sequence} of side-effect
free expressions (not to a single string, and not expression that uses
side effects).  For example:

@verbatim[#:indent 2]|{
  << (if (zero? (random 2))
       (list >>foo1<<)
       (list >>foo2<<))
  >>
  << (if (zero? (random 2)) (list >>
  foo1
  <<) (list >>
  foo2
  <<)) >>
}|

will print two lines, each containing @litchar{foo1} or @litchar{foo} (the
first approach plays better with the smart space handling).  The @racket[show]
function can be used instead of @racket[list] with the same results, since it
will print out the values in the same way the REPL does.  The conversion
process does not transform every continuous piece of text into a single Racket
string because doing this:

@itemize[

  @item{the Racket process will need to allocating big strings which makes
    this unfeasible for big files,}

  @item{it will not play well with ``interactive'' input feeding, for example,
    piping in the output of some process will show results only on Racket
    marker boundaries,}

  @item{special treatment for newlines in these strings will become expensive.}

]

(Note that this is different from the BRL approach.)

@section{Raw preprocessing directives}

Some preprocessing directives happen at the "raw level"---the stage
where text is transformed into Racket expressions.  These directives
cannot be changed from within transformed text because they change the
way this transformation happens.  Some of these transformation

@itemize[

  @item{Skipping input:

  First, the processing can be modified by specifying a @racket[skip-to] string
  that disables any output until a certain line is seen.  This is useful
  for script files that use themselves for input.  For example, the
  following script:

@verbatim[#:indent 2]|{
    #!/bin/sh
    echo shell output
    exec mzpp -s "---TEXT-START---" "$0"
    exit 1
    ---TEXT-START---
    Some preprocessed text
    123*456*789 = << (* 123 456 789) >>
}|

  will produce this output:

@verbatim[#:indent 2]|{
    shell output
    Some preprocessed text
    123*456*789 = 44253432
}|}

  @item{Quoting the markers:

  In case you need to use the actual text of the markers, you can quote
  them.  A backslash before a beginning or an ending marker will make
  the marker treated as text, it can also quote a sequence of
  backslashes and a marker.  For example, using the default markers,
  @litchar{\<<\>>} will output @litchar{<<>>}, @litchar{\\<<\\\>>} will output @litchar{\<<\\>>} and
  @litchar{\a\b\<<} will output @litchar{\a\b<<}.}

  @item{Modifying the markers:

  Finally, if the markers collide with a certain file contents, it is
  possible to change them.  This is done by a line with a special
  structure---if the current Racket markers are @litchar{<beg1>} and
  @litchar{<end1>} then a line that contains exactly:

@verbatim[#:indent 2]|{
    <beg1><beg2><beg1><end1><end2><end1>
}|

  will change the markers to @litchar{<beg2>} and @litchar{<end2>}.  It is
  possible to change the markers from the Racket side (see below), but this
  will not change already-transformed text, which is the reason for this
  special format.}

]

@section{The mzpp read-eval-print loop}

The REPL is initialized by requiring @racket[preprocessor/mzpp], so the same
module provides both the preprocessor functionality as well as bindings for
embedded Racket code in processed files.  The REPL is then fed the transformed
Racket code that is generated from the source text (the same code that
@DFlag{debug} shows).  Each expression is evaluated and its result is printed
using the @racket[show] function (multiple values are all printed), where
@racket[show] works in the following way:

@itemize[

  @item{@|void-const| and @racket[#f] values are ignored.}

  @item{Structures of pairs are recursively scanned and their parts printed
    (no spaces are used, so to produce Racket code as output you must use
    format strings---again, this is not intended for preprocessing Racket
    code).}

  @item{Procedures are applied to zero arguments (so a procedure that doesn't
    accept zero arguments will cause an error) and the result is sent back to
    @racket[show].  This is useful for using thunks to wrap side-effects as
    values (e.g, the @racket[thunk] wraps shown by the debug output above).}

  @item{Promises are forced and the result is sent again to @racket[show].}

  @item{All other values are printed with @racket[display].  No newlines are
    used after printing values.}

]

@section[#:tag "mzpp-lib"]{Provided bindings}

@defmodule[preprocessor/mzpp]

First, bindings that are mainly useful for invoking the preprocessor:

@defproc[(preprocess [in (or/c path-string? input-port?)] ...) void?]{

  This is the main entry point to the preprocessor---invoking it on the
  given list of files and input ports.  This is quite similar to
  @racket[include], but it adds some setup of the preprocessed code environment
  (like requiring the @exec{mzpp} module).}

@defparam[skip-to str string?]{

  A string parameter---when the preprocessor is started, it ignores
  everything until a line that contains exactly this string is
  encountered.  This is primarily useful through a command-line flag for
  scripts that extract some text from their own body.}

@defboolparam[debug? on?]{

  A boolean parameter.  If true, then the REPL is not invoked, instead,
  the converted Racket code is printed as is.}

@defboolparam[no-spaces? on?]{

  A boolean parameter.  If true, then the "smart" preprocessing of
  spaces is turned off.}

@deftogether[(
@defparam[beg-mark str string?]
@defparam[end-mark str string?]
)]{

  These two parameters are used to specify the Racket beginning and end
  markers.}

All of the above are accessible in preprocessed texts, but the only one that
might make any sense to use is @racket[preprocess] and @racket[include] is a
better choice.  When @racket[include] is used, it can be wrapped with parameter
settings, which is why they are available.  Note in particular that these
parameters change the way that the text transformation works and have no effect
over the current preprocessed document (for example, the Racket marks are used
in a different thread, and @racket[skip-to] cannot be re-set when processing
has already began).  The only one that could be used is @racket[no-spaces?] but
even that makes little sense on selected parts.

The following are bindings that are used in preprocessed texts:

@deftogether[(
@defproc[(push-indentation [str string?]) void?]
@defproc[(pop-indentation) void?]
)]{

  These two calls are used to save the indentation column where the Racket
  beginning mark was found, and will be used by @racket[newline*] (unless smart
  space handling mode is disabled).}

@defproc[(show [v any/c]) void?]{

  The arguments are displayed as specified above.}

@defproc[(newline*) void?]{

  This is similar to @racket[newline] except that it tries to handle spaces in
  a ``smart'' way---it will print a newline and then spaces to reach the
  left margin of the opening @litchar{<<}.  (Actually, it tries a bit more, for
  example, it won't print the spaces if nothing is printed before
  another newline.)  Setting @racket[no-spaces?] to true disable this leaving
  it equivalent to @racket[newline].}

@defproc[(include [file path-string?] ...) void?]{

  This is the preferred way of including another file in the processing.  File
  names are searched relatively to the current preprocessed file, and during
  processing the current directory is temporarily changed to make this work.
  In addition to file names, the arguments can be input ports (the current
  directory is not changed in this case).  The files that will be incorporated
  can use any current Racket bindings etc, and will use the current
  markers---but the included files cannot change any of the parameter settings
  for the current processing (specifically, the marks and the working directory
  will be restored when the included files are processed).}

  Note that when a sequence of files are processed (through command-line
  arguments or through a single @racket[include] expression), then they are all
  taken as one textual unit---so changes to the markers, working directory etc
  in one file can modify the way sequential files are processed.  This means
  that including two files in a single @racket[include] expression can be
  different than using two expressions.

@deftogether[(
@defthing[stdin parameter?]
@defthing[stdout  parameter?]
@defthing[stderr  parameter?]
@defthing[cd  parameter?]
)]{

  These are shorter names for the corresponding port parameters and
  @racket[current-directory].}

@defparam[current-file path path-string?]{

  This is a parameter that holds the name of the currently processed
  file, or #f if none.}

@defform[(thunk expr ...)]{

  Expands to @racket[(lambda () expr ...)].}
