#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     scheme/port
                     preprocessor/mztext))


@title[#:tag "mztext"]{@exec{mztext}}

@exec{mztext} is another Racket-based preprocessing language.  It can
be used as a preprocessor in a similar way to @exec{mzpp} since it
also uses @racketmodname[preprocessor/pp-run] functionality.  However,
@exec{mztext} uses a completely different processing principle, it is
similar to TeX rather than the simple interleaving of text and Racket
code done by @exec{mzpp}.

Text is being input from file(s), and by default copied to the
standard output.  However, there are some magic sequences that trigger
handlers that can take over this process---these handlers gain
complete control over what is being read and what is printed, and at
some point they hand control back to the main loop.  On a high-level
point of view, this is similar to ``programming'' in TeX, where macros
accept as input the current input stream.  The basic mechanism that
makes this programming is a @deftech{composite input port} which is a
prependable input port---so handlers are not limited to processing
input and printing output, they can append their output back on the
current input which will be reprocessed.

The bottom line of all this is that @exec{mztext} is can perform more
powerful preprocessing than the @exec{mzpp}, since you can define your own
language as the file is processed.

@section{Invoking mztext}

Use the @Flag{h} flag to get the available flags.  SEE above for an
explanation of the @DFlag{run} flag.

@section{mztext processing: the standard command dispatcher}

@exec{mztext} can use arbitrary magic sequences, but for convenience,
there is a default built-in dispatcher that connects Racket code with
the preprocessed text---by default, it is triggered by @litchar["@"].
When file processing encounters this marker, control is transferred to
the command dispatcher.  In its turn, the command dispatcher reads a
Racket expression (using @racket[read]), evaluates it, and decides
what to do next.  In case of a simple Racket value, it is converted to
a string and pushed back on the preprocessed input.  For example, the
following text:

@verbatim[#:indent 2]|{
  foo
  @"bar"
  @(+ 1 2)
  @"@(* 3 4)"
  @(/ (read) 3)12
}|

generates this output:

@verbatim[#:indent 2]|{
  foo
  bar
  3
  12
  4
}|

An explanation of a few lines:

@itemize[

  @item{@litchar|{@"bar"}|, @litchar|{@(+ 1 2)}|---the Racket objects
  that is read is evaluated and displayed back on the input port which
  is then printed.}

  @item{@litchar|{@"@(* 3 4)"}| --- demonstrates that the results
  are ``printed'' back on the input: the string that in this case
  contains another use of @litchar["@"] which will then get read back
  in, evaluated, and displayed.}

  @item{@litchar|{@(/ (read) 3)12}| --- demonstrates that the Racket
  code can do anything with the current input.}

]

The complete behavior of the command dispatcher follows:

@itemize[

  @item{If the marker sequence is followed by itself, then it is simply
  displayed, using the default, @litchar["@@"] outputs a @litchar["@"].}

  @item{Otherwise a Racket expression is read and evaluated, and the result is
  processed as follows:

    @itemize[

      @item{If the result consists of multiple values, each one is processed,}

      @item{If it is @|void-const| or @racket[#f], nothing is done,}

      @item{If it is a structure of pairs, this structure is processed
      recursively,}

      @item{If it is a promise, it is forced and its value is used instead,}

      @item{Strings, bytes, and paths are pushed back on the input stream,}

      @item{Symbols, numbers, and characters are converted to strings and pushed
      back on the input,}

      @item{An input port will be perpended to the input, both processed as a
      single input,}

      @item{Procedures of one or zero arity are treated in a special way---see
      below, other procedures cause an error}

      @item{All other values are ignored.}

    ]
  }

  @item{When this processing is done, and printable results have been re-added
  to the input port, control is returned to the main processing loop.}

]

A built-in convenient behavior is that if the evaluation of the Racket
expression returned a @|void-const| or @racket[#f] value (or multiple values
that are all @|void-const| or @racket[#f]), then the next newline is swallowed
using @racket[swallow-newline] (see below) if there is just white spaces before
it.

During evaluation, printed output is displayed as is, without
re-processing.  It is not hard to do that, but it is a little expensive,
so the choice is to ignore it.  (A nice thing to do is to redesign this
so each evaluation is taken as a real filter, which is done in its own
thread, so when a Racket expression is about to evaluated, it is done in
a new thread, and the current input is wired to that thread's output.
However, this is much too heavy for a "simple" preprocesser...)

So far, we get a language that is roughly the same as we get from @exec{mzpp}
(with the added benefit of reprocessing generated text, which could be
done in a better way using macros).  The special treatment of procedure
values is what allows more powerful constructs.  There are handled by
their arity (preferring a the nullary treatment over the unary one):

@itemize[

@item{A procedure of arity 0 is simply invoked, and its resulting value is
  used.  The procedure can freely use the input stream to retrieve
  arguments.  For example, here is how to define a standard C function
  header for use in a Racket extension file:

@verbatim[#:indent 2]|{
    @(define (cfunc)
       (format
        "Scheme_Object *~a(int argc, Scheme_Object *argv[])\n"
        (read-line)))
    @cfunc foo
    @cfunc bar

  ==>

    Scheme_Object * foo(int argc, Scheme_Object *argv[])
    Scheme_Object * bar(int argc, Scheme_Object *argv[])
}|

  Note how @racket[read-line] is used to retrieve an argument, and how this
  results in an extra space in the actual argument value.  Replacing
  this with @racket[read] will work slightly better, except that input will
  have to be a Racket token (in addition, this will not consume the
  final newline so the extra one in the format string should be
  removed).  The @racket[get-arg] function can be used to retrieve arguments
  more easily---by default, it will return any text enclosed by
  parenthesis, brackets, braces, or angle brackets (see below).  For
  example:

@verbatim[#:indent 2]|{
    @(define (tt)
       (format "<tt>~a</tt>" (get-arg)))
    @(define (ref)
       (format "<a href=~s>~a</a>" (get-arg) (get-arg)))
    @(define (ttref)
       (format "<a href=~s>@tt{~a}</a>" (get-arg) (get-arg)))
    @(define (reftt)
       (format "<a href=~s>~a</a>" (get-arg) (tt)))
    @ttref{racket-lang.org}{Racket}
    @reftt{racket-lang.org}{Racket}

  ==>

    <a href="racket-lang.org"><tt>Racket</tt></a>
    <a href="racket-lang.org"><tt>Racket</tt></a>
}|

  Note that in @racket[reftt] we use @racket[tt] without arguments since it will
  retrieve its own arguments.  This makes @racket[ttref]'s approach more
  natural, except that "calling" @racket[tt] through a Racket string doesn't
  seem natural.  For this there is a @racket[defcommand] command (see below)
  that can be used to define such functions without using Racket code:

@verbatim[#:indent 2]|{
    @defcommand{tt}{X}{<tt>X</tt>}
    @defcommand{ref}{url text}{<a href="url">text</a>}
    @defcommand{ttref}{url text}{<a href="url">@tt{text}</a>}
    @ttref{racket-lang.org}{Racket}

  ==>

    <a href="racket-lang.org"><tt>Racket</tt></a>
}|}

  @item{A procedure of arity 1 is invoked differently---it is applied on a
  thunk that holds the "processing continuation".  This application is
  not expected to return, instead, the procedure can decide to hand over
  control back to the main loop by using this thunk.  This is a powerful
  facility that is rarely needed, similarly to the fact that @racket[call/cc]
  is rarely needed in Racket.}

]

Remember that when procedures are used, generated output is not
reprocessed, just like evaluating other expressions.

@section[#:tag "mztext-lib"]{Provided bindings}

@defmodule[preprocessor/mztext]

Similarly to @exec{mzpp}, @racketmodname[preprocessor/mztext] contains
both the implementation as well as user-visible bindings.

Dispatching-related bindings:

@defproc*[([(command-marker) string?]
           [(command-marker [str string?]) void?])]{


  A string parameter-like procedure that can be used to set a
  different command marker string.  Defaults to @litchar["@"].  It can
  also be set to @racket[#f] which will disable the command dispatcher
  altogether.  Note that this is a procedure---it cannot be used with
  @racket[parameterize].}

@defproc*[([(dispatchers) (listof list?)]
           [(dispatchers [disps (listof list?)]) void?])]{

  A parameter-like procedure (same as @racket[command-marker]) holding a list
  of lists---each one a dispatcher regexp and a handler function.  The
  regexp should not have any parenthesized subgroups, use @racket["(?:...)"] for
  grouping.  The handler function is invoked whenever the regexp is seen
  on the input stream: it is invoked on two arguments---the matched
  string and a continuation thunk.  It is then responsible for the rest
  of the processing, usually invoking the continuation thunk to resume
  the default preprocessing.  For example:

@verbatim[#:indent 2]|{
    @(define (foo-handler str cont)
       (add-to-input (list->string 
                      (reverse (string->list (get-arg)))))
       (cont))
    @(dispatchers (cons (list "foo" foo-handler) (dispatchers)))
    foo{>Foo<oof}

  ==>

    Foo
}|

  Note that the standard command dispatcher uses the same facility, and
  it is added by default to the dispatcher list unless @racket[command-marker]
  is set to @racket[#f].}


@defproc[(make-composite-input [v any/c] ...) input-port?]{

  Creates a composite input port, initialized by the given values
  (input ports, strings, etc).  The resulting port will read data from
  each of the values in sequence, appending them together to form a
  single input port.  This is very similar to
  @racket[input-port-append], but it is extended to allow prepending
  additional values to the beginning of the port using
  @racket[add-to-input].  The @exec{mztext} executable relies on this
  functionality to be able to push text back on the input when it is
  supposed to be reprocessed, so use only such ports for the current
  input port.}

@defproc[(add-to-input [v any/c] ...) void?]{

  This should be used to ``output'' a string (or an input port) back
  on the current composite input port.  As a special case, thunks can
  be added to the input too---they will be executed when the ``read
  header'' goes past them, and their output will be added back
  instead.  This is used to plant handlers that happen when reading
  beyond a specific point (for example, this is how the directory is
  changed to the processed file to allow relative includes).  Other
  simple values are converted to strings using @racket[format], but
  this might change.}

@defparam[paren-pairs pairs (listof (list/c string? string?))]{

  This is a parameter holding a list of lists, each one holding two
  strings which are matching open/close tokens for @racket[get-arg].}

@defboolparam[get-arg-reads-word? on?]{

  A parameter that holds a boolean value defaulting to @racket[#f].  If true,
  then @racket[get-arg] will read a whole word (non-whitespace string delimited
  by whitespaces) for arguments that are not parenthesized with a pair
  in @racket[paren-pairs].}

@defproc[(get-arg) (or/c string? eof-object?)]{

  This function will retrieve a text argument surrounded by a paren
  pair specified by @racket[paren-pairs].  First, an open-pattern is
  searched, and then text is assembled making sure that open-close
  patterns are respected, until a matching close-pattern is found.
  When this scan is performed, other parens are ignored, so if the
  input stream has @litchar|{{[(}}|, the return value will be
  @racket["[("].  It is possible for both tokens to be the same, which
  will have no nesting possible.  If no open-pattern is found, the
  first non-whitespace character is used, and if that is also not
  found before the end of the input, an @racket[eof] value is
  returned.  For example (using @racket[defcommand] which uses
  @racket[get-arg]):

@verbatim[#:indent 2]|{
    @(paren-pairs (cons (list "|" "|") (paren-pairs)))
    @defcommand{verb}{X}{<tt>X</tt>}
    @verb abc
    @(get-arg-reads-word? #t)
    @verb abc
    @verb |FOO|
    @verb

  ==>

    <tt>a</tt>bc
    <tt>abc</tt>
    <tt>FOO</tt>
    verb: expecting an argument for `X'
}|

}

@defproc[(get-arg*) (or/c string? eof-object?)]{

  Similar to @racket[get-arg], except that the resulting text is first
  processed.  Since arguments are usually text strings,
  ``programming'' can be considered as lazy evaluation, which
  sometimes can be too inefficient (TeX suffers from the same
  problem).  The @racket[get-arg*] function can be used to reduce some
  inputs immediately after they have been read.}

@defproc[(swallow-newline) void?]{

  This is a simple command that simply does this:

@racketblock[
    (regexp-try-match #rx"^[ \t]*\r?\n" (stdin))
]

  The result is that a newline will be swallowed if there is only
  whitespace from the current location to the end of the line.  Note
  that as a general principle @racket[regexp-try-match] should be
  preferred over @racket[regexp-match] for @exec{mztext}'s
  preprocessing.

}


@defproc[(defcommand [name any/c][args list?][text string?]) void?]{

  This is a command that can be used to define simple template
  commands.  It should be used as a command, not from Racket code
  directly, and it should receive three arguments:

  @itemize[

    @item{The name for the new command (the contents of this argument is
    converted to a string),}

    @item{The list of arguments (the contents of this is turned to a list of
    identifiers),}

    @item{Arbitrary text, with @bold{textual} instances of the variables that
    denote places they are used.}

  ]

  For example, the sample code above:

@verbatim[#:indent 2]|{
    @defcommand{ttref}{url text}{<a href="url">@tt{text}</a>}
}|

  is translated to the following definition expression:

@racketblock[
    (define (ttref)
      (let ([url (get-arg)] [text (get-arg)])
        (list "<a href=\"" url "\">@tt{" text "}</a>")))
]

  which is then evaluated.  Note that the arguments play a role as both
  Racket identifiers and textual markers.

}


@defproc[(include [file path-string?] ...) void?]{

  This will add all of the given inputs to the composite port and run
  the preprocessor loop.  In addition to the given inputs, some thunks
  are added to the input port (see @racket[add-to-input] above) to change
  directory so relative includes work.

  If it is called with no arguments, it will use @racket[get-arg] to get an
  input filename, therefore making it possible to use this as a
  dispatcher command as well.}

@defproc[(preprocess [in (or/c path-string? input-port?)]) void?]{

  This is the main entry point to the preprocessor---creating a new
  composite port, setting internal parameters, then calling @racket[include] to
  start the preprocessing.}


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

