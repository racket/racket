#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     readline/pread
                     readline/readline))

@(define readline "Readline")
@(define Readline "Readline")

@title{@bold{Readline}: Terminal Interaction}

The @filepath{readline} collection (not to be confused with MzScheme's
@scheme[read-line] function) provides glue for using GNU's @|readline|
library with the MzScheme @scheme[read-eval-print-loop].

@section{Normal Use of @|Readline|}

@defmodule*[(readline/rep readline/rep-start)]

The @schememodname[readline/rep] library installs a @|readline|-based
input port, and hooks the prompt-and-read part of MzScheme's
@scheme[read-eval-print-loop] to interact with it

You can start MzScheme with

@commandline{mzscheme -il readline/rep}

or you can put the following in your @filepath{~/.mzschemerc} so that
MzScheme starts with @|readline| support when appropriate:

@schemeblock[
(when (regexp-match? #rx"xterm" 
                     (getenv "TERM"))
  (dynamic-require 'readline/rep #f))
]

The @schememodname[readline/rep] module is actually a wrapper around
@schememodname[readline/rep-start]; it will @emph{not} invoke
@schememodname[readline/rep-start] if the input port is not a terminal
port (e.g., when the input is redirected from a file); see
@scheme[terminal-port?].  Still, the @envvar{TERM} condition
above is useful for starting MzScheme in dumb terminals (e.g., inside
Emacs.)

Completion is set to use the visible bindings in the current
namespace; this is far from ideal, but it's better than @|readline|'s
default filename completion which is rarely useful.  In addition, the
@|readline| history is stored across invocations in MzScheme's
preferences file, assuming that MzScheme exits normally.


@section{Interacting with the @|Readline|-Enabled Input Port }

@defmodule[readline/pread]{ The @schememodname[readline/pread] library
provides customization, and support for prompt-reading after
@schememodname[readline/rep] installs the new input port.}

The reading facility that the new input port provides can be
customized with the following parameters.


@defparam[current-prompt bstr bytes?]{

A parameter that determines the prompt that is used, as a byte string.
Defaults to @scheme[#"> "].}


@defboolparam[show-all-prompts on?]{

A parameter. If @scheme[#f], no prompt is shown until you write input
that is completely readable.  For example, when you type

@schemeblock[
(foo bar) (+ 1
             2)
]

you will see a single prompt in the beginning.

The problem is that the first expression can be @scheme[(read-line)],
which normally consumes the rest of the text on the @emph{same} line.
The default value of this parameter is therefore @scheme[#t], making
it mimic plain I/O interactions.}


@defparam[max-history n nonnegative-exact-integer?]{

A parameter that determines the number of history entries to save,
defaults to @scheme[100].}


@defboolparam[keep-duplicates keep?]{

A parameter. If @scheme[#f] (the default), then lines that are equal
to the previous one are not added as new history items.}


@defboolparam[keep-blanks keep?]{

A parameter. If @scheme[#f] (the default), blank input lines are not
kept in history.}


@defparam[readline-prompt status (or/c false/c bytes? (one-of/c 'space))]{

The new input port that you get when you require
@schememodname[readline/rep] is a custom port that uses @|readline| for
all inputs.  The problem is when you want to display a prompt and then
read some input, @|readline| will get confused if it is not used when the
cursor is at the beginning of the line (which is why it has a
@scheme[_prompt] argument.)  To use this prompt:

@schemeblock[
(parameterize ([readline-prompt some-byte-string])
  ...code-that-reads...)
]

This expression makes the first call to @|readline| use the prompt, and
subsequent calls will use an all-spaces prompt of the same length (for
example, when you're reading an S-expression).  The normal value of
@scheme[readline-prompt] is @scheme[#f] for an empty prompt (and
spaces after the prompt is used, which is why you should use
@scheme[parameterize] to restore it to @scheme[#f]).

A proper solution would be to install a custom output port, too, which
keeps track of text that is displayed without a trailing newline.  As
a cheaper solution, if line-counting is enabled for the terminal's
output-port, then a newline is printed before reading if the column is
not 0. (The @schememodname[readline/rep] library enables line-counting
for the output port.)

@bold{Warning:} The @|readline| library uses the output port directly.
You should not use it when @scheme[current-input-port] has been
modified, or when it was not a terminal port when MzScheme was started
(eg, when reading input from a pipe).  Expect some problems if you
ignore this warning (not too bad, mostly problems with detecting an
EOF).}



@section{Direct Bindings for @|Readline| Hackers}

@defmodule[readline/readline]

@defproc[(readline [prompt string?]) string?]{

Prints the given prompt string and reads a line.}


@defproc[(readline-bytes [prompt bytes?]) bytes?]{

Like @scheme[readline], but using raw byte-strings for the prompt and
returning a byte string.}


@defproc[(add-history [str string?]) void?]{

Adds the given string to the @|readline| history, which is accessible to
the user via the up-arrow key.}


@defproc[(add-history-bytes [str string?]) void?]{

Adds the given byte string to the @|readline| history, which is
accessible to the user via the up-arrow key.}


@defproc[(set-completion-function! [proc ((or/c string? bytes?)
                                          . -> . (listof (or/c string? bytes?)))]
                                   [type (one-of/c 'string 'bytes) 'string])
         void?]{

Sets @|readline|'s @tt{rl_completion_entry_function} to
@scheme[proc]. The @scheme[type] argument determines the type of value
upplied to the @scheme[proc].}


@section{License Issues}

GNU's @|readline| library is covered by the GPL, and that applies to code
that links with it.  PLT Scheme is LGPL, so this code is not used by
default; you should explicitly enable it if you want to.  Also, be
aware that if you write code that uses this library, it will make your
code link to the @|readline| library when invoked, with the usual GPL
implications.
