#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     readline
                     readline/pread
                     readline/readline
                     scheme/contract
                     (except-in scheme/foreign ->)))

@(define readline "Readline")
@(define Readline "Readline")

@title{@bold{Readline}: Terminal Interaction}

The @filepath{readline} collection (not to be confused with MzScheme's
@scheme[read-line] function) provides glue for using GNU's @|readline|
library with the MzScheme @scheme[read-eval-print-loop].

@section{Normal Use of @|Readline|}

@defmodule*[(readline readline/rep-start)]

The @schememodname[readline] library installs a @|readline|-based
input port, and hooks the prompt-and-read part of MzScheme's
@scheme[read-eval-print-loop] to interact with it

You can start MzScheme with

@commandline{mzscheme -il readline}

or evaluate

@schemeblock[
(require readline)
]

in the MzScheme @scheme[read-eval-print-loop] to load @|readline|
manually.  You can also put (require readline) in your
@filepath{~/.mzschemerc}, so that MzScheme automatically loads
@|readline| support in interactive mode.

If you want to enable @|readline| support only sometimes---such as
only when you use an @exec{xterm}, and not when you use an Emacs
shell---then you can use @scheme[dynamic-require], as in the following
example:

@schemeblock[
(when (regexp-match? #rx"xterm" 
                     (getenv "TERM"))
  (dynamic-require 'readline #f))
]

The @schememodname[readline] library automatically checks whether the
current input port is a terminal, as determined by
@scheme[terminal-port?], and it installs @|readline| only to replace
terminal ports.  The @schememodname[readline/rep-start] module
installs @|readline| without a terminal check.

By default, @|readline|'s completion is set to use the visible
bindings in the current namespace. This is far from ideal, but it's
better than @|readline|'s default filename completion which is rarely
useful.  In addition, the @|readline| history is stored across
invocations in MzScheme's preferences file, assuming that MzScheme
exits normally.

@defproc[(install-readline!) void?]{

Adds @scheme[(require readline)] to the result of
@scheme[(find-system-path 'init-file)], which is
@filepath{~/.mzschemerc} under Unix. Consequently, @|readline| will be
loaded whenever MzScheme is started in interactive mode. The
declaration is added only if it is not already present, as determined
by @scheme[read]ing and checking all top-level expressions in the
file. For more fine-grained control, such as conditionally loading
@|readline| based on an environment variable, edit
@filepath{~/.mzschemerc} manually.}


@section{Interacting with the @|Readline|-Enabled Input Port }

@defmodule[readline/pread]{ The @schememodname[readline/pread] library
provides customization, and support for prompt-reading after
@schememodname[readline] installs the new input port.}

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
@schememodname[readline] is a custom port that uses @|readline| for
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
not 0. (The @schememodname[readline] library enables line-counting
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
                                   [type (one-of/c _string _bytes) _string])
         void?]{

Sets @|readline|'s @tt{rl_completion_entry_function} to
@scheme[proc]. The @scheme[type] argument, whose possible values are
from @schememodname[scheme/foreign], determines the type of value
supplied to the @scheme[proc].}


@section{License Issues}

GNU's @|readline| library is covered by the GPL, and that applies to code
that links with it.  PLT Scheme is LGPL, so this code is not used by
default; you should explicitly enable it if you want to.  Also, be
aware that if you write code that uses this library, it will make your
code link to the @|readline| library when invoked, with the usual GPL
implications.
