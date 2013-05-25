#lang scribble/manual
@(require "doc-utils.rkt"
          scribble/decode (only-in scribble/core)
          (for-label racket readline racket/help racket/enter
                     racket/trace profile
                     macro-debugger/stepper-text
                     macro-debugger/analysis/check-requires))

@title{XREPL: eXtended REPL}
@author[@author+email["Eli Barzilay" "eli@barzilay.org"]]

@defmodule[xrepl]{
  Loading the @racketmodname[xrepl] library enables XREPL, which extends
  the @exec{racket} @tech[#:doc GUIDE]{REPL} significantly, turning it
  into a more useful tool for interactive exploration and development.
  Additions include ``meta commands,'' using readline, keeping past
  evaluation results, and more.}

@; ---------------------------------------------------------------------
@section{Installing XREPL}

To use XREPL, start @exec{racket} and enter @racket[(require xrepl)].
You will know that it works when the prompt changes to a @litchar{->},
and, if you're working on a capable terminal, you will now have readline
editing.  You can also start @exec{racket} and ask for XREPL to be
loaded using command-line arguments:
@commandline{racket -il xrepl}

If you want to enable XREPL automatically, add this expression to your
Racket initialization file.
@margin-note*{To load XREPL conditionally (e.g., not in older Racket
  versions), you can use @racket[(dynamic-require 'xrepl #f)].  This
  is a plain expression that can be placed inside @racket[when] and
  elsewhere.}
An easy way to do the necessary editing is to enter @cmd[install!],
which will inspect and edit your initialization file (it will describe
the change and ask for your permission).  Alternatively, you can edit
the file directly: on Unix, it is @filepath{~/.racketrc}, and for
other platforms evaluate @racket[(find-system-path 'init-file)] to see
where it is.

XREPL will set up a readline-based reader, so you do not need to load
that yourself.  If your initialization file was previously set to load
readline via @racket[install-readline!], the @cmd[install!] command
will (notify you and) remove it.  If you added it yourself, consider
removing it.  (This is not strictly needed, but XREPL is slightly
better at detecting when to use readline.)

@; ---------------------------------------------------------------------
@section{Meta REPL Commands}

Most of the XREPL extensions are implemented as meta commands.  These
commands are entered at the REPL, prefixed by a @litchar{,} and followed
by the command name.  Note that several commands correspond directly to
Racket functions (e.g., @cmd[exit]) --- but since they work outside of
your REPL, they can be used even if the matching bindings are not
available.

@; ---------------------------------
@subsection{Generic Commands}

@defcmd[help]{
  Without an argument, displays a list of all known commands.  Specify a
  command to get help specific to that command.
}

@defcmd[exit]{
  Exits Racket, optionally with an error code (see @racket[exit]).
}

@defcmd[cd]{
  Sets the @racket[current-directory] to the given path.  If no path is
  specified, use your home directory.  Path arguments are passed through
  @racket[expand-user-path] so you can use @litchar{~}.  An argument of
  @litchar{-} means ``the previous path''.
}

@defcmd[pwd]{
  Reports the value of @racket[current-directory].
}

@defcmd[shell]{
  Use @cmd[shell] (or @cmd[sh]) to run a generic shell command (via
  @racket[system]).  For convenience, a few synonyms are provided ---
  they run the specified executables (still using @racket[system]).

  When the REPL is in the context of a module with a known source file,
  the shell command can use the @envvar{F} environment variable as the
  path to the file.  Otherwise, @envvar{F} is set to an empty string.
}

@defcmd[edit]{
  Runs an editor, as specified by your @envvar{EDITOR} environment
  variable, with the given file/s arguments.  If no files are specified
  and the REPL is currently inside a module's namespace, then the file
  for that module is used.  If the @envvar{EDITOR} environment variable
  is not set, use the @cmd[drracket] command instead.
}

@defcmd[drracket]{
  Runs DrRacket with the specified file/s.  If no files are given, and
  the REPL is currently inside a module, the file for that module is
  used.

  DrRacket is launched directly, without starting a new subprocess, and
  it is then kept running in a hidden window so further invocations are
  immediate.  (When this command is used for the first time, you will
  see DrRacket start as usual, and then its window will disappear ---
  that window is keeping DrRacket ready for quick editing.)

  In addition to file arguments, arguments can specify one of a few
  flags for additional operations:
  @itemize[
  @item{@litchar{-new}: opens a new editing window.  This is the default
    when no files are given and the REPL is not inside a module,}
  @item{@litchar{-open}: opens the specified file/s (or the current
    module's file).  This is the default when files are given or when
    inside a module.}
  @item{@litchar{-quit}: exits the running DrRacket instance.  Quitting
    DrRacket is usually not necessary.  Therefore, if you try to quit it
    from the DrRacket window, it will instead just close the window but
    DrRacket will still be running in the background.  Use this command
    in case there is some exceptional problem that requires actually
    quitting the IDE.  (Once you do so, future uses of this command will
    start a fresh instance.)}]
}

@; ---------------------------------
@subsection{Binding Information}

@defcmd[apropos]{
  Searches for known bindings in the current namespace.  The arguments
  specify which binding to look for: use a symbol (without a
  @litchar{'}) to look for bindings that contain that name, and use a
  regexp (e.g., @racket[#rx"..."]) to use a regexp for the search.
  Multiple arguments are and-ed together.

  If no arguments are given, @emph{all} bindings are listed.
}

@defcmd[describe]{
  For each of the specified names, describe where where it is coming
  from and how it was defined if it names a known binding.  In addition,
  describe the module (list its imports and exports) that is named by
  arguments that are known module names.

  By default, bindings are searched for at the runtime level (phase 0).
  You can add a different phase level for identifier lookups as a first
  argument.  In this case, only a binding can be described, even if the
  same name is a known module.
}

@defcmd[doc]{
  Uses Racket's @racket[help] to browse the documentation, look for a
  binding, etc.  Note that this can be used even in languages that don't
  have the @racket[help] binding.
}

@; ---------------------------------
@subsection{Requiring and Loading Files}

@defcmd[require]{
  Most arguments are passed to @racket[require] as is.  As a
  convenience, if a symbolic argument specifies an existing file name,
  then use its string form to specify the require, or use a
  @racket[file] in case of an absolute path.  In addition, an argument
  that names a known symbolic module name (e.g., one that was defined on
  the REPL, or a builtin module like @racket[#%network]), then its
  quoted form is used.  (Note that these shorthands do not work inside
  require subforms like @racket[only-in].)
}

@defcmd[require-reloadable]{
  Same as @cmd[require], but arranges to load the code in a way that
  makes it possible to reload it later, or if a module was already
  loaded (using this command) then reload it.  Note that the arguments
  should be simple module names, without any require macros.  If no
  arguments are given, use arguments from the last use of this command
  (if any).

  Module reloading is enabled by turning off the
  @racket[compile-enforce-module-constants] parameter --- note that this
  prohibits some optimizations, since the compiler assumes that all
  bindings may change.
}

@defcmd[enter]{
  Uses @racket[enter!] to have the REPL go ``inside'' a given module's
  namespace.  A module name can specify an existing file as with the
  @cmd[require-reloadable] command.  If no module is given, and the REPL
  is already in some module's namespace, then `enter!' is used with that
  module, causing it to reload if needed.  Using @racket[#f] makes it go
  back to the toplevel namespace.

  Note that this can be used even in languages that don't have the
  @racket[enter!] binding.  In addition, @racket[enter!] is used in a
  way that does not make it require itself into the target namespace.
}

@defcmd[toplevel]{
  Makes the REPL go back to the toplevel namespace.  Same as using the
  @cmd[enter] command with a @racket[#f] argument.
}

@defcmd[load]{
  Uses @racket[load] to load the specified file(s).
}

@; ---------------------------------
@subsection{Debugging}

@defcmd[backtrace]{
  Whenever an error is displayed, XREPL will not show its context
  printout.  Instead, use the @cmd[backtrace] command to display the
  backtrace for the last error.
}

@defcmd[time]{
  Times execution of an expression (or expressions).  This is similar to
  @racket{time} but the information that is displayed is a bit easier to
  read.

  In addition, you can provide an initial number to specify repeating
  the evaluation a number of times.  In this case, each iteration is
  preceded by two garbage collections, and when the iteration is done
  its timing information and evaluation result(s) are displayed.  When
  the requested number of repetitions is done, some extreme results are
  removed (top and bottom 2/7ths), and the remaining results are be
  averaged.  Finally, the resulting value(s) are from the last run are
  returned (and can be accessed via the bindings for the last few
  results, see @secref["past-vals"]).
}

@defcmd[trace]{
  Traces the named function (or functions), using @racket[trace].
}

@defcmd[untrace]{
  Untraces the named function (or functions), using @racket[untrace].
}

@defcmd[errortrace]{
  @racketmodname[errortrace] is a useful Racket library which can
  provide a number of useful services like precise profiling, test
  coverage, and accurate error information.  However, using it can be a
  little tricky.  @cmd[errortrace] and a few related commands fill this
  gap, making @racketmodname[errortrace] easier to use.

  @cmd[errortrace] controls global use of @racketmodname[errortrace].
  With a flag argument of @litchar{+} errortrace instrumentation is
  turned on, with @litchar{-} it is turned off, and with no arguments it
  is toggled.  In addition, a @litchar{?} flag displays instrumentation
  state.

  Remember that @racketmodname[errortrace] instrumentation hooks into
  the Racket compiler, and applies only to source code that gets loaded
  from source and therefore compiled.  Therefore, you should use it
  @emph{before} loading the code that you want to instrument.
}

@defcmd[profile]{
  This command can perform profiling of code in one of two very
  different ways: either statistical profiling via the
  @racketmodname[profile] library, or using the exact profiler feature
  of @racketmodname[errortrace].

  When given a parenthesized expression, @cmd[profile] will run it via
  the statistical profiler, as with the @racket[profile] form, reporting
  results as usual.  This profiler adds almost no overhead, and it
  requires no special setup.  In particular, it does not require
  pre-compiling code in a special way.  However, there are some
  imprecise elements to this profiling: the profiler samples stack
  snapshots periodically which can miss certain calls, and it is also
  sensitive to some compiler optimizations like inlining procedures and
  thereby not showing them in the displayed analysis.  See
  @other-doc['(lib "profile/scribblings/profile.scrbl")] for more
  information.

  In the second mode of operation, @cmd[profile] uses the precise
  @racketmodname[errortrace] profiler.  This profiler produces precise
  results, but like other uses of the @racketmodname[errortrace], it
  must be enabled before loading the code that is to be profiled.  It
  can add noticeable overhead (potentially affecting the reported
  runtimes), but the results are accurate in the sense that no procedure
  is skipped.  (For additional details, see
  @other-doc['(lib "errortrace/scribblings/errortrace.scrbl")].)

  In this mode, the arguments are flags that control the profiler.  A
  @litchar{+} flag turns the profiler on --- and as usual with
  @racketmodname[errortrace] functionality, this applies to code that is
  compiled from now on.  A @litchar{-} flag turns this instrumentation
  off, and without any flags it is toggled.  Once the profiler is
  enabled, you can run some code and then use this command to report
  profiling results: use @litchar{*} to show profiling results by time,
  and @litchar{#} for the results by counts.  Once you've seen the
  results, you can evaluate additional code to collect more profiling
  information, or you can reset the results with a @litchar{!} flag.
  You can also combine several flags to perform the associated
  operations, for example, @cmd[prof]{*!-} will show the accumulated
  results, clear them, and turn profiler instrumentation off.

  Note that using @emph{any} of these flags turns errortrace
  instrumentation on, even @cmd[prof]{-} (or no flags).  Use the
  @cmd[errortrace] command to turn off instrumentation completely.
}

@defcmd[execution-counts]{
  This command makes it easy to use the execution counts functionality
  of @racketmodname[errortrace].  Given a file name (or names),
  @cmd[execution-counts] will enable errortrace instrumentation for
  coverage, require the file(s), display the results, disables coverage,
  and disables instrumentation (if it wasn't previously turned on).
  This is useful as an indication of how well the test coverage is for
  some file.
}

@defcmd[coverage]{
  Runs a given file and displays coverage information for the run.  This
  is somewhat similar to the @cmd[execution-counts] command, but instead
  of using @racketmodname[errortrace] directly, it runs the file in a
  (trusted) sandbox, using the @racketmodname[racket/sandbox] library
  and its ability to provide coverage information.
}

@; ---------------------------------
@subsection{Miscellaneous Commands}

@defcmd[switch-namespace]{
  This powerful command controls the REPL's namespace.  While
  @cmd[enter] can be used to make the REPL go into the namespace of a
  specific module, the @cmd[switch-namespace] command can switch between
  @emph{toplevel namespaces}, allowing you to get multiple separate
  ``workspaces''.

  Namespaces are given names that are symbols or integers, where
  @litchar{*} is the name for the first initial namespace, serving as
  the default one.  These names are not bindings --- they are only used
  to label the known namespaces.

  The most basic usage for this command is to simply specify a new name.
  A namespace that corresponds to that name will be created and the REPL
  will switch to that namespace.  The prompt will now indicate this
  namespace's name.  The name is usually insignificant, except when it
  is a @racket[require]-able module: in this case, the new namespace is
  initialized to use that module's bindings.  For example,
  @cmd[switch]{racket/base} creates a new namespace that is called
  @litchar{racket/base} and initializes it with
  @racketmodname[racket/base].  For all other names, the new namespace
  is initialized the same as the current one.

  Additional @cmd[switch] uses:
  @itemize[
  @item{@cmd[switch]{!} --- reset the current namespace, recreating it
    using the same initial library.  Note that it is forbidden to reset
    the default initial namespace, the one named @litchar{*} --- this
    namespace corresponds to the one that Racket was started with, and
    where XREPL was initialized.  There is no technical reason for
    forbidding this, but doing so is not useful as no resources will
    actually be freed.}
  @item{@cmd[switch]{! <module>} --- resets the current namespace with
    the explicitly given simple module spec.}
  @item{@cmd[switch]{<name> !} --- switch to a newly made namespace.  If
    a namespace by that name already existed, it is rest.}
  @item{@cmd[switch]{<name> ! <module>} --- same, but reset to the given
    module instead of what it previously used.}
  @item{@cmd[switch]{- <name>} --- drop the specified namespace, making
    it possible to garbage-collect away any associated resources.  You
    cannot drop the current namespace or the default one (@litchar{*}).}
  @item{@cmd[switch]{?} --- list all known namespaces.}]

  Do not confuse namespaces with sandboxes or custodians.  The
  @cmd{switch} command changes @emph{only} the
  @racket[current-namespace] --- it does not install a new custodian or
  restricts evaluation in any way.  Note that it is possible to pass
  around values from one namespace to another via past result reference;
  see @secref["past-vals"].
}

@defcmd[syntax]{
  Manipulate syntaxes and inspect their expansion.

  Useful operations revolve around a ``currently set syntax''.  With no
  arguments, the currently set syntax is displayed; an argument of
  @litchar{^} sets the current syntax from the last input to the REPL;
  and an argument that holds any other s-expression will set it as the
  current syntax.

  Syntax operations are specified via flags:
  @itemize[
  @item{@litchar{+} uses @racket[expand-once] on the current syntax and
    prints the resulting syntax.  In addition, the result becomes the
    new ``current'' syntax, so you can use this as a poor-man's syntax
    stepper.  (Note that in some rare cases expansion via a sequence of
    @racket[expand-once] might differ from the actual expansion.)}
  @item{@litchar{!} uses @racket[expand] to completely expand the
    current syntax.}
  @item{@litchar{*} uses the macro debugger's textual output to show
    expansion steps for the current syntax, leaving macros from
    @racketmodname[racket/base] intact.  Does not change the current
    syntax.  Uses @racket[expand/step-text], see @other-doc['(lib
    "macro-debugger/macro-debugger.scrbl")] for details.}
  @item{@litchar{**} uses the macro debugger similarly to @litchar{*},
    but expands @racketmodname[racket/base] macros too, showing the
    resulting full expansion process.}]
  Several input flags and/or syntaxes can be specified in succession as
  arguments to @cmd{syntax}.  For example, @cmd[stx]{(when 1 2) ** !}.
}

@defcmd[check-requires]{
  Uses @racket[show-requires] to analyze the @racket[require]s of the
  specified module, defaulting to the currently entered module if we're
  in one.  See @other-doc['(lib "macro-debugger/macro-debugger.scrbl")]
  for details.
}

@defcmd[log]{
  Starts (or stops) logging events at a specific level.  The level can
  be:
  @itemize[
  @item{a known level name (currently one of @litchar{fatal},
    @litchar{error}, @litchar{warning}, @litchar{info},
    @litchar{debug}),}
  @item{@racket[#f] for no logging,}
  @item{@racket[#t] for maximum logging,}
  @item{an integer level specification, with @racket[0] for no logging
    and bigger ones for additional verbosity.}]
}

@defcmd[install!]{
  Convenient utility command to install XREPL in your Racket
  initialization file.  This is done carefully, you will be notified of
  potential issues, and asked to authorize changes.
}

@; ---------------------------------------------------------------------
@section[#:tag "past-vals"]{Past Evaluation Results}

XREPL makes the last few interaction results available for evaluation
via special toplevel variables: @racketidfont{^}, @racketidfont{^^},
..., @racketidfont{^^^^^}.  The first, @racketidfont{^}, refers to the
last result, @racketidfont{^^} to the previous one and so on.

As with the usual REPL printouts, @void-const results are not kept.  In
case of multiple results, they are spliced in reverse, so
@racketidfont{^} refers to the last result of the last evaluation.  For
example:
@verbatim[#:indent 4]{
    -> 1
    1
    -> (values 2 3)
    2
    3
    -> (values 4)
    4
    -> (list ^ ^^ ^^^ ^^^^)
    '(4 3 2 1)}
The rationale for this is that @racketidfont{^} always refers to the
last @emph{printed} result, @racketidfont{^^} to the one before that,
etc.

In addition to these names, XREPL also binds @racketidfont{$1},
@racketidfont{$2}, ..., @racketidfont{$5} to the same references, so you
can choose the style that you like.  All of these bindings are made
available only if they are not already defined.  This means that if you
have code that uses these names, it will continue to work as usual (and
it will shadow the saved value binding).

The bindings are identifier macros that expand to the literal saved
values; so referring to a saved value that is missing (because not
enough values were shown) raises a syntax error.  In addition, the
values are held in a @tech[#:doc REFERENCE]{weak reference}, so they can disappear after
a garbage-collection.

Note that this facility can be used to ``transfer'' values from one
namespace to another---but beware of struct values that might come from
a different instantiation of a module.

@; ---------------------------------------------------------------------
@section{Hacking XREPL}

XREPL is mainly a convenience tool, and as such you might want to hack
it to better suit your needs.  Currently, there is no convenient way to
customize and extend it, but this will be added in the future.

Meanwhile, if you're interested in tweaking XREPL, the @cmd[enter]
command can be used as usual to go into its implementation.  For
example --- change an XREPL parameter:
@verbatim[#:indent 4]{
    -> ,en xrepl/xrepl
    xrepl/xrepl> ,e
    xrepl/xrepl> (saved-values-char #\~)
    xrepl/xrepl> ,top
    -> 123
    123
    -> ~
    123}
or add a command:
@verbatim[#:indent 4]{
  -> ,en xrepl/xrepl
  xrepl/xrepl> (defcommand eli "stuff" "eli says" ["Make eli say stuff"]
                 (printf "Eli says: ~a\n" (getarg 'line)))
  xrepl/xrepl> ,top
  -> ,eli moo
  Eli says: moo}
While this is not intended as @emph{the} way to extend and customize
XREPL, it is a useful debugging tool should you want to do so.

If you have any useful tweaks and extensions, please mail the author or
the Racket developer's
@hyperlink["http://racket-lang.org/community.html"]{mailing list}.

@; ---------------------------------------------------------------------
@section{License Issues}

Under most circumstances XREPL uses the @racketmodname[readline]
library, and therefore a similar license caveat applies: XREPL cannot be
enabled by default because of the @seclink["readline-license" #:doc
RL]{readline licensing}, you have to explicitly do so yourself to use
it.  (Note that XREPL is intended to be used only for enhanced
interaction, not as a library; so there are no additional issues.)

@; ---------------------------------------------------------------------
@(check-all-documented)
