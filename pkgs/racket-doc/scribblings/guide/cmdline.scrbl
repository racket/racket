#lang scribble/manual
@(require (only-in xrepl/doc-utils [cmd xreplcmd])
          "guide-utils.rkt")

@(define xrepl-doc '(lib "xrepl/xrepl.scrbl"))

@title[#:tag "cmdline-tools"]{Command-Line Tools}

Racket provides, as part of its standard distribution, a number of
command-line tools that can make racketeering more pleasant.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@include-section["compile.scrbl"] @; raco

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Interactive evaluation: XREPL}

The Racket distribution includes @seclink[#:doc xrepl-doc
"top"]{XREPL} (eXtended REPL), which provides everything you expect
from a modern interactive environment. For example, XREPL provides an
@xreplcmd{enter} command to have a REPL that runs in the context of a
given module, and an @xreplcmd{edit} command to invoke your editor (as
specified by the @envvar{EDITOR} environment variable) on the file you
entered. A @xreplcmd{drracket} command makes it easy to use your
favorite editor to write code, and still have DrRacket at hand to try
things out.

For more information about XREPL, see @other-doc[xrepl-doc].

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Shell completion}

Shell auto-completion for @exec{bash} and @exec{zsh} is available in
@filepath{share/pkgs/shell-completion/racket-completion.bash} and
@filepath{share/pkgs/shell-completion/racket-completion.zsh},
respectively.
To enable it, just run the appropriate file from your @tt{.bashrc} or
your @tt{.zshrc}.

The @filepath{shell-completion} collection is only available in the Racket Full
distribution. The completion scripts are also available
@hyperlink["https://github.com/racket/shell-completion"]{online}.
