#lang scribble/manual

@title[#:tag "other-editors"]{Using Racket with Your Editor of Choice}
@; author["Vincent St-Amour" "Asumu Takikawa" "Jon Rafkind"]

Do you want to program with Racket but would rather use your editor of choice
instead of DrRacket? Then this page is for you.

Included is a brief summary of tools that will improve your Racketeering
experience.

@section{Command-line Tools}
Racket provides, as part of its standard distribution, a number of useful
command-line tools that can make Racketeering more pleasant.

@subsection{Raco}
The @exec{raco} utility provides access to Racket's build tools, package
management utilities, documentation search and much more. For more information,
see @other-doc['(lib "scribblings/raco/raco.scrbl")].

@subsection{XREPL}
Racket ships with an eXtended REPL which provides everything you expect from a
modern interactive environment. For instance, XREPL provides an @tt{,enter}
command to have a REPL that runs in the context of a given module, and an
@tt{,edit} command to invoke your @tt{$EDITOR} on the file you entered. A
@tt{,drracket} command is also provided to make it easy to use your favorite
editor to write code, and still have DrRacket easily available to try things
out.

For more information, see @racketmodname[xrepl].

@subsection{Bash completion}
Shell auto-completion for @exec{bash} is available in
@tt{collects/meta/contrib/completion/racket-completion.bash}.
To enable it, just run the file from your @tt{.bashrc}.

Note: the @tt{meta} collection is only available in the Racket Full
distribution. The completion script is also available
@hyperlink["https://raw.github.com/plt/racket/master/collects/meta/contrib/completion/racket-completion.bash"]{online}.


@section{Emacs}
Emacs has long been a favorite among Lispers and Schemers, and is popular among
Racketeers as well.

@subsection{Major Modes}

@subsubsection{Quack}
@hyperlink["http://www.neilvandyke.org/quack/"]{Quack} is an extension of
Emacs's @tt{scheme-mode} that provides enhanced support for Racket, including
highlighting and indentation of Racket-specific forms, and documentation
integration.

Quack is included in the Debian and Ubuntu repositories as part of the
@tt{emacs-goodies-el} package. A Gentoo port is also available (under the name
@tt{app-emacs/quack}).

@subsubsection{Geiser}
@hyperlink["http://www.nongnu.org/geiser/"]{Geiser} provides a programming
environment where the editor is tightly integrated with the Racket
REPL. Programmers accustomed to environments such as Slime or Squeak should
feel at home using Geiser. Geiser requires GNU Emacs 23.2 or better.

Quack and Geiser can be used together, and complement each other nicely. More
information is available in the
@hyperlink["http://www.nongnu.org/geiser/"]{Geiser manual}.

Debian and Ubuntu packages for Geiser are available under the name
@tt{geiser}.

@subsubsection{scheme-mode}
Emacs ships with a major mode for Scheme, @tt{scheme-mode}, that while not as
featureful as the above options works well for editing Racket code. However,
this mode does not provide support for Racket-specific forms.


@subsubsection{Scribble Mode}
No Racket program is complete without documentation. Scribble support for emacs
is available with Neil Van Dyke's
@hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{Scribble Mode}.

@subsubsection{Other Modes for Scribble}
In addition to the above Scribble mode, @tt{texinfo-mode} (included with GNU
Emacs) and plain text modes work well when editing Scribble documents. The
Racket major modes above are not really suited to this task, given how
different Scribble's syntax is from Racket's.


@subsection{Minor Modes}

@subsubsection{Paredit}
@hyperlink["http://mumble.net/~campbell/emacs/paredit.el"]{Paredit} is a minor
mode for pseudo-structurally editing programs in Lisp-like languages. In
addition to providing high-level S-expression editing commands, it prevents you
from accidentally unbalancing parentheses.

Debian and Ubuntu packages for Paredit are available under the name
@tt{paredit-el}.

@subsubsection{scheme-complete}
Alex Shinn's
@hyperlink["http://synthcode.com/wiki/scheme-complete"]{scheme-complete}
provides intelligent, context-sensitive code completion. It also integrates
with Emacs's @tt{eldoc} mode to provide live documentation in the minibuffer.

While this mode was designed for R5RS Scheme, it can still be useful for Racket
development. However, this means that the tool is unaware of large portions of
the Racket standard library, and there may be some discrepancies in the live
documentation in cases where Scheme and Racket have diverged.

@subsubsection{Rainbow Delimiters}
The @hyperlink["http://www.emacswiki.org/emacs/RainbowDelimiters"]{rainbow
delimiters} mode colors parentheses and other delimiters according to their
nesting depth. It makes it easier to know, at a glance, which parentheses
match.

@subsubsection{ParenFace}
@hyperlink["http://www.emacswiki.org/emacs/ParenFace"]{ParenFace} lets you
choose in which face (font, color, etc.) parentheses should be displayed. This
makes it possible to make "tone down" parentheses.


@section{Vim}

@subsection{Settings}

Many distributions of vim ship with support for Scheme, which will mostly work
for Racket. You can enable filetype detection of Racket files as Scheme with the
following:

@verbatim|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=scheme
endif
}|

Alternatively, you can use the
@hyperlink["https://github.com/wlangstroth/vim-racket"]{vim-racket}
plugin to enable auto-detection, indentation, and syntax highlighting
specifically for Racket files. Using the plugin is the easiest method, but if you
would like to roll your own settings or override settings from the plugin, add
something like the following to your vimrc:

@verbatim|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=racket
  au filetype racket set lisp
  au filetype racket set autoindent
endif
}|

However, if you take this path you may need to do more work when installing
plugins because many Lisp-related plugins and scripts for vim are not aware of
Racket. You can also set these conditional commands in a @tt{scheme.vim} or
@tt{racket.vim} file in the @tt{ftplugin} subdirectory of your vim folder.

Most installations of vim will automatically have useful defaults enabled,
but if your installation does not, you will want to set at least the following
in your .vimrc file:

@verbatim|{
" Syntax highlighting
syntax on

" These lines make vim load various plugins
filetype on
filetype indent on
filetype plugin on

" No tabs!
set expandtab
}|

@subsection{Indentation}

You can enable indentation for Racket by setting both the @tt{lisp} and
@tt{autoindent} options. However, the indentation is limited and not as
complete as what you can get in Emacs. You can also use Dorai Sitaram's
@hyperlink["http://evalwhen.com/scmindent/index.html"]{scmindent} for
better indentation of Racket code. The instructions on how to
use the indenter are available on the website.

If you use the built-in indenter, you can customize it by setting how to
indent certain keywords. The vim-racket plugin mentioned above sets
some default keywords for you. You can add keywords yourself in your
.vimrc like this:

@verbatim|{
" By default vim will indent arguments after the function name
" but sometimes you want to only indent by 2 spaces similar to
" how DrRacket indents define. Set the `lispwords' variable to
" add function names that should have this type of indenting.

set lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules
set lispwords+=..more..
}|

@subsection{Highlighting}

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=1230"]{Rainbow
Parenthesis} script for vim can be useful for more visible parenthesis
matching. Syntax highlighting for Scheme is shipped with vim on many platforms,
which will work for the most part with Racket. The vim-racket script will
provide good default highlighting settings for you.

@subsection{Structured editing}

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=2531"]{Slimv}
plugin has a paredit mode that works like paredit in Emacs. However, the plugin
is not aware of Racket. You can either set vim to treat Racket as Scheme files
or you can modify the paredit script to load on @tt{*.rkt} files.

@subsection{Miscellaneous}

If you are installing many vim plugins (not necessary specific to Racket), we
recommend using a plugin that will make loading other plugins easier.
@hyperlink["http://www.vim.org/scripts/script.php?script_id=2332"]{Pathogen} is
one plugin that does this; using it, you can install new plugins by extracting
them to subdirectories in the @tt{bundle} folder of your vim directory.
