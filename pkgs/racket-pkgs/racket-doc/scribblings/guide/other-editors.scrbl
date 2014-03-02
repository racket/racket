#lang scribble/manual
@(require "guide-utils.rkt")

@title[#:tag "other-editors" #:style 'toc]{Command-Line Tools and Your Editor of Choice}
@; author["Vincent St-Amour" "Asumu Takikawa" "Jon Rafkind"]

Although DrRacket is the easiest way for most people to start with
Racket, many Racketeers prefer command-line tools and other text
editors.  The Racket distribution includes several command-line tools,
and popular editors include or support packages to make them work well
with Racket.

@local-table-of-contents[]

@; ------------------------------------------------------------
@include-section["cmdline.scrbl"]

@; ------------------------------------------------------------
@section{Emacs}

Emacs has long been a favorite among Lispers and Schemers, and is
popular among Racketeers as well.

@subsection{Major Modes}

@itemlist[

 @item{@hyperlink["http://www.neilvandyke.org/quack/"]{Quack} is an
       extension of Emacs's @tt{scheme-mode} that provides enhanced
       support for Racket, including highlighting and indentation of
       Racket-specific forms, and documentation integration.

       Quack is included in the Debian and Ubuntu repositories as part
       of the @tt{emacs-goodies-el} package. A Gentoo port is also
       available (under the name @tt{app-emacs/quack}).}

 @item{@hyperlink["http://www.nongnu.org/geiser/"]{Geiser} provides a
       programming environment where the editor is tightly integrated
       with the Racket REPL. Programmers accustomed to environments
       such as Slime or Squeak should feel at home using
       Geiser. Geiser requires GNU Emacs 23.2 or better.

       Quack and Geiser can be used together, and complement each
       other nicely. More information is available in the
       @hyperlink["http://www.nongnu.org/geiser/"]{Geiser manual}.

       Debian and Ubuntu packages for Geiser are available under the
       name @tt{geiser}.}

 @item{@hyperlink["https://github.com/greghendershott/racket-mode"]{Racket mode}
       provides thorough syntax highlighting and DrRacket-style REPL
       and buffer execution support for Emacs.

       Racket mode can be installed via @hyperlink["http://melpa.milkbox.net"]{MELPA}
       or manually from the Github repository.}

 @item{Emacs ships with a major mode for Scheme, @tt{scheme-mode},
       that while not as featureful as the above options, but works
       reasonably well for editing Racket code. However, this mode
       does not provide support for Racket-specific forms.}

 @item{No Racket program is complete without documentation. Scribble
       support for emacs is available with Neil Van Dyke's
       @hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{Scribble
       Mode}.

       In addition, @tt{texinfo-mode} (included with GNU Emacs) and
        plain text modes work well when editing Scribble
        documents. The Racket major modes above are not really suited
        to this task, given how different Scribble's syntax is from
        Racket's.}

]

@subsection{Minor Modes}

@itemlist[

 @item{@hyperlink["http://mumble.net/~campbell/emacs/paredit.el"]{Paredit}
       is a minor mode for pseudo-structurally editing programs in
       Lisp-like languages. In addition to providing high-level
       S-expression editing commands, it prevents you from
       accidentally unbalancing parentheses.

       Debian and Ubuntu packages for Paredit are available under the
       name @tt{paredit-el}.}

 @item{Alex Shinn's
       @hyperlink["http://synthcode.com/wiki/scheme-complete"]{scheme-complete}
       provides intelligent, context-sensitive code completion. It
       also integrates with Emacs's @tt{eldoc} mode to provide live
       documentation in the minibuffer.

       While this mode was designed for @seclink["r5rs"]{@|r5rs|}, it
       can still be useful for Racket development. That the tool is
       unaware of large portions of the Racket standard library, and
       there may be some discrepancies in the live documentation in
       cases where Scheme and Racket have diverged.}

 @item{The
       @hyperlink["http://www.emacswiki.org/emacs/RainbowDelimiters"]{RainbowDelimiters}
       mode colors parentheses and other delimiters according to their
       nesting depth. Coloring by nesting depth makes it easier to
       know, at a glance, which parentheses match.}

 @item{@hyperlink["http://www.emacswiki.org/emacs/ParenFace"]{ParenFace}
       lets you choose in which face (font, color, etc.) parentheses
       should be displayed. Choosing an alternate face makes it
       possible to make ``tone down'' parentheses.}

]

@; ------------------------------------------------------------

@section{Vim}

Many distributions of Vim ship with support for Scheme, which will
mostly work for Racket. You can enable filetype detection of Racket
files as Scheme with the following:

@verbatim[#:indent 2]|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=scheme
endif
}|

Alternatively, you can use the
@hyperlink["https://github.com/wlangstroth/vim-racket"]{vim-racket}
plugin to enable auto-detection, indentation, and syntax highlighting
specifically for Racket files. Using the plugin is the easiest method, but if you
would like to roll your own settings or override settings from the plugin, add
something like the following to your @filepath{.vimrc} file:

@verbatim[#:indent 2]|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=racket
  au filetype racket set lisp
  au filetype racket set autoindent
endif
}|

However, if you take this path you may need to do more work when installing
plugins because many Lisp-related plugins and scripts for vim are not aware of
Racket. You can also set these conditional commands in a @filepath{scheme.vim} or
@filepath{racket.vim} file in the @filepath{ftplugin} subdirectory of your vim folder.

Most installations of vim will automatically have useful defaults enabled,
but if your installation does not, you will want to set at least the following
in your @filepath{.vimrc} file:

@verbatim[#:indent 2]|{
" Syntax highlighting
syntax on

" These lines make vim load various plugins
filetype on
filetype indent on
filetype plugin on

" No tabs!
set expandtab
}|

@subsubsub*section{Indentation}

You can enable indentation for Racket by setting both the @tt{lisp} and
@tt{autoindent} options in Vim. However, the indentation is limited and not as
complete as what you can get in Emacs. You can also use Dorai Sitaram's
@hyperlink["http://evalwhen.com/scmindent/index.html"]{scmindent} for
better indentation of Racket code. The instructions on how to
use the indenter are available on the website.

If you use the built-in indenter, you can customize it by setting how to
indent certain keywords. The vim-racket plugin mentioned above sets
some default keywords for you. You can add keywords yourself in your
@filepath{.vimrc} file like this:

@verbatim[#:indent 2]|{
" By default vim will indent arguments after the function name
" but sometimes you want to only indent by 2 spaces similar to
" how DrRacket indents define. Set the `lispwords' variable to
" add function names that should have this type of indenting.

set lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules
set lispwords+=..more..
}|

@subsubsub*section{Highlighting}

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=1230"]{Rainbow
Parenthesis} script for vim can be useful for more visible parenthesis
matching. Syntax highlighting for Scheme is shipped with vim on many platforms,
which will work for the most part with Racket. The vim-racket script
provides good default highlighting settings for you.

@subsubsub*section{Structured Editing}

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=2531"]{Slimv}
plugin has a paredit mode that works like paredit in Emacs. However, the plugin
is not aware of Racket. You can either set vim to treat Racket as Scheme files
or you can modify the paredit script to load on @filepath{.rkt} files.

@subsubsub*section{Scribble}

Vim support for writing scribble documents is provided by the
@hyperlink["http://www.vim.org/scripts/script.php?script_id=3756"]{scribble.vim}
plugin.

@subsubsub*section{Miscellaneous}

If you are installing many vim plugins (not necessary specific to Racket), we
recommend using a plugin that will make loading other plugins easier.
@hyperlink["http://www.vim.org/scripts/script.php?script_id=2332"]{Pathogen} is
one plugin that does this; using it, you can install new plugins by extracting
them to subdirectories in the @filepath{bundle} folder of your Vim installation.
