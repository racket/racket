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

 @item{@hyperlink["https://github.com/greghendershott/racket-mode"]{Racket mode}
       provides thorough syntax highlighting and DrRacket-style REPL
       and buffer execution support for Emacs.

       Racket mode can be installed via @hyperlink["https://melpa.org/"]{MELPA}
       or manually from the Github repository.}

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
       name @tt{geiser}. A Gentoo port is also available (under the
       name @tt{app-emacs/geiser}).}

 @item{Emacs ships with a major mode for Scheme, @tt{scheme-mode},
       that while not as featureful as the above options, works
       reasonably well for editing Racket code. However, this mode
       does not provide support for Racket-specific forms.}

 @item{No Racket program is complete without documentation. Scribble
       support for Emacs is available with Neil Van Dyke's
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

 @item{@hyperlink["https://github.com/Fuco1/smartparens"]{Smartparens}
       is a minor mode for editing s-expressions, keeping parentheses
       balanced, etc.  Similar to Paredit.}

 @item{@hyperlink["https://github.com/drym-org/symex.el"]{Symex} is an
       intuitive modal (Vim-like) way of editing code with minimum
       keystrokes, built on top of a DSL providing advanced structural
       editing features, and runtime integration with Racket Mode.}

 @item{Alex Shinn's
       @hyperlink["http://synthcode.com/wiki/scheme-complete"]{scheme-complete}
       provides intelligent, context-sensitive code completion. It
       also integrates with Emacs's @tt{eldoc} mode to provide live
       documentation in the minibuffer.

       While this mode was designed for @seclink["r5rs"]{@|r5rs|}, it
       can still be useful for Racket development. The tool is
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

 @item{@hyperlink["https://github.com/countvajhula/mindstream"]{Mindstream}
       lets you enter an interactive programming session (similar
       to DrRacket's Definitions and Interactions workflow) at any time,
       starting from templates you provide. Sessions are implicitly
       versioned, freeing you to experiment without fear of losing work,
       growing organically from throwaway scratch buffers to full projects.}
]

@subsection{Packages specific to Evil Mode}

@itemlist[

 @item{@hyperlink["https://github.com/willghatch/emacs-on-parens"]{on-parens}
       is a wrapper for smartparens motions to work better with
       evil-mode's normal state.}

 @item{@hyperlink["https://github.com/timcharper/evil-surround"]{evil-surround}
       provides commands to add, remove, and change parentheses and
       other delimiters.}

 @item{@hyperlink["https://github.com/noctuid/evil-textobj-anyblock"]{evil-textobj-anyblock}
       adds a text-object that matches the closest of any
       parenthesis or other delimiter pair.}

]

@; ------------------------------------------------------------

@section{Vim}

Many distributions of Vim ship with support for Scheme, which will mostly work
for Racket. Vim also ships with some special support for Racket.

The @tt{racket} filetype comes with
@itemlist[
  @item{syntax highlighting}
  @item{custom indentation for Racket forms}
  @item{and other support including comments and @tt{raco fmt}}
]

There is also support for several @seclink["top" #:doc '(lib "scribblings/raco/raco.scrbl")]{raco commands}
in the form of builtin @tt{compiler} plugins; see @tt{:help compiler} for more
information.

For information about older Vim versions, see @secref{vim-versions}.

@subsection[#:tag "vim-racket"]{Enhanced Racket Support}

Vim will detect your Racket files as Scheme out of the box. To get the
additional features of the Racket filetype, consider installing the
@tt{vim-racket} plugin from
@hyperlink["https://github.com/benknoble/vim-racket"]{benknoble/vim-racket}. It
enables auto-detection of Racket files on top of enhanced indentation and syntax
highlighting. Vim's default support comes from a subset of this plugin;
installing it yourself provides additional features.

The @tt{vim-racket} plugin detects the @tt{filetype} option based on the @(hash-lang)
line. For example:@itemlist[
    @item{A file starting with @code{#lang racket} or @code{#lang racket/base} has @tt{filetype} equal to @tt{racket}.}
    @item{A file starting with @code{#lang scribble/base} or @code{#lang scribble/manual} has @tt{filetype} equal to @tt{scribble}.}
]

The @tt{vim-racket} plugin comes with configuration for Racket and some other
standard Racket languages.

Many Racket languages still need syntax and indent support. If you create Vim
support for other Racket languages, please consider contributing them to
@hyperlink["https://github.com/benknoble/vim-racket"]{benknoble/vim-racket} so
other Vim users will benefit.

@subsection{Indentation}

If you use @secref{vim-racket} and Vim version 9 or greater, improved
indentation for the @tt{racket} filetype is configured out of the box.

Otherwise, you can manually enable indentation for Racket by setting both the
@tt{lisp} and @tt{autoindent} options in Vim. You will want to customize the
buffer-local @tt{lispwords} option to control how special forms are indented.
See @tt{:help 'lispwords'}. However, using @tt{lispwords} for indentation can be
limited and may not be as complete as what you can get in Emacs. You can also
use Dorai Sitaram's
@hyperlink["https://github.com/ds26gte/scmindent"]{scmindent} for better
indentation of Racket code. The instructions on how to use the indenter are
available on the website.

@subsection{Highlighting}

Syntax highlighting for Scheme and Racket is shipped with Vim on many platforms.
You will want to use the @tt{racket} filetype for the best syntax experience;
see @secref{vim-racket} for enhanced syntax highlighting for Racket languages.

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=1230"]{Rainbow
Parenthesis} script for Vim can be useful for more visible parenthesis
matching.

@subsection{Structured Editing}

The @hyperlink["http://www.vim.org/scripts/script.php?script_id=2531"]{Slimv}
plugin has a paredit mode that works like paredit in Emacs. However, the plugin
is not aware of Racket. You can either set Vim to treat Racket as Scheme files
or you can modify the paredit script to load on @filepath{.rkt} files.

For a more Vim-like set of key-mappings, pair either of @itemlist[
    @item{@hyperlink["https://github.com/guns/vim-sexp"]{guns/vim-sexp}}
    @item{@hyperlink["https://github.com/benknoble/vim-sexp"]{benknoble/vim-sexp}}
]@margin-note{The @tt{benknoble/vim-sexp} fork is slightly more modern vimscript.}
with @hyperlink["https://github.com/tpope/vim-sexp-mappings-for-regular-people"]{tpope/vim-sexp-mappings-for-regular-people}.
The experience is on par with paredit, but more comfortable for the fingers.

@subsection{REPLs}

There are many general-purpose Vim + REPL plugins out there. Here are a few that
support Racket out of the box: @itemlist[
    @item{@hyperlink["https://github.com/rhysd/reply.vim"]{rhysd/reply.vim}}
    @item{@hyperlink["https://github.com/kovisoft/slimv"]{kovisoft/slimv}, if you are using the @tt{scheme} filetype}
    @item{@hyperlink["https://github.com/benknoble/vim-simpl"]{benknoble/vim-simpl}}
]

@subsection{Scribble}

Vim support for writing scribble documents is provided by
@hyperlink["https://github.com/benknoble/scribble.vim"]{benknoble/scribble.vim}.

@subsection{Miscellaneous}

If you are installing many Vim plugins (not necessary specific to Racket), we
recommend using a plugin that will make loading other plugins easier. There are
many plugin managers.

@hyperlink["https://github.com/tpope/vim-pathogen"]{Pathogen} is one plugin that
does this; using it, you can install new plugins by extracting them to
subdirectories in the @filepath{bundle} folder of your personal Vim files
(@filepath{~/.vim} on Unix, @filepath{$HOME/vimfiles} on MS-Windows).

With newer Vim versions, you can use the package system (@tt{:help packages}).

One relatively up-to-date reference on the various managers is
@hyperlink["https://vi.stackexchange.com/q/388/10604"]{What are the differences between the vim plugin managers?}.
The same site, @hyperlink["https://vi.stackexchange.com"]{Vi & Vim} is a great
place to get help from Vimmers.

@subsection[#:tag "vim-versions"]{Older Versions of Vim}

As of
@hyperlink["https://github.com/vim/vim/commit/9b03d3e75b4274493bbe76772d7b92238791964c"]{Version 9.0.0336},
Vim ships with runtime files from @secref{vim-racket}, but these exclude
filetype detection for the @tt{racket} filetype. If you are using this version
or versions newer than this you probably want to tweak the suggestions in this
document to use the @tt{racket} filetype instead of @tt{scheme}. You should also
consider installing the plugin yourself to get the latest changes, since Ben is
slow to sync changes upstream to Vim and since the plugin contains improved
filetype detection.

As of @hyperlink["https://github.com/vim/vim/commit/1aeaf8c0e0421f34e51ef674f0c9a182debe77ae"]{version 7.3.518},
Vim detects files with the extension @tt{.rkt} as having the
@tt{scheme} filetype. @hyperlink["https://github.com/vim/vim/commit/9cd91a1e8816d727fbdbf0b3062288e15abc5f4d"]{Version 8.2.3368}
added support for @tt{.rktd} and @tt{.rktl}.

In older versions, you can enable filetype detection of Racket
files as Scheme with the following:

@verbatim[#:indent 2]|{
if has("autocmd")
  autocmd filetypedetect BufReadPost *.rkt,*.rktl,*.rktd set filetype=scheme
endif
}|

If your Vim supports the ftdetect system, in which case it's likely new enough
to support Racket already, you can nevertheless put the following in
@filepath{~/.vim/ftdetect/racket.vim}
(@filepath{$HOME/vimfiles/ftdetect/racket.vim} on MS-Windows; see @tt{:help runtimepath}).

@verbatim[#:indent 2]|{
" :help ftdetect
" If you want to change the filetype only if one has not been set
autocmd BufRead,BufNewFile *.rkt,*.rktl,*.rktd setfiletype scheme
" If you always want to set this filetype
autocmd BufRead,BufNewFile *.rkt,*.rktl,*.rktd set filetype=scheme
}|

@; ------------------------------------------------------------

@section{Sublime Text}

The @hyperlink["https://sublime.wbond.net/packages/Racket"]{Racket package}
provides support for syntax highlighting and building for Sublime Text.
@; ------------------------------------------------------------

@section{Visual Studio Code}

The @hyperlink["https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"]{Magic Racket}
extension provides Racket support including REPL integration and syntax highlighting in Visual Studio Code.
