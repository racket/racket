#lang scribble/doc
@(require "common.ss"
          scribble/bnf
          (for-label scheme/gui/base))

@(define (keybinding key . desc)
   (apply item @index[(list (format "~a keybinding" key)) key] " : " desc))

@(define-syntax-rule (def-mod-beg id)
  (begin
   (require (for-label mzscheme))
   (define id @scheme[#%module-begin])))
@(def-mod-beg mz-mod-begin)

@title{Keyboard Shortcuts}

@index['("keybindings")]{Most} key presses simply insert a character
into the editor, such as @litchar{a}, @litchar{3}, or
@litchar{(}. Other keys and key combinations act as keyboard shortcuts
that move the blinking caret, delete a line, copy the selection,
etc. Keyboard shortcuts are usually trigger by key combinations using
the Control, Meta, or Command key.

@margin-note{Many of the key-binding actions can also be performed
with menu items.}

C-@nonterm{key} means press the Control key, hold it down and then
press @nonterm{key} and then release them both. For example: C-e
(Control-E) moves the blinking caret to the end of the current line.

M-@nonterm{key} is the same as C-@nonterm{key}, except with the Meta
key.  Depending on your keyboard, Meta may be called ``Left,''
``Right,'' or have a diamond symbol, but it's usually on the bottom
row next to the space bar. M-@nonterm{key} can also be performed as a
two-character sequence: first, strike and release the Escape key, then
strike @nonterm{key}. Under Windows and Mac OS X, Meta is only
available through the Escape key.

DEL is the Delete key.

SPACE is the Space bar.
  
On most keyboards, ``<'' and ``>'' are shifted characters. So, to
get M->, you actually have to type Meta-Shift->. That is, press and
hold down both the Meta and Shift keys, and then strike ``>''.

Under Windows, some of these keybindings are actually standard menu
items.  Those keybindings will behave according to the menus, unless
the @onscreen{Enable keybindings in menus} preference is unchecked.

@index['("Emacs keybindings")]{If} you are most familiar with
Emacs-style key bindings, you should uncheck the @onscreen{Enable
keybindings in menus} preference. Many of the keybindings below are
inspired by Emacs.}


@section{Moving Around}

@itemize[
@keybinding["C-f"]{move forward one character}
@keybinding["C-b"]{move backward one character}
@keybinding["M-f"]{move forward one word}
@keybinding["M-b"]{move backward one word}
@keybinding["C-v"]{move forward one page}
@keybinding["M-v"]{move backward one page}
@keybinding["M-<"]{move to beginning of file}
@keybinding["M->"]{move to end of file}

@keybinding["C-a"]{move to beginning of line (left)}
@keybinding["C-e"]{move to end of line (right)}
@keybinding["C-n"]{move to next line (down)}
@keybinding["C-p"]{move to previous line (up)}

@keybinding["M-C-f"]{move forward one S-expression}
@keybinding["M-C-b"]{move backward one S-expression}
@keybinding["M-C-u"]{move up out of an S-expression}
@keybinding["M-C-d"]{move down into a nested S-expression}
@keybinding["M-C-SPACE"]{select forward S-expression}
@keybinding["M-C-p"]{match parentheses backward}

@keybinding["M-C-left"]{move backwards to the nearest editor box}
@keybinding["A-C-left"]{move backwards to the nearest editor box}
@keybinding["M-C-right"]{move forward to the nearest editor box}
@keybinding["A-C-right"]{move forward to the nearest editor box}
@keybinding["M-C-up"]{move up out of an embedded editor}
@keybinding["A-C-up"]{move up out of an embedded editor}
@keybinding["M-C-down"]{move down into an embedded editor}
@keybinding["A-C-down"]{move down into an embedded editor}

@keybinding["C-F6"]{move the cursor from the definitions
window to the interactions window (or the search window, if it is open).}
]

@section{Editing Operations}

@itemize[
@keybinding["C-_"]{undo}
@keybinding["C-+"]{redo}
@keybinding["C-x u"]{undo}

@keybinding["C-d"]{delete forward one character}
@keybinding["C-h"]{delete backward one character}
@keybinding["M-d"]{delete forward one word}
@keybinding["M-DEL"]{delete backward one word}
@keybinding["C-k"]{delete forward to end of line}
@keybinding["M-C-k"]{delete forward one S-expression}

@keybinding["M-w"]{copy selection to clipboard}
@keybinding["C-w"]{delete selection to clipboard (cut)}
@keybinding["C-y"]{paste from clipboard (yank)}

@keybinding["C-t"]{transpose characters}
@keybinding["M-t"]{transpose words}
@keybinding["M-C-t"]{transpose sexpressions}

@keybinding["M-C-m"]{toggle dark green marking of matching parenthesis}
@keybinding["M-C-k"]{cut complete sexpression}

@keybinding["M-("]{wrap selection in parentheses}
@keybinding["M-["]{wrap selection in square brackets}
@keybinding["M-{"]{wrap selection in curly brackets}
@keybinding["M-S-L"]{wrap selection in @litchar{(lambda () }...@litchar{)}
                     and put the insertion point in the arglist of the lambda}

@keybinding["C-c C-o"]{the sexpression following the
  insertion point is put in place of its containing sexpression}
@keybinding["C-c C-e"]{the first and last characters (usually parentheses)
  of the containing expression are removed}
@keybinding["C-c C-l"]{wraps a let around the
  sexpression following the insertion point and puts a printf in at
  that point (useful for debugging).}


@keybinding["M-o"]{toggle @as-index{overwrite mode}}
]

@section{File Operations}

@itemize[
@keybinding["C-x C-s"]{save file}
@keybinding["C-x C-w"]{save file under new name}
]

@section{Search}

@itemize[
@keybinding["C-s"]{search for string forward}
@keybinding["C-r"]{search for string backward}
]

@section{Miscellaneous}

@itemize[
@keybinding["F5"]{Run}
]


@section{Interactions}

The @tech{interactions window} has all of the same keyboard shortcuts
as the @tech{definitions window} plus a few more:

@itemize[
@keybinding["M-p"]{bring the previously entered expression down to the prompt}
@keybinding["M-n"]{bring the expression after the current expression in the
  expression history down to the prompt}
]

@section[#:tag "defining-shortcuts"]{Defining Custom Shortcuts}

 The @onscreen{Add User-defined Keybindings...} menu item in the
 @onscreen{Keybindings} sub-menu of @onscreen{Edit} selects a file
 containing Scheme definitions of keybindings. The file must contain a
 single module that uses a special keybindings language,
 @scheme[framework/keybinding-lang].  For example, a file named
 @filepath{mykeys.ss} for keybindings might contain the following
 code:

@schemeblock[
(module mykeys framework/keybinding-lang
  ...)
]

The @scheme[framework/keybinding-lang] languages provides all bindings
@schememodname[mzscheme], @schememodname[scheme/gui/base], and
@scheme[scheme/classs], except that it adjusts @|mz-mod-begin| to
introduce a @schemeidfont{keybinding} form:

@specform[#:literals (keybindings)
          (keybinding string-expr proc-expr)]{

Declares a keybinding, where @scheme[string-expr] must produce a
suitable first argument for @xmethod[keymap% map-function], and the
@scheme[proc-expr] must produce a suitable second argument for
@xmethod[keymap% add-function].}

Note that @schememodname[drscheme/tool-lib] adds all of the names
defined in @other-manual['(lib "scribblings/tools/tools.scrbl")] to
your keybindings module, which helps in defining DrScheme-specific
keybindings.


