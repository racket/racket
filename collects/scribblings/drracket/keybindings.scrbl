#lang scribble/doc
@(require "common.ss"
	  scribble/struct
          scribble/bnf
          racket/list
	  mrlib/tex-table
          (for-label racket/gui/base))

@(define (keybinding key . desc)
   (let* ([keys (if (string? key) (list key) key)]
          [key-str (apply string-append (add-between keys " "))])
     (apply item @index[(map (lambda (x) (format "~a keybinding" x)) keys) key-str] " : " desc)))

@(define-syntax-rule (def-mod-beg id)
  (begin
   (require (for-label racket/base))
   (define id @racket[#%module-begin])))
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

@section{LaTeX and TeX inspired keybindings}

@itemize[
@keybinding['("C-\\" "M-\\")]{traces backwards from the insertion
point, looking for a backslash followed by a @index["LaTeX"]{LaTeX} macro name; if one is
found, it replaces the backslash and the macro's name with the keybinding.
These are the currently supported macro names and the keys they map into:
@(make-table
  '()
  (map (lambda (line) 
	 (let ([macro (list-ref line 0)]
	       [char (list-ref line 1)])
	   (list (make-flow (list (make-paragraph (list (index (format "\\~a keyboard shortcut" macro))
							(tt (format " \\~a" macro))))))
		 (make-flow (list (make-paragraph (list (hspace 1) char)))))))
       tex-shortcut-table))
}
]

@section[#:tag "defining-shortcuts"]{Defining Custom Shortcuts}

 The @onscreen{Add User-defined Keybindings...} menu item in the
 @onscreen{Keybindings} sub-menu of @onscreen{Edit} selects a file
 containing Racket definitions of keybindings. The file must contain a
 module that uses a special keybindings language,
 @racket[framework/keybinding-lang]. To do so, begin your file with
 this line:

@racketmod[
s-exp framework/keybinding-lang
]

The @racket[framework/keybinding-lang] languages provides all of the bindings
from @racketmodname[racket], @racketmodname[racket/class], and
@racketmodname[drracket/tool-lib], 
except that it adjusts @|mz-mod-begin| to
introduce a @racketidfont{keybinding} form:

@specform[#:literals (keybindings)
          (keybinding string-expr proc-expr)]{

Declares a keybinding, where @racket[string-expr] must produce a
suitable first argument for @xmethod[keymap% map-function], and the
@racket[proc-expr] must produce a suitable second argument for
@xmethod[keymap% add-function].}

For example, this remaps the key combination ``control-a'' key to ``!''.

@racketmod[
s-exp framework/keybinding-lang
(keybinding "c:a" (λ (editor evt) (send editor insert "!")))
]

Note that DrRacket does not reload this file automatically when you
make a change, so you'll need to restart DrRacket to see changes to
the file.
