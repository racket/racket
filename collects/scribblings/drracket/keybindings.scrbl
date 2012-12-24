#lang scribble/doc
@(require "common.rkt"
          scribble/struct scribble/bnf
          racket/list racket/runtime-path racket/port
          mrlib/tex-table
          (for-label drracket/tool-lib))

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
strike @nonterm{key}. On Windows and Mac OS X, Meta is only
available through the Escape key.

DEL is the Delete key.

SPACE is the Space bar.
  
On most keyboards, ``<'' and ``>'' are shifted characters. So, to
get M->, you actually have to type Meta-Shift->. That is, press and
hold down both the Meta and Shift keys, and then strike ``>''.

On Windows (and sometimes under Unix)
some of these keybindings are actually standard menu
items.  Those keybindings will behave according to the menus, unless
the @onscreen{Enable keybindings in menus} preference is unchecked.
For example, the C-e keybinding mentioned above actually toggles
the visibility of the interactions window.

@index['("Emacs keybindings")]{If} you are most familiar with
Emacs-style key bindings (especially on windows or some linux installations
where the control key is, by default, for the menu shortcuts), 
you should uncheck the @onscreen{Enable
keybindings in menus} preference. Many of the keybindings below are
inspired by Emacs. See also @secref["defining-shortcuts"] for suggestions
on how to bind keys to menu items on a selective basis.

And finally, the authoritative source for keybindings 
is the @onscreen{Edit} menu's @onscreen{Show Active Keybindings}
menu item. Keybindings in DrRacket are often sensitive to
the window that has the keyboard focus, so the contents
of the window that @onscreen{Show Active Keybindings} opens
will depend where the keyboard focus was when the menu was
selected.

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

@keybinding["C-C C-Z"]{move the cursor to the interactions window}
@keybinding["C-F6"]{move the cursor between different windows (usually
                    the interactions and definitions windows, but also the
                    search window and other editable portions of DrRacket).
                    Also, search for ``shift-focus'' in the
                    @onscreen{Show Active Keybindings} menu's window for more, 
                    platform-specific keybindings that have this functionality}
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
                     and put the insertion point in the argument list of the lambda}

@keybinding["C-c C-o"]{the sexpression following the
  insertion point is put in place of its containing sexpression}
@keybinding["C-c C-e"]{the first and last characters (usually parentheses)
  of the containing expression are removed}
@keybinding["C-c C-l"]{wraps a let around the
  sexpression following the insertion point and puts a printf in at
  that point (useful for debugging).}

@keybinding["M-o"]{toggle @as-index{overwrite mode}}

@keybinding["C-x r a"]{Adjust nearby ASCII art rectangles 
                       (that use @litchar{+}, @litchar{-}, or @litchar{|})
                       to use Unicode characters.
                       
                       For example, if the insertion point is next to this rectangle:
                       @tabular[(list (list @litchar{+-+})
                                      (list @litchar{| |})
                                      (list @litchar{+-+}))]
                       then the keystroke will turn it into this one:
                       @tabular[(list (list @litchar{╔═╗})
                                      (list @litchar{║ ║})
                                      (list @litchar{╚═╝}))]
                       Similarly, if the rectangle near the insertion point has
                       mixed Unicode and ASCII, it will all be converted to 
                       the Unicode characters.
                       }
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

@section{Evaluation}

@itemize[
@keybinding["F5"]{Run}
]

@section{Documentation}
@itemize[
  @keybinding["f1"]{Search in the documentation for the words near the insertion point}
  @keybinding["f2"]{Reveal the blue box for the identifier at the insertion point (requires
                    background check syntax to be enabled, or normal check syntax to have been
                    run).}
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
@keybinding['("C-\\" "M-\\" "c:x;l")]{traces backwards from the insertion
point, looking for a backslash followed by a @index["LaTeX"]{LaTeX} 
macro name or a prefix of such a name. If a macro name is found,
it replaces the backslash and the name with the corresponding key in
the table below; if a (proper) prefix @math{p} is found, it replaces @math{p} 
with the longest common prefix of all macro names that have @math{p} as a 
prefix (unless there is only one such name, in which case it behaves as if 
@math{p} were a complete macro name).

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

Since the file contains plain Racket code, you can write keybindings
files that use DrRacket's @seclink[#:doc '(lib
"scribblings/tools/tools.scrbl") "implementing-tools"]{Extension API}.
For example, the following file binds ``control-t'' and ``control-='' to
a execute the program and open a new tab respectively, as they were used
before version 5.2.

@racketmod[
s-exp framework/keybinding-lang

(define modifiers
  (apply string-append
         (map (λ (p)
                (case p
                  [(ctl) "c:"] [(cmd) "d:"] [(alt meta) "m:"]
                  [(shift) "s:"] [(option) "a:"]))
              (get-default-shortcut-prefix))))

(define-syntax-rule (frame-key key command)
  (keybinding
   (string-append modifiers key)
   (λ (ed evt)
     (when (is-a? ed text:basic<%>)
       (define fr (send ed get-top-level-window))
       @code:comment{note: fr could be #f}
       (when fr (send fr command))))))

(frame-key "t" execute-callback)
(frame-key "=" create-new-tab)
]

Another example, this file rebinds ``control-w'' to delete the word
behind the insertion point, but it does it by setting a new key to 
be an existing keyboard shortcut. If you see a key in the 
@onscreen{Show Active Keybindings} dialog (in the @onscreen{Keybindings}
submenu of the @onscreen{Edit} menu), then you can use its
name with the new keystroke you want, like this:

@racketmod[
s-exp framework/keybinding-lang

(define (rebind key command)
  (keybinding
   key
   (λ (ed evt)
     (send (send ed get-keymap) call-function
           command ed evt #t))))

(rebind "c:w" "backward-kill-word")
]

This example shows how to bind a menu item (based on its name) to a particular key.
The call at the end of the example binds ``control-a'' to the @onscreen{Run}
menu item.

@racketmod[
s-exp framework/keybinding-lang

(define (menu-bind key menu-item)
  (keybinding
   key
   (λ (ed evt)
     (define canvas (send ed get-canvas))
     (when canvas
       (define menu-bar (find-menu-bar canvas))
       (when menu-bar
         (define item (find-item menu-bar menu-item))
         (when item
           (define menu-evt
             (new control-event% 
                  [event-type 'menu]
                  [time-stamp 
                   (send evt get-time-stamp)]))
           (send item command menu-evt)))))))

(define/contract (find-menu-bar c)
  (-> (is-a?/c area<%>) (or/c #f (is-a?/c menu-bar%)))
  (let loop ([c c])
    (cond
      [(is-a? c frame%) (send c get-menu-bar)]
      [(is-a? c area<%>) (loop (send c get-parent))]
      [else #f])))

(define/contract (find-item menu-bar label)
  (-> (is-a?/c menu-bar%)
      string?
      (or/c (is-a?/c selectable-menu-item<%>) #f))
  (let loop ([o menu-bar])
    (cond
      [(is-a? o selectable-menu-item<%>)
       (and (equal? (send o get-plain-label) label)
            o)]
      [(is-a? o menu-item-container<%>)
       (for/or ([i (in-list (send o get-items))])
         (loop i))]
      [else #f])))

(menu-bind "c:a" "Run")]

Note that DrRacket does not reload keybindings files automatically when you
make changes, so you'll need to restart DrRacket to see changes to
the file.

@section{Sending Program Fragments to the REPL}

@index['("Emacs keybindings")]Users comfortable with Emacs and the conventional Lisp/Scheme-style
of interaction with an ``inferior process'' commonly request
keybindings in DrRacket that send program fragments to be evaluated
at the prompt. This style of interaction is fraught with difficulty, 
especially for beginners, and so DrRacket, by default, does not support
it. Instead, clicking DrRacket's ``Run'' button starts with a clean slate
and sends the entire contents of the definitions window, ensuring that
the state in the REPL matches what you would expect by reading
the source code of the program.

Based on years of experience with Emacs modes, some of the authors 
consider this mode of interaction also appropriate for experienced 
programmers. Indeed, they go through great effort to mimic this 
behavior in Emacs. 

That said, some people may wish to use such incremental keystroke modes
anyway. Therefore the remainder of this section illustrates how to add such
an incremental mode for your personal use with an example keybindings
file. Specifically, the file shows how to add the ability to send
expressions piecemeal to the interactions window. It also demonstrates how
to pull together a bunch of pieces of DrRacket's implementation and its
libraries to implement keystrokes. 

@(define-runtime-path incremental-keybindings.rkt "incremental-keybindings.rkt")
@(let ([sp (open-output-string)])
   (call-with-input-file incremental-keybindings.rkt
     (λ (port)
       (copy-port port sp)))
   (codeblock (get-output-string sp)))

Others may wish to use the above example to invent other keystrokes for
making work in DrRacket convenient. 
