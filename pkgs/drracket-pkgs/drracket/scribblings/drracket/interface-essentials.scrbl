#lang scribble/doc
@(require "common.rkt"
          scribble/decode scribble/eval scribble/struct scribble/racket
          (for-label racket/gui/base framework compiler/embed)
          setup/getinfo racket/pretty string-constants)

@(define (ioinputfont . s)
   (apply tt s))
@(define (iooutputfont . s)
   (make-element output-color (decode-content s)))

@title[#:tag "interface-essentials" #:style 'toc]{Interface Essentials}

The DrRacket window has three parts: a row of buttons at the top, two
editing panels in the middle, and a status line at the bottom.

@centerline{@image[#:scale 0.7 "example.png"]}

The top editing panel, called the @deftech{definitions window}, is for
defining programs. The above figure shows a program that defines the
function @racket[square].

The bottom panel, called the @deftech{interactions window}, is for
evaluating Racket expressions interactively. The @onscreen{Language} line
in the interactions window indicates which primitives are available in
the definitions and interactions windows.  In the above figure, the
language is determined from the program source's @hash-lang[] line.

@margin-note{The interactions window is described further in
@secref["interactions-window"], later in this manual.}

Clicking the @onscreen{Run} button evaluates the program in the
definitions window, making the program's definitions available in the
interactions window. Given the definition of @racket[square] as in the
figure above, typing @racket[(square 2)] in the interactions window
produces the result @racket[4].

The @deftech{status line} at the bottom of DrRacket's window provides
information about the current line and position of the editing caret,
whether the current file can be modified, and whether DrRacket is
currently evaluating any expression. The @as-index{recycling icon}
flashes while DrRacket is ``recycling'' internal resources, such as
memory.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "buttons"]{Buttons}

The left end of the row of buttons in DrRacket contains a miniature
button with the @index['("filename button")]{current file's
name}. Clicking the button opens a menu that shows the file's full
pathname. Selecting one of the menu entries produces an open-file
dialog starting in the corresponding directory.

Below the filename button is a @as-index{@onscreen{(define ...)}
button} for a pop-up menu of names that are defined in the definitions
window. Selecting an item from the menu moves the blinking caret to
the corresponding definition.

The @as-index{@onscreen{Save} button} appears whenever the definitions
window is modified. Clicking the button saves the contents of the
definitions window to a file. The current name of the file appears to
the left of the @onscreen{Save} button, but a file-selection dialog
appears if the file has never been saved before.

The @as-index{@onscreen{Step} button}---which appears only for the
@|HtDP| teaching languages @drlang{Beginning Student} through
@drlang{Intermediate Student with Lambda}---starts the
@as-index{Stepper}, which shows the evaluation of a program as a
series of small steps. Each evaluation step replaces an expression in
the program with an equivalent one using the evaluation rules of
DrRacket. For example, a step might replace @racket[(+ 1 2)] with
@racket[3]. These are the same rules used by DrRacket to evaluate a
program.  Clicking @onscreen{Step} opens a new window that contains
the program from the definitions window, plus several new buttons:
these buttons allow navigation of the evaluation as a series of steps.

@margin-note{The debugging interface is described further in
@secref["debugger"], later in this manual.}

The @as-index{@onscreen{Debug} button}---which does @emph{not} appear
for the @|HtDP| teaching languages---starts a more conventional
stepping @as-index{debugger}.  It runs the program in the definitions
window like the @onscreen{Run} button, but also opens a debugging
panel with several other buttons that provide control over the
program's execution.

Clicking the @as-index{@onscreen{Check Syntax} button} annotates the
program text in the definitions window.  It add the following
annotations:

@itemize[

 @item{@bold{Syntactic Highlighting:} Imported variables and locally
        defined variables are highlighted with color
        changes. Documented identifiers are hyperlinked (via a
        right-click) to the documentation page.}

 @item{@bold{Lexical Structure:} The lexical structure is shown with
       arrows overlaid on the program text.  When the mouse cursor
       passes over a variable, DrRacket draws an arrow from the
       binding location to the variable, or from the binding location
       to every bound occurrence of the variable.

       @index['("Check syntax" "purple arrows")]{@index['("Check
       syntax" "question-mark arrows")]{In}} addition to indicating
       definite references with blue arrows, DrRacket also draws
       arrows to indicate potential references within macro
       definitions. Potential arrows are drawn in purple and annotated
       with a question mark to indicate uncertainty, because DrRacket
       cannot predict how such identifiers will eventually be
       used. Their roles may depend on the arguments to the macro and
       the context the macro is used in.

       @index['("alpha renaming")]{Additionally}, right-clicking (or
       Control-clicking on Mac OS X) on a variable activates a
       pop-up menu that lets you jump from binding location to bound
       location and vice versa, @as-index{@"\u03B1"-rename} the
       variable, or tack the arrows so they do not disappear.}

 @item{@index['("tail calls")]{@bold{Tail Calls:}} Any
       sub-expression that is (syntactically) in tail-position with
       respect to its enclosing context is annotated by drawing a
       light purple arrow from the tail expression to its surrounding
       expression.}

  @item{@bold{Require Annotations:} Right-clicking (or
        Control-clicking on Mac OS X) on the argument to
        @racket[require] activates a pop-up menu that lets you open the
        file that contains the @racket[require]d module.

        Passing the mouse cursor over a @racket[require] expression
        inside a module shows all of the variables that are used from
        that @racket[require] expression. Additionally, if no
        variables are used from that require expression, it is colored
        like an unbound variable.

         Finally, passing the mouse cursor over a variable that is
         imported from a module shows the module that it is imported
         from in a status line at the bottom of the frame.}

]

Check Syntax also runs automatically as you edit your program,
and the bottom, rightmost corner of the DrRacket window
shows its status. A red dot means that something has gone wrong;
move your mouse over the dot to find out what is wrong. Mis-matched parentheses indicates
that the buffer's parens are also mismatched; mouse over the parens for details.

When nothing goes wrong, the colors indicate the stages processing of the program:
blue (expanding), purple (computing check syntax information), orchid (updating the
editor with the check syntax information), and green (finished).

Also, right-clicking in that area yields a menu that lets you disable
(or re-eneable) automatic Check Syntax.

The @as-index{@onscreen{Run} button} evaluates the program in the
@tech{definitions window} and resets the @tech{interactions window}.

The @as-index{@onscreen{Break} button} interrupts an evaluation, or
beeps if DrRacket is not evaluating anything. For example, after
clicking @onscreen{Run} or entering an expression into the
interactions window, click @onscreen{Break} to cancel the
evaluation. Click the @onscreen{Break} button once to try to interrupt
the evaluation gracefully; click the button twice to kill the
evaluation immediately.

@; ----------------------------------------------------------------------

@section[#:tag "choose-language"]{Choosing a Language}

@section-index["language levels"]

DrRacket supports multiple dialects of Racket, as well as some
non-Racket languages. You specify a language in one of two ways:

@itemize[

 @item{Select the @drlang{Use the language declared in the source}
       option (via the @menuitem["Language" "Choose Language..."] menu
       item), and then specify a specific language as part of the
       program usually by starting the definitions-window content with
       @hash-lang[].}

  @item{Select the @menuitem["Language" "Choose Language..."] menu
       item, and choose a specific language. After changing the
       language, click @onscreen{Run} to reset the language in the
       interactions window. The bottom-left corner of DrRacket's main
       window also has a shortcut menu item for selecting previously
       selected languages.}

]

Using a language declared in a program's source is the recommend mode,
and it is described further in @secref["module"].

The @menuitem["Language" "Choose Language..."] dialog contains a
@onscreen{Show Details} button for configuring certain details of the
chosen language. Whenever the selected options do not match the
default language specification, a @onscreen{Custom} indicator appears
next to the language-selection control at the top of the dialog.

See @secref["languages"] (later in this manual) for more information
on the languages that DrRacket supports.

@; ----------------------------------------------------------------------

@section[#:tag "editor"]{Editing with Parentheses}

@index['("flashing parenthesis matches")]{@index['("gray highlight
regions")]{In}} Racket mode, especially, DrRacket's editor provides
special support for managing parentheses in a program. When the
blinking caret is next to a parenthesis, DrRacket shades the region
between the parenthesis and its matching parenthesis. This feature is
especially helpful when for balancing parentheses to complete an
expression.

@index['("formatting Racket code")]{@index['("indenting Racket
code")]{Although}} whitespace is not significant in Racket, DrRacket
encourages a particular format for Racket code. When you type Enter or
Return, the editor inserts a new line and automatically indents it. To
make DrRacket re-indent an existing line, move the blinking caret to
the line and hit the Tab key. (The caret can be anywhere in the line.)
You can re-indent an entire region by selecting the region and typing
Tab.

@index['("changing a parenthesis as you type")]{@index['("automatic
parenthesis")]{DrRacket}} also rewrites parenthesis as you type them,
in order to make them match better. If you type a closing parenthesis
@litchar{)}, a closing square bracket @litchar{]}, or a closing curly brace
@litchar["}"], and if DrRacket can match it back to some earlier opening
parenthesis, bracket, or brace, then DrRacket changes what you type to
match.  DrRacket also rewrites open square brackets, usually to an
open parenthesis. There are some exceptions where opening square
brackets are not automatically changed to parentheses:

@itemize[

 @item{If the square bracket is after @racket[cond]-like keyword,
       potentially skipping some of the sub-expressions in the
       @racket[cond]-like expression (for example, in a @racket[case]
       expression, the square brackets start in the second
       sub-expression).}

 @item{If the square bracket begins a new expression immediately after
       a @racket[local]-like keyword. Note that the second expression
       after a @racket[local]-like keyword will automatically become
       an ordinary parenthesis.}

 @item{If the square bracket is after a parenthesis that is after a
       @racket[letrec]-like keyword,}

 @item{If the square bracket is in a sequence and the s-expression
      before in the sequence is a compound expression, DrRacket uses
      the same kind parenthesis, brace, or bracket as before, or}

 @item{If the square bracket is in the middle of string,
       comment, character, or symbol.}
]

The upshot of DrRacket's help is that you can always use the
(presumably unshifted) square brackets on your keyboard to type
parenthesis. For example, when typing

@racketblock[
(define (length l)
  (cond
   [(empty? l) 0]
   [else (+ 1 (length (rest l)))]))
]

If you always type @litchar{[} and @litchar{]} where any of the square
brackets or parentheses appear, DrRacket will change the square
brackets to match the code above.

Of course, these features can be disabled and customized in the
preferences dialog; see @secref["prefs-explanation"].  Also, in case
DrRacket does not produce the character you want, holding down the
control key while typing disables DrRacket's parenthesis, brace, and
bracket converter.
 
@; -------------------------------

@section{Searching}

DrRacket's search and replace feature is interactive,
similar to those in Safari, Firefox, and Emacs, but with a
few differences.

To start a search, first select the @onscreen{Find} menu
item from the @onscreen{Edit} menu. This will open a small
editor at the bottom of the DrRacket window. Start typing in
there and, as you type, all occurrences of the string you're
searching for will be circled in the editor window. Watch
the space right next to the search window to see how many
occurrences of the search string there are in your
file. When you're ready, you use the @onscreen{Find Again}
menu item to jump to the first occurrence of the search
string. This will color in one of the circles. Use
@onscreen{Find Again} a second time to jump to the next
occurrence.

If you click back into the definitions window, the
@onscreen{Find Again} menu item, DrRacket will move the
selection to the next occurrence of the search string.

DrRacket also supports a mode where typing in the search
editor takes you directly to the next occurrence of the
search string, without selecting the @onscreen{Find Again}
menu item. In the preference dialog, in the
@onscreen{Editing} section and then in the
@onscreen{General} section is a checkbox labeled
@onscreen{Search using anchors}. When it is checked,
DrRacket shows a little red dot and a red line indicating
where the @deftech{search anchor} is. When the search anchor
is enabled, typing in the search window jumps to the first 
occurrence of the search string after the anchor.

@; -------------------------------

@section{Tabbed Editing}

DrRacket's allows you to edit multiple files in a single window via
tabs. The @menuitem["File" "New Tab"] menu item creates a new tab to
show a new file. Each tab has its own interactions window.

In the @onscreen{General} pane of the
the preferences window, a checkbox labeled @onscreen{Open files in
separate tabs} causes DrRacket to open files in new tabs in the
frontmost window, rather than opening a new window for the file.

The key bindings Control-Pageup and Control-Pagedown move between
tabs. On Mac OS X, Command-Left-Square-Bracket and Command-Right-Square-Bracket also
move between tabs.

@; ----------------------------------------------------------------------

@section[#:tag "interactions-window"]{The Interactions Window}

@index['("> prompt")]{@index['("evaluating expressions")]{The}}
interactions window lets you type an expression after the @tt{>}
prompt for immediate evaluation. You cannot modify any text before the
last @tt{>} prompt. To enter an expression, the blinking caret must
appear after the last prompt, and also after the space following the
prompt.

When you type a complete expression and hit Enter or Return, DrRacket
evaluates the expression and prints the result. After printing the
result, DrRacket creates a new prompt for another expression. Some
expressions return a special ``void'' value; DrRacket never prints
void, but instead produces a new prompt immediately.

If the expression following the current prompt is incomplete, then
DrRacket will not try to evaluate it. In that case, hitting Enter or
Return produces a new, auto-indented line. You can force DrRacket to
evaluate the expression by typing Alt-Return or Command-Return
(depending on your platform).

To copy the @as-index{previous expression} to the current prompt, type
ESC-p (i.e., type Escape and then type p). Type ESC-p multiple times
to @as-index{cycle back through old expressions}. Type ESC-n to cycle
forward through old expressions. There are other keys that have these
same functions; see @seclink["Keyboard Shortcuts"] and the menu
item @onscreen{Show Active Keybindings} menu item in the @onscreen{Edit} menu.

Clicking the @onscreen{Run} button evaluates the program in the
@tech{definitions window} and makes the program's definitions
available in the interactions window. Clicking @onscreen{Run} also
resets the interactions window, erasing all old interactions and
removing old definitions from the interaction environment. Although
@onscreen{Run} erases old @tt{>} prompts, ESC-p and ESC-n can still
retrieve old expressions.

@; ----------------------------------------

@subsection{Errors}

@index['("error highlighting")]{Whenever} DrRacket encounters an error
while evaluating an expression, it prints an error message in the
interactions window and highlights the expression that triggered the
error. The highlighted expression might be in the definitions window,
or it might be after an old prompt in the interactions window.

For certain kinds of errors, DrRacket turns a portion of the error
message into a hyperlink. Click the hyperlink to get help regarding a
function or keyword related to the error.

For some run-time errors, DrRacket shows a bug icon next to the error
message. Click the bug icon to open a window that shows a ``stack'' of
expressions that were being evaluated at the time of the error. In
addition, if the expressions in the stack appear in the
@tech{definitions window}, a red arrow is drawn to each expression
from the next deeper one in the stack.

@; ----------------------------------------

@subsection{Input and Output}

@section-index["I/O"]

Many Racket programs avoid explicit input and output operations,
obtaining input via direct function calls in the @tech{interactions
window}, and producing output by returning values. Other Racket
programs explicitly print output for the user during evaluation using
@as-index{@racket[write]} or @as-index{@racket[display]}, or
explicitly request input from the user using @as-index{@racket[read]}
or @as-index{@racket[read-char]}.

Explicit input and output appear in the @tech{interactions window},
but within special boxes that separate explicit I/O from normal
expressions and results. For example, evaluating

@racketblock[
@#,tt{>} (read)
]

in the interactions window produces a special box for entering input:

@centerline{@image[#:scale 0.6 "io.png"]}

Type a number into the box and hit Enter, and that number becomes the
result of the @racket[(read)] expression. Once text is submitted for
an input box, it is moved outside the input box, and when DrRacket
shows a new prompt, it hides the interaction box. Thus, if you type
@racket[5] in the above input box and hit Return, the result appears
as follows:

@racketblock[
@#,tt{>} (read)
@#,ioinputfont{5}
@#,racketresult[5]
@#,tt{>} @#,tt{_}
]

In this case, the first @ioinputfont{5} is the input, and the second
@racketresult[5] is the result of the @racket[(read)] expression. The
second @racketresult[5] is colored blue, as usual for a result printed
by DrRacket. (The underscore indicates the location of the blinking
caret.)

Output goes into the @tech{interactions window} directly. If you run
the program

@racketmod[
racket
(define v (read))
(display v) (newline)
v
]

and provide the input S-expression @racket[(1 2)], the interactions
window ultimately appears as follows:

@racketblock[
@#,ioinputfont{(1 2)}
@#,iooutputfont{(1 2)}
@#,racketresult['(1 2)]
@#,tt{>} @#,tt{_}
]

In this example, @racket[display] produces output immediately beneath
the input you typed, and the final result is printed last. The
displayed output is drawn in purple. (The above example assumes
default printing. With constructor-style value printing, the final
before the prompt would be @racket[(list 1 2)].)

Entering the same program line-by-line in the interactions window
produces a different-looking result:

@racketblock[
@#,tt{>} (define v (read))
@#,ioinputfont{(1 2)}
@#,tt{>} (display v)
@#,iooutputfont{(1 2)}
@#,tt{>} v
@#,racketresult['(1 2)]
@#,tt{>} @#,tt{_}
]

Depending on the input operation, you may enter more text into an
input box than is consumed. In that case, the leftover text remains in
the input stream for later reads. For example, in the following
interaction, two values are provided in response to the first
@racket[(read)], so the second value is returned immediately for the
second @racket[(read)]:

@racketblock[
@#,tt{>} (read)
@#,ioinputfont{5 6}
@#,racketresult[5]
@#,tt{>} (read)
@#,racketresult[6]
@#,tt{>} @#,tt{_}
]

The following example illustrates that submitting input with Return
inserts a newline character into the input stream:

@racketblock[
@#,tt{>} (read)
@#,ioinputfont{5}
@#,racketresult[5]
@#,tt{>} (read-char)
@#,racketresult[#\newline]
@#,tt{>} @#,tt{_}
]

The @onscreen{eof} button that appears beside an input box inserts
a single @racket[eof-object] into the input stream, but more IO may
follow in a later sequence. For example, in the following interaction,
the user typed @litchar{1} and then clicked the @onscreen{eof} button:

@racketblock[
@#,tt{>} (read-char)
@#,ioinputfont{1}@#,racketresult[#\1]
@#,tt{>} (read-char)
@#,racketresultfont{#<eof>}
]

At this point, however, future interactions can still take place:
new calls to input functions with open a new input box and new
characters will come from the same port.

Within a @racket[@#,hash-lang[] @#,racketmodname[racket]] module,
the results of top-level expression print the same as the results of
an expression entered in the @tech{interactions window}. The reason is
that @racket[@#,hash-lang[] @#,racketmodname[racket]] explicitly
prints the results of top-level expressions using
@racket[(current-print)], and DrRacket sets @racket[(current-print)]
to print values in the same way as for interactions.

@; ----------------------------------------------------------------------

@section{Graphical Syntax}

In addition to normal textual program, DrRacket supports certain
graphical elements as expressions within a program. Plug-in tools can
extend the available graphical syntax, but this section describes some
of the more commonly used elements.

@subsection[#:tag "images"]{Images}

DrRacket's @menuitem["Insert" "Insert Image..."] menu item lets you
select an image file from disk (in various formats such as GIF, PNG,
and BMP), and the image is inserted at the current editing caret.

As an expression an image behaves like a number or string constant: it
evaluates to itself. DrRacket's @tech{interactions window} knows how
to draw image-value results or images displayed via @racket[print].

A program can manipulate image values in various ways, such as using
the @racket[htdp/image] library or as an
@racket[image-snip%] value.

@subsection[#:tag "xml-boxes"]{XML Boxes and Racket Boxes}

DrRacket has special support for XML concrete syntax. The
@menuitem["Special" "Insert XML Box"] menu item inserts an embedded
editor into your program. In that embedded editor, you type XML's
concrete syntax. When a program containing an XML box is evaluated,
the XML box is translated into an @deftech{x-expression} (or
@deftech{xexpr}), which is an s-expression representation of an XML
expression. Each xexpr is a list whose first element is a symbol
naming the tag, second element is an association list representing
attributes and remaining elements are the nested XML expressions.

XML boxes have two modes for handling whitespace. In one mode, all
whitespace is left intact in the resulting xexpr.  In the other mode,
any tag that only contains nested XML expressions and whitespace has
the whitespace removed. You can toggle between these modes by
right-clicking or Control-clicking (Mac OS X) on the top portion of
the XML box.

In addition to containing XML text, XML boxes can also
contain Racket boxes. Racket boxes contain Racket
expressions. These expressions are evaluated and their
contents are placed into the containing XML box's xexpr.
There are two varieties of Racket box: the standard Racket
box and the splicing Racket box. The standard Racket box
inserts its value into the containing xexpr. The contents of
the splice box must evaluate to a list and the elements of
the list are ``flattened'' into the containing xexpr.
Right-clicking or control-clicking (Mac OS X) on the top of a Racket
box opens a menu to toggle the box between a Racket box and
a Racket splice box.

@; ----------------------------------------------------------------------

@section[#:tag "debugger"]{Graphical Debugging Interface}

@margin-note{@bold{Tip:} Changing the name of a file in the middle of
a debugging session will prevent the debugger from working properly on
that file.}

Like the @onscreen{Run} button, the @as-index{@onscreen{Debug} button}
runs the program in the definitions window.  However, instead of
simply running it from start to finish, it lets users control and
observe the program as it executes.  The interface includes a panel of
buttons above the definitions window, as well as extensions to the
definitions window itself.

The program starts out paused just before the first expression is
executed.  This is indicated in the definitions window by the presence
of a green triangle over this expression's left parenthesis.

@subsection{Debugger Buttons}

While execution is paused, several buttons are available:

@itemize[

    @item{The @as-index{@onscreen{Go} button} is enabled
whenever the program is paused.  It causes the program to resume
until it either completes, reaches a breakpoint, or raises an
unhandled exception.}

    @item{The @as-index{@onscreen{Step} button} is enabled whenever
the program is paused.  It causes the program to make a single step
and then pause again.}

    @item{The @as-index{@onscreen{Over} button} is only enabled when
execution is paused at the start of an expression that is not in tail
position.  It sets a one-time breakpoint at the end of the
expression (represented by a yellow circle) and causes the program to
proceed.  When execution reaches the one-time breakpoint, it pauses
and removes that breakpoint.}

    @item{The @as-index{@onscreen{Out} button} is only enabled when
execution is paused within the context of another expression.  Like
the @onscreen{Over} button, it sets a one-time breakpoint and
continues execution.  In this case, the program stops upon returning
to the context or raising an unhandled exception.}

]

If the program is running (not paused), then only the @as-index{Pause}
button will be enabled.  Clicking it will interrupt execution and
pause it.  In this case, the current expression may only be known
approximately, and it will be represented as a gray triangle.  The
other features described above will still be available.

At any time, execution may be interrupted by clicking the
@onscreen{Stop} button.  However, unlike with the @onscreen{Pause}
button, stopped execution cannot be continued.

@subsection{Definitions Window Actions}

When execution is paused, the definitions window supports several
additional actions:

@itemize[

    @item{Hovering the mouse cursor over a parenthesis may reveal a
pink circle.  If so, right-clicking or control-clicking (Mac OS X)
will open a menu with options to @onscreen{Pause at this point} or
@onscreen{Continue to this point}.  The former sets an ordinary
breakpoint at that location; the latter sets a one-time breakpoint and
resumes execution.  An ordinary breakpoint appears as a red circle,
and a one-time breakpoint appears as a yellow circle.

@bold{Tip:} If the debugged program is not a module, then the
@italic{first time} it is debugged, breakpoints will only become
available in expressions as they are evaluated.  However, the next
time the program is debugged, the debugger will remember the set of
breakable locations from the previous session.

@bold{Tip:} Clicking the @onscreen{Run} button after a debugging
session will cause all breakpoints to disappear from the definitions
window.  These breakpoints are not forgotten, and clicking
@onscreen{Debug} again will restore them.  However, breakpoints do
@italic{not} persist across restarts of DrRacket.}

    @item{If execution is paused at the start of an expression, then
right-clicking or control-clicking (Mac OS X) on the green triangle
opens a menu with the option to @onscreen{Skip expression...}.
Selecting this opens a text box in which to enter a value for the
expression.  The expression is skipped, with the entered value
substituted for it.}

    @item{If execution is paused at the end of an expression, then the
expression and its value are displayed to the left of the button bar.
Right-clicking or control-clicking (Mac OS X) on the green triangle
opens a menu with options to @onscreen{Print return value to console}
and @onscreen{Change return value...}.  The former displays the return
value in the interactions window; the latter opens a text box in which
to enter a substitute value.}

    @item{Hovering the mouse cursor over a bound variable displays the
variable's name and value to the right of the button bar.
Right-clicking or control-clicking (Mac OS X) opens a menu with
options to @onscreen{Print value of <var> to console} or
@onscreen{(set! <var> ...)}.  The former displays the variable's value
in the interactions window; the latter opens a text box in which to
enter a new value for the variable.}

]

@subsection{Stack View Pane}

In addition, while execution is paused, the stack view pane at the
right of the DrRacket frame is active.  The top of the pane shows a
list of active stack frames.  Mousing over a frame produces a faint
green highlighting of the corresponding expression.  Clicking on the
frame selects that frame, which makes its lexical variables visible.
The selected frame is indicated by a bold font.

The bottom of the pane shows the lexical variables in the selected
stack frame.

The following screenshot illustrates several aspects of the debugger
interface.  The red circle before the @racket[if] is a breakpoint,
and the green triangle at the end of the @racket[(fact (sub1 n))] is where
execution is currently paused.  The expression's return value is
displayed at the left of the button bar, and the value of @racket[n]
is displayed in the stack view pane.

@centerline{@image[#:scale 0.5 "debugger1.png"]}

@subsection{Debugging Multiple Files}

To debug a program that spans several files, make sure that all of the
files are open in DrRacket.  Click the @onscreen{Debug} button in the
window containing the main program.  As this program loads additional
files that are present in other windows or tabs, message boxes will
pop up asking whether or not to include the file in the debugging
session.  Including the file means that it will be possible to set
breakpoints, inspect variables, and single-step in that file.

@bold{Tip:} A file may only be involved in one debugging session at a
time.  If you try to debug a file that loads another file that is
already being debugged, a message box will pop up explaining that the
file cannot be included in another debugging session.

@; ----------------------------------------------------------------------

@section[#:tag "module-browser"]{The Module Browser}

The module browser shows you the structure of all of the files in your program.
It can be opened via the @onscreen{Show} menu, or via the 
@onscreen{Module Browser} 
menu items in the @onscreen{Racket} menu.

A module browser window contains a square for each
  module. The squares are colored based on the number of
  lines of code in the module. If a module has more lines of
  code, it gets a darker color. If a module is red, it means
  that DrRacket did not find a source file for it.
  
  In addition, for each normal import, a blue line drawn is
  from the module to the importing module. Similarly, purple
  lines are drawn for each for-syntax, for-template or for-meta import. In the initial
  module layout, modules to the left import modules to the
  right, but since modules can be moved around
  interactively, that property might not be preserved.

  To open the file corresponding to the module, double click
  on the box for that module.
  
  The module browser will also show you the phases that each
  module is loaded in; choose the ``Long, with phases'' menu item
  in the ``Names'' pop-up menu. The integers indicate the phases and
  if @racket[#f] is present, it means the module is loaded @racket[for-label].
  
  The bar along the bottom helps you find your way in a module graph. Specifically,
  if you type something there, then all of the modules whose filenames match
  what you type will turn green in the module window. This bar is only visible
  in the stand alone module browser window (via the @onscreen{Racket} menu)

@section[#:tag "color-scheme"]{Color Schemes}

DrRacket comes with three different color schemes, available in the preferences dialog's
@onscreen{color} panel.

You can add your own color schemes to DrRacket, too. The first step is to
create a pkg (see @secref["how-to-create" #:doc '(lib "pkg/scribblings/pkg.scrbl")])
and add an @filepath{info.rkt} file to it. The file should define
@racket[framework:color-schemes] as a list of hashes that describe the color schemes.

@(define example-key #f)

As an example, this is the specification of the @racket["Modern"] style:
@(let ()
   (define pth (collection-file-path "info.rkt" "drracket"))
   (define-values (base name dir?) (split-path pth))
   (define info (get-info/full base))
   (unless info (error 'framework/main.rkt "could not find example for modern color scheme"))
   (define key 'framework:color-schemes)
   (define datum (info key))
   (define name-as-string-datum
     (let loop ([datum datum])
       (cond
         [(list? datum)
          (for/list ([datum (in-list datum)])
            (loop datum))]
         [(hash? datum)
          (for/hash ([(k v) (in-hash datum)])
            (if (and (equal? k 'name) (string-constant? v))
                (values k (dynamic-string-constant v))
                (values k (loop v))))]
         [(and (symbol? datum)
               (regexp-match #rx"framework:" (symbol->string datum)))
          (unless example-key (set! example-key datum))
          datum]
         [else datum])))
   (define sp (open-output-string))
   (parameterize ([pretty-print-columns 60]
                  [current-output-port sp])
     (pretty-write
      `(define ,key 
         ',name-as-string-datum)))
   (codeblock 
    (string-append "#lang info\n"
                   (get-output-string sp))))

Each of the keys, e.g., @code[(format "~s" `',example-key)], maps to a color and possibly to
some style information. All keys accept colors (the vectors shown
above represent colors in r/g/b format), but only some accept style information. To
find out which are which and to get a complete list of the possible keys, click the button
labeled @onscreen[(regexp-replace #rx"&&" (string-constant style-and-color-names) "&")]
at the bottom of the 
@onscreen[(string-constant color-schemes)] tab of the
@onscreen[(string-constant preferences-colors)] tab in the preferences dialog.
If one can accept style information, then you may include any of the symbols @racket['bold],
@racket['underline], or @racket['italic] in the list with the color.

Full details on the specification of the info files can be found in the documentation
for the function @racket[color-prefs:register-info-based-color-schemes].

You may have to restart DrRacket (and, at least the first time after you add the @filepath{info.rkt}
file, re-run @tt{raco setup}) to see changes to your color scheme.

Color schemes are not limited only to the colors that DrRacket already knows about.
If you are adding your own plugin to DrRacket, you can add new names that can be 
mapped in the color scheme. See @racket[color-prefs:register-color-preference] for
more information.

@section[#:tag "create-exe"]{Creating Executables}

DrRacket's @onscreen{Create Executable...} menu item lets you create
an executable for your program that you can start without first
starting DrRacket. To create an executable, first save your program to
a file and set the language and teachpacks.  Click @onscreen{Run},
just to make sure that the program is working as you expect. The
executable you create will not have a read-eval-print-loop, so be sure
to have an expression that starts your program running in the
definitions window before creating the executable.

Once you are satisfied with your program, choose the @onscreen{Create
Executable...}  menu item from the @onscreen{Racket} menu. You will be
asked to choose an executable file name or an archive file name. In
the latter case, unpack the generated archive (on this machine or
another one) to access the executable. In either case, you will be
able to start the executable in the same way that you start any other
program on your computer.

The result of @onscreen{Create Executable...} is either a
@defterm{launcher executable}, a @defterm{stand-alone executable}, or
a @defterm{distribution archive}, and it uses either a
@defterm{Racket} (textual) or @defterm{GRacket} (graphical) engine.
For programs implemented with certain languages, @onscreen{Create
Executable...}  will prompt you to choose the executable type and
engine, while other languages support only one type or engine.

Each type has advantages and disadvantages:

@itemize[

  @item{A @deftech{launcher executable} uses the latest version of
  your program source file when it starts. It also accesses library
  files from your DrRacket installation when it runs. Since a launcher
  executable contains specific paths to access those files, launchers
  usually cannot be moved from one machine to another.}

 @item{A @deftech{stand-alone executable} embeds a compiled copy of
 your program and any Racket libraries that your program uses. When
 the executable is started, it uses the embedded copies and does not
 need your original source file. It may, however, access your DrRacket
 installation for DLLs, frameworks, shared libraries, or helper
 executables. Consequently, a stand-alone executable usually cannot be
 moved from one machine to another.}

 @item{A @deftech{distribution archive} packages a stand-alone
 executable together with any needed DLLs, frameworks, shared
 libraries, and helper executables. A distribution archive can be
 unpacked and run on any machine with the same operating system as
 yours.}

]

In general, DrRacket's gives you the most options when it infers a
language from a program's source. Most other languages only allow one
type of executable. The teaching languages, for example, create
stand-alone executables in distributions. The legacy languages create
only launchers.

@bold{Tip:} Disable debugging in the language dialog before creating
your executable. With debugging enabled, you will see a stack trace
with error messages, but your program will run more slowly.  To
disable debugging, open the language dialog, click the @onscreen{Show
Details} button, and select @onscreen{No debugging or profiling}, if
it is available.

When you create an executable in some languages, you can supply
additional files to determine the executable's icon and similar
properties, depending on the platform. The file's purpose is
determined by its suffix:

@itemlist[

 @item{On Windows, supply an @filepath{.ico} file for an icon. Only
       16x16, 32x32, or 48x48 images from the @filepath{.ico} file are
       used.}

 @item{On Mac OS X, supply an @filepath{.icns} file for an icon.  You
       can set the application's creator with an @filepath{.creator}
       file (whose first four bytes are used), and you can set
       documents for the application through a @filepath{.utiexports}
       file (see @racket['uti-exports] in
       @racket[create-embedding-executable] for more information).}

 @item{On Unix, supply a @filepath{.png} or @filepath{.ico} file for
       an icon.}

]

@section[#:tag "follow-log"]{Following Log Messages}

The @onscreen{Show Log} menu item in the @onscreen{View} menu opens
a pane in the DrRacket window showing log messages. 

Along the top of the window is a text field that should be filled with
a description of which log messages are interesting, as described in
the @secref[#:doc '(lib "scribblings/reference/reference.scrbl") "logging"]
section of @other-doc['(lib "scribblings/reference/reference.scrbl")].

