#lang scribble/doc
@(require "common.ss"
          scribble/decode
          scribble/eval
          scribble/struct
          (for-label htdp/convert))

@(define (ioinputfont . s)
   (apply tt s))
@(define (iooutputfont . s)
   (make-element "schemestdout" (decode-content s)))

@title[#:tag "interface-essentials"]{Interface Essentials}

The DrScheme window has three parts: a row of buttons at the top, two
editing panels in the middle, and a status line at the bottom.

@image["screen-shot.png"]

The top editing panel, called the @deftech{definitions window}, is for
defining programs. The above figure shows a program that defines the
function @scheme[square].

The bottom panel, called the @deftech{interactions window}, is for
evaluating Scheme expressions interactively. The @onscreen{Language} line
in the interactions window indicates which primitives are available in
the definitions and interactions windows.  In the above figure, the
language is @drlang{Module}.

@margin-note{The interactions window is described further in
@secref["interactions-window"], later in this manual.}

Clicking the @onscreen{Run} button evaluates the program in the
definitions window, making the program's definitions available in the
interactions window. Given the definition of @scheme[square] as in the
figure above, typing @scheme[(square 2)] in the interactions window
produces the result @scheme[4].

The @deftech{status line} at the bottom of DrScheme's window provides
information about the current line and position of the editing caret,
whether the current file can be modified, and whether DrScheme is
currently evaluating any expression. The @as-index{recycling icon}
flashes while DrScheme is ``recycling'' internal resources, such as
memory.

@; ----------------------------------------------------------------------

@section{Buttons}

The left end of the row of buttons in DrScheme contains a miniature
button with the @index['("filename button")]{current file's
name}. Clicking the button opens a menu that shows the file's full
pathname. Selecting one of the menu entries produces an open-file
dialog starting in the corresponding directory.

Below the filename button is a @as-index{@onscreen{(define ...)}
button} for a popup menu of names that are defined in the definitions
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
DrScheme. For example, a step might replace @scheme[(+ 1 2)] with
@scheme[3]. These are the same rules used by DrScheme to evaluate a
program.  Clicking @onscreen{Step} opens a new window that contains
the program from the definitions window, plus several new buttons:
these buttons allow navigation of the evaluation as a series of steps.

The @as-index{@onscreen{Debug} button}---which does @emph{not} appear
for the @|HtDP| teaching languages---starts a more conventional
stepping @as-index{debugger}. It runs the program in the definitions
window like the @onscreen{Run} button, but also opens a debugging
panel with @onscreen{Pause}, @onscreen{Continue}, and @onscreen{Step}
buttons. A newly started program is paused the program's first
possible pause point, and the current pause location is shown with a
green arrow. Click the @onscreen{Continue} button to continue running
the program, click @onscreen{Step} to run until the next possible
pause point, and right-click on an expression's open or close
parenthesis to set or remove an explicit pause. (Valid pause points
are highlighted with a pink dot as you mouse over the program text.)
When the program is paused, move the mouse over a variable to display
its value in the debugging panel to the right of the buttons. When
pausing at an expression's result, the result is shown to the left of
the debugging panel's buttons, and the result can be changed by
right-clicking the pause point. Click the @onscreen{Stop} button to
stop debugging so that the program in the definitions window can be
edited. Debugging also stops when all expressions in the definition
window have been evaluated.

Clicking the @as-index{@onscreen{Check Syntax} button} annotates the
program text in the definitions window.  It add the following
annotations:

@itemize{

 @item{@bold{Syntactic Highlighting:} Imported variables and locally
        defined variables are highlighted with color
        changes. Documented identifiers are hyperlinked (via a
        right-click) to the documentation page.}

 @item{@bold{Lexical Structure:} The lexical structure is shown with
       arrows overlaid on the program text.  When the mouse cursor
       passes over a variable, DrScheme draws an arrow from the
       binding location to the variable, or from the binding location
       to every bound occurrence of the variable.

       @index['("Check syntax" "purple arrows")]{@index['("Check
       syntax" "question-mark arrows")]{In}} addition to indicating
       definite references with blue arrows, DrScheme also draws
       arrows to indicate potential references within macro
       definitions. Potential arrows are drawn in purple and annotated
       with a question mark to indicate uncertainty, because DrScheme
       cannot predict how such identifiers will eventually be
       used. Their roles may depend on the arguments to the macro and
       the context the macro is used in.

       @index['("alpha renaming")]{Additionally}, right-clicking (or
       Control-clicking under Mac OS X) on a variable activates a
       popup menu that lets you jump from binding location to bound
       location and vice-versa, @as-index{@"\u03B1"-rename} the
       variable, or tack the arrows so they do not disappear.}

 @item{@index['("tail calls")]{@bold{Tail Calls:}} Any
       sub-expression that is (syntactically) in tail-position with
       respect to its enclosing context is annotated by drawing a
       light purple arrow from the tail expression to its surrounding
       expression.}

  @item{@bold{Require Annotations:} Right-clicking (or
        Control-clicking under Mac OS X) on the argument to
        @scheme[require] activates a popup menu that lets you open the
        file that contains the @scheme[require]d module.

        Passing the mouse cursor over a @scheme[require] expression
        inside a module shows all of the variables that are used from
        that @scheme[require] expression. Additionally, if no
        variables are used from that require expression, it is colored
        like an unbound variable.

         Finally, passing the mouse cursor over a variable that is
         imported from a module shows the module that it is imported
         from in a status line at the bottom of the frame.}

}

The @as-index{@onscreen{Run} button} evaluates the program in the
@tech{definitions window} and resets the @tech{interactions window}.

The @as-index{@onscreen{Break} button} interrupts an evaluation, or
beeps if DrScheme is not evaluating anything. For example, after
clicking @onscreen{Run} or entering an expression into the
interactions window, click @onscreen{Break} to cancel the
evaluation. Click the @onscreen{Break} button once to try to interrupt
the evaluation gracefully; click the button twice to kill the
evaluation immediately.

@; ----------------------------------------------------------------------

@section[#:tag "choose-language"]{Choosing a Language}

@section-index["language levels"]

DrScheme supports multiple dialects of Scheme, as well as some
non-Scheme languages. You specify a language in one of two ways:

@itemize{

 @item{Select the @menuitem["Language" "Choose Language..."] menu
       item, and choose a language other than @drlang{Module}. After
       changing the language, click @onscreen{Run} to reset the
       language in the interactions window. The bottom-left corner of
       DrScheme's main window also has a shortcut menu item for
       selecting previously selected languages.}

 @item{Select the @drlang{Module} language (via the
       @menuitem["Language" "Choose Language..."] menu item), and then
       specify a specific language as part of the program usually by
       starting the definitions-window content with @hash-lang[].}

}

The latter method, @drlang{Module} with @hash-lang[], is the recommend
mode, and it is described further in @secref["module"].

The @menuitem["Language" "Choose Language..."] dialog contains a
@onscreen{Show Details} button for configuring certain details of the
chosen language. Whenever the selected options do not match the
default language specification, a @onscreen{Custom} indicator appears
next to the language-selection control at the top of the dialog.

See @secref["languages"] (later in this manual) for more information
on the languages that DrScheme supports.

@; ----------------------------------------------------------------------

@section[#:tag "editor"]{The Editor}

@index['("flashing parenthesis matches")]{@index['("gray highlight
regions")]{In}} Scheme mode, especially, DrScheme's editor provides
special support for managing parentheses in a program. When the
blinking caret is next to a parenthesis, DrScheme shades the region
between the parenthesis and its matching parenthesis. This feature is
especially helpful when for balancing parentheses to complete an
expression.

@index['("formatting Scheme code")]{@index['("indenting Scheme
code")]{Although}} whitespace is not significant in Scheme, DrScheme
encourages a particular format for Scheme code. When you type Enter or
Return, the editor inserts a new line and automatically indents it. To
make DrScheme re-indent an existing line, move the blinking caret to
the line and hit the Tab key. (The caret can be anywhere in the line.)
You can re-indent an entire region by selecting the region and typing
Tab.

@index['("changing a parenthesis as you type")]{@index['("automatic
parenthesis")]{DrScheme}} also rewrites parenthesis as you type them,
in order to make them match better. If you type a closing parenthesis
@litchar{)}, a closing square bracket @litchar{]}, or a closing curley brace
@litchar["}"], and if DrScheme can match it back to some earlier opening
parenthesis, bracket, or brace, then DrScheme changes what you type to
match.  DrScheme also rewrites open square brackets, usually to an
open parenthesis. There are some exceptions where opening square
brackets are not automatically changed to parentheses:

@itemize{

 @item{If the square bracket is after @scheme[cond]-like keyword,
       potentially skipping some of the sub-expressions in the
       @scheme[cond]-like expression (for example, in a @scheme[case]
       expression, the square brackets start in the second
       sub-expression).}

 @item{If the square bracket begins a new expression immediately after
       a @scheme[local]-like keyword. Note that the second expression
       after a @scheme[local]-like keyword will automatically become
       an ordinary parenthesis.}

 @item{If the square bracket is after a parenthesis that is after a
       @scheme[letrec]-like keyword,}

 @item{If the square bracket is in a sequence and the s-expression
      before in the sequence is a compound expression, DrScheme uses
      the same kind parenthesis, brace, or bracket as before, or}

 @item{If the square bracket is in the middle of string,
       comment, character, or symbol.}
}

The upshot of DrScheme's help is that you can always use the
(presumably unshifted) square brackets on your keyboard to type
parenthesis. For example, when typing

@schemeblock[
(define (length l)
  (cond
   [(empty? l) 0]
   [else (+ 1 (length (rest l)))]))
]

If you always type @litchar{[} and @litchar{]} where any of the square
brackets or parentheses appear, DrScheme will change the square
brackets to match the code above.

Of course, these features can be disabled and customized in the
preferences dialog; see @secref["prefs-explanation"].  Also, in case
DrScheme does not produce the character you want, holding down the
control key while typing disables DrScheme's parenthesis, brace, and
bracket converter.
 
@; -------------------------------

@subsection{Tabbed Editing}

DrScheme's allows you to edit multiple files in a single window via
tabs. The @menuitem["File" "New Tab"] menu item creates a new tab to
show a new file. Each tab has its own interactions window.

In the @onscreen{General} sub-pane of the @onscreen{Editing} pane in
the preferences window, a checkbox labelled @onscreen{Open files in
separate tabs} causes DrScheme to open files in new tabs in the
frontmost window, rather than opening a new window for the file.

The key bindings Control-Pageup and Control-Pagedown move between
tabs. Under Mac OS X, Command-Shift-Left and Command-Shift-Right also
move between tabs.

@; ----------------------------------------------------------------------

@section[#:tag "interactions-window"]{The Interactions Window}

@index['("> prompt")]{@index['("evaluating expressions")]{The}}
interactions window lets you type an expression after the @tt{>}
prompt for immediate evaluation. You cannot modify any text before the
last @tt{>} prompt. To enter an expression, the blinking caret must
appear after the last prompt, and also after the space following the
prompt.

When you type a complete expression and hit Enter or Return, DrScheme
evaluates the expression and prints the result. After printing the
result, DrScheme creates a new prompt for another expression. Some
expressions return a special ``void'' value; DrScheme never prints
void, but instead produces a new prompt immediately.

If the expression following the current prompt is incomplete, then
DrScheme will not try to evaluate it. In that case, hitting Enter or
Return produces a new, auto-indented line. You can force DrScheme to
evaluate the expression by typing Alt-Return or Command-Return
(depending on your platform).

To copy the @as-index{previous expression} to the current prompt, type
ESC-p (i.e., type Escape and then type p). Type ESC-p multiple times
to @as-index{cycle back through old expressions}. Type ESC-n to cycle
forward through old expressions.

Clicking the @onscreen{Run} button evaluates the program in the
@tech{definitions window} and makes the program's definitions
available in the interactions window. Clicking @onscreen{Run} also
resets the interactions window, erasing all old interactions and
removing old definitions from the interaction environment. Although
@onscreen{Run} erases old @tt{>} prompts, ESC-p and ESC-n can still
retrieve old expressions.

@; ----------------------------------------

@subsection{Errors}

@index['("error highlighting")]{Whenever} DrScheme encounters an error
while evaluating an expression, it prints an error message in the
interactions window and highlights the expression that triggered the
error. The highlighted expression might be in the definitions window,
or it might be after an old prompt in the interactions window.

For certain kinds of errors, DrScheme turns a portion of the error
message into a hyperlink. Click the hyperlink to get help regarding a
function or keyword related to the error.

For some run-time errors, DrScheme shows a bug icon next to the error
message. Click the bug icon to open a window that shows a ``stack'' of
expressions that were being evaluated at the time of the error. In
addition, if the expressions in the stack appear in the
@tech{definitions window}, a red arrow is drawn to each expression
from the next deeper one in the stack.

@; ----------------------------------------

@subsection{Input and Output}

@section-index["I/O"]

Many Scheme programs avoid explicit input and output operations,
obtaining input via direct function calls in the @tech{interactions
window}, and producing output by returning values. Other Scheme
programs explicitly print output for the user during evaluation using
@as-index{@scheme[write]} or @as-index{@scheme[display]}, or
explicitly request input from the user using @as-index{@scheme[read]}
or @as-index{@scheme[read-char]}.

Explicit input and output appear in the @tech{interactions window},
but within special boxes that separate explicit I/O from normal
expressions and results. For example, evaluating

@schemeblock[
#, @tt{>} (read)
]

in the interactions window produces a special box for entering input:

@image["io.png"]

Type a number into the box and hit Enter, and that number becomes the
result of the @scheme[(read)] expression. Once text is submitted for
an input box, it is moved outside the input box, and when DrScheme
shows a new prompt, it hides the interaction box. Thus, if you type
@scheme[5] in the above input box and hit Return, the result appears
as follows:

@schemeblock[
#, @tt{>} (read)
#, @ioinputfont{5}
#, @schemeresult[5]
#, @tt{>} #, @tt{_}
]

In this case, the first @ioinputfont{5} is the input, and the second
@schemeresult[5] is the result of the @scheme[(read)] expression. The
second @schemeresult[5] is colored blue, as usual for a result printed
by DrScheme. (The underscore indicates the location of the blinking
caret.)

Output goes into the interaction window directly. If you run the program

@schememod[
scheme
(define v (read))
(display v)
]

and provide the input S-expression @scheme[(1 2)], the interactions
window ultimately appears as follows:

@schemeblock[
#, @ioinputfont{(1 2)}
#, @iooutputfont{(1 2)}
#, @schemeresult[(1 2)]
#, @tt{>} #, @tt{_}
]

In this example, @scheme[display] produces output immediately beneath
the input you typed, and the final result is printed last. The
displayed output is drawn in purple. (The above example assumes
default printing. With constructor-style value printing, the final
before the prompt would be @scheme[(list 1 2)].)

Entering the same program line-by-line in the interactions window
produces a different-looking result:

@schemeblock[
#, @tt{>} (define v (read))
#, @ioinputfont{(1 2)}
#, @tt{>} (display v)
#, @iooutputfont{(1 2)}
#, @tt{>} v
#, @schemeresult[(1 2)]
#, @tt{>} #, @tt{_}
]

Depending on the input operation, you may enter more text into an
input box than is consumed. In that case, the leftover text remains in
the input stream for later reads. For example, in the following
interaction, two values are provided in response to the first
@scheme[(read)], so the second value is returned immediately for the
second @scheme[(read)]:

@schemeblock[
#, @tt{>} (read)
#, @ioinputfont{5 6}
#, @iooutputfont{5}
#, @tt{>} (read)
#, @iooutputfont{6}
#, @tt{>} #, @tt{_}
]

The following example illustrates that submitting input with Return
inserts a newline character into the input stream:

@schemeblock[
#, @tt{>} (read)
#, @ioinputfont{5}

#, @iooutputfont{5}
#, @tt{>} (read-char)
#, @schemeresult[#\newline]
#, @tt{>} #, @tt{_}
]

@; ----------------------------------------------------------------------

@section{Creating Executables}

DrScheme's @onscreen{Create Executable...} menu item lets you create
an executable for your program that you can start without first
starting DrScheme. To create an executable, first save your program to
a file and set the language and teachpacks.  Click @onscreen{Run},
just to make sure that the program is working as you expect. The
executable you create will not have a read-eval-print-loop, so be sure
to have an expression that starts your program running in the
definitions window before creating the executable.

Once you are satisfied with your program, choose the @onscreen{Create
Executable...}  menu item from the @onscreen{Scheme} menu. You will be
asked to choose an executable file name or an archive file name. In
the latter case, unpack the generated archive (on this machine or
another one) to access the executable. In either case, you will be
able to start the executable in the same way that you start any other
program on your computer.

The result of @onscreen{Create Executable...} is either a
@defterm{launcher executable}, a @defterm{stand-alone executable}, or
a @defterm{distribution archive}, and it uses either a
@defterm{MzScheme} (textual) or @defterm{MrEd} (graphical) engine.
For programs implemented with certain languages, @onscreen{Create
Executable...}  will prompt you to choose the executable type and
engine, while other languages support only one type or engine.

Each type has advantages and disadvantages:

@itemize{

  @item{A @deftech{launcher executable} uses the latest version of
  your program source file when it starts. It also accesses library
  files from your DrScheme installation when it runs. Since a launcher
  executable contains specific paths to access those files, launchers
  usually cannot be moved from one machine to another.}

 @item{A @deftech{stand-alone executable} embeds a compiled copy of
 your program and any Scheme libraries that your program uses. When
 the executable is started, it uses the embedded copies and does not
 need your original source file. It may, however, access your DrScheme
 installation for DLLs, frameworks, shared libraries, or helper
 executables. Consequently, a stand-alone executable usually cannot be
 moved from one machine to another.}

 @item{A @deftech{distribution archive} packages a stand-alone
 executable together with any needed DLLs, frameworks, shared
 libraries, and helper executables. A distribution archive can be
 unpacked and run on any machine with the same operating system as
 yours.}

}

In general, DrScheme's @drlang{Module} language gives you the most
options. Most other languages only allow one type of executable. The
teaching langauges create stand-alone executables in
distributions. The lagacy languages create launchers.

@bold{Tip:} Disable debugging in the language dialog before creating
your executable. With debugging enabled, you will see a stack trace
with error messages, but your program will run more slowly.  To
disable debugging, open the language dialog, click the @onscreen{Show
Details} button, and select @onscreen{No debugging or profiling}, if
it is available.

@;{

@section[#:tag "drscheme:sec:printing"]{Printed Results}

\index{output format}\index{printing format}\index{read-eval-print loop}

This section describes the different formats that DrScheme
uses for printing results in the interactions window. Each
of the different settings here also apply to the
@scheme[print] primitive. That is, printing in the
interactions window is identical to output produced by the
@scheme[print] primitive.

@subsection[#:tag "drscheme:sec:printing:cons"]{Constructor-style Output}

\index{constructor-style output} 
%
{\drscheme}'s @defterm{constructor-style output} treats @scheme[cons],
 @scheme[vector], and similar primitives as value constructors, rather
 than functions. It also treats @scheme[list] as shorthand for multiple
 @scheme[cons]'s ending with the empty list.  Constructor-style printing
 is valuable for beginning computer science students, because output
 values look the same as input values.

Results printed in DrScheme's interactions window using
 constructor-style printing look different than results printed in
 traditional Scheme implementations, which use @scheme[write] to print
 results. The table in Figure~\ref{fig:constructor-printing} shows the
 differences between values printed in constructor style and values
 printed with @scheme[write].

\begin{figure}
\begin{center}
\input{drs-constructor-style-examples.tex}
\end{center}
\caption{Comparison of constructor-style output to @scheme[write]}\label{fig:constructor-printing}
\end{figure}

@subsection[#:tag "drscheme:sec:printing:quasi"]{Quasiquote-style Output}

\index{quasiquote-style output} 
%
Constructor-style output is inconvenient for printing S-expression
 results that represent programs. For example, the value @scheme['(lambda
 (x) (lambda (y) (+ x y)))] prints as
%
\begin{center}
@scheme[(list 'lambda (list 'x) (list 'lambda (list 'y) (list '+ 'x 'y)))] 
\end{center}
%
with constructor-style printing. 

DrScheme's @defterm{quasiquote-style output} combines the
 input--output invariance of constructor-style printing with the
 S-expression readability of @scheme[write]. It uses @scheme[quasiquote] to
 print lists, and uses @scheme[unquote] to escape back to constructor
 style printing for non-lists and non-symbols.

With quasiquote-style printing, the above example prints as:
%
\begin{center}
@scheme[`(lambda (x) (lambda (y) (+ x y)))] 
\end{center}

This example:
\begin{center}
@scheme[(list 'lambda (list 'x) (box '(lambda (y) (+ x y))))] 
\end{center}
in quasiquote-style printing prints as:
\begin{center}
@scheme[`(lambda (x) ,(box `(lambda (y) (+ x y))))]
\end{center}


@section{XML}
\label{drscheme:sec:xml}

\index{XML}
%
DrScheme has special support for XML concrete syntax. The
@onscreen{Special} menu's @onscreen{Insert XML Box} menu inserts an
embedded editor into your program. In that embedded editor,
you type XML's concrete syntax. When a program containing an
XML box is evaluated, the XML box is translated into an
x-expression (or xexpr). Xexprs are s-expression
representation for XML expressions. Each xexpr is a list
whose first element is a symbol naming the tag, second
element is an association list representing attributes and
remaining elements are the nested XML expressions.

XML boxes have two modes for handling whitespace. In one
mode, all whitespace is left intact in the resulting xexpr.
In the other mode, any tag that only contains nested XML
expressions and whitespace has the whitespace removed. You
can toggle between these modes by right-clicking or
control-clicking (Mac OS X) on the top portion of the XML box.

In addition to containing XML text, XML boxes can also
contain Scheme boxes. Scheme boxes contain Scheme
expressions. These expressions are evaluated and their
contents are placed into the containing XML box's xexpr.
There are two varieties of Scheme box: the standard Scheme
box and the splicing Scheme box. The standard Scheme box
inserts its value into the containing xexpr. The contents of
the splice box must evaluate to a list and the elements of
the list are ``flattened'' into the containing xexpr.
Right-clicking or control-clicking (Mac OS X) on the top of a Scheme
box opens a menu to toggle the box between a Scheme box and
a Scheme splice box.

}