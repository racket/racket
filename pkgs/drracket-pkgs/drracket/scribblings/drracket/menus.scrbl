#lang scribble/doc
@(require "common.rkt" scribble/struct)

@(define (defmenuitem . s)
  (let ([mi (apply onscreen s)])
    @index*[(list (string-append (element->string mi) " menu item"))
            (list (elem mi " menu item"))]{@|mi| :}))

@(define lam-str "\u03BB")

@title{Menus}

@section[#:tag "menu:file"]{@onscreen{File}}

@itemize[
  
 @item{@defmenuitem{New} Creates a new DrRacket window.}

 @item{@defmenuitem{New Tab} Creates a new tab in the current DrRacket window.}

 @item{@defmenuitem{Open...} Opens a find-file dialog for choosing
  a file to load into a @tech{definitions window}.}
  
 @item{@defmenuitem{Open Recent} Lists recently opened
   files. Choosing one of them opens that file for editing.}
  
 @item{@defmenuitem{Open Require Path...} Opens a dialog where you
        can enter in a @racket[require]-like module path (e.g.,
        @litchar{racket/base.rkt} or @litchar{data/splay-tree.rkt}
        or @litchar{"x.rkt"})
        and edit the corresponding files in the @tech{definitions window}.}
 
 @item{@defmenuitem{Install PLT File...} Opens a dialog asking for the
   location of the @filepath{.plt} file (either on the local disk or
   on the web) and installs the contents of the file.}

 @item{@defmenuitem{Revert} Re-loads the file that is currently in the
  @tech{definitions window}. All changes since the file was last saved
  will be lost.}
  
 @item{@defmenuitem{Save Definitions} Saves the program in the
  @tech{definitions window}. If the program has never been saved
  before, a save-file dialog appears.}
  
 @item{@defmenuitem{Save Definitions As...} Opens a save-file dialog for
  choosing a destination file to save the program in the definitions
  window. Subsequent saves write to the newly-selected file.}
  
 @item{@defmenuitem{Save Other} Contains these sub-items
 @itemize[

 @item{@defmenuitem{Save Definitions As Text...} Like @onscreen{Save
  Definitions As...}, but the file is saved in plain-text format (see
  @secref["drracket-file-formats"]). Subsequent saves also write in
  plain-text format.}
  
 @item{@defmenuitem{Save Interactions} Saves the contents of the interactions
  window to a file. If the interaction constants have never been saved
  before, a save-file dialog appears.}
  
 @item{@defmenuitem{Save Interactions As...}  Opens a save-file dialog for
  choosing a destination file to save the contents of the interactions
  window. Subsequent saves write to the newly-selected file.}
  
 @item{@defmenuitem{Save Interactions As Text...}  Like @onscreen{Save
  Interactions As...}, but the file is saved in plain-text format (see
  @secref["drracket-file-formats"]). Subsequent saves are write in
  plain-text format.}
  
 ]}
 
@item{@defmenuitem{Log Definitions and Interactions...} Starts a
  running of log of the text in the interactions and definitions
  windows, organized by executions. In a directory of your choosing,
  DrRacket saves files with the names @filepath{01-definitions},
  @filepath{01-interactions}, @filepath{02-definitions},
  @filepath{02-interactions}, @|etc| as you interact with various
  programs.}

 @item{@defmenuitem{Print Definitions...} Opens a dialog for printing
  the current program in the @tech{definitions window}.}

 @item{@defmenuitem{Print Interactions...} Opens a dialog for printing the
  contents of the @tech{interactions window}.}

@item{@defmenuitem{Search in Files...} Opens a dialog where you can
  specify the parameters of a multi-file search. The results of the
  search are displayed in a separate window.}

 @item{@defmenuitem{Close} Closes this DrRacket window. If this window
  is the only open DrRacket window, then DrRacket quits, except on
  Mac OS X.}

 @item{{@onscreen{Quit} or @onscreen{Exit}} Exits DrRacket. (On Mac
  OS X, this menu item is in the Apple menu.)}

]

@; ----------------------------------------

@section{@onscreen{Edit}}

All @onscreen{Edit} menu items operate on either the definitions or
interactions window, depending on the location of the selection or
blinking caret. Each window maintains its own Undo and Redo history.

@itemize[

 @item{@defmenuitem{Undo} Reverses an editing action. Each window
  maintains a history of actions, so multiple @onscreen{Undo}
  operations can reverse multiple editing actions.}

 @item{@defmenuitem{Redo} Reverses an @onscreen{Undo} action. Each
  window (and boxed-subwindow) maintains its own history of
  @onscreen{Undo} actions, so multiple @onscreen{Redo} operations can
  reverse multiple @onscreen{Undo} actions.}

 @item{@defmenuitem{Cut} Copies the selected text to the clipboard and
  deletes it from the window.}

 @item{@defmenuitem{Copy} Copies the selected text to the clipboard.}

 @item{@defmenuitem{Paste} Pastes the current clipboard contents into the
  window.}

 @item{@defmenuitem{Delete} or @defmenuitem{Clear} Deletes the selected text.}

 @item{@defmenuitem{Select All} Highlights the entire text of the buffer.}

  @item{@defmenuitem{Wrap Text} Toggles between wrapped text and
  unwrapped text in the window.}

 @item{@defmenuitem{Find} Opens an interactive search
 window at the bottom of the frame and moves the insertion
 point to the search string editor (or out of it, if the
 insertion point is already there).

 See also @secref["Searching"].}

 @item{@defmenuitem{Find From Selection}
        Just like @onscreen{Find}, except that it
        copies the current selection into the search
        window (but without using the clipboard, so
        paste will paste whatever was last copied,
        not the new search string)}

 @item{@defmenuitem{Find Again} Finds the next occurrence of the text 
 in the search window.}
  
 @item{@defmenuitem{Find Again Backwards} Finds the next occurrence of the text 
 in the search window, but searching backwards.}
  
@item{@defmenuitem{Replace & Find Again} Replaces the selection with the
  replace string (if it matches the find string) and finds the next
  occurrence of the text that was last searched for, looking forwards.}

@item{@defmenuitem{Replace & Find Again Backwards} Replaces the selection with the
  replace string (if it matches the find string) and finds the next
  occurrence of the text that was last searched for, looking backwards.}

@item{@defmenuitem{Replace All} Replaces all occurrences of
the search string with the replace string.}

@item{@defmenuitem{Find Case Sensitive} Toggles between
case-sensitive and case-insensitive search.}
  
@item{@defmenuitem{Keybindings} 
@itemize[

@item{@defmenuitem{Show Active Keybindings} Shows all of the
  keybindings available in the current window.}

@item{@defmenuitem{Add User-defined Keybindings...} Choosing this menu
  item opens a file dialog where you can select a file containing
  Racket-definitions of keybindings. See @secref["defining-shortcuts"]
  for more information.}

]}

@item{@defmenuitem{Spell Check String Constants} Uses @tt{aspell}
       or @tt{ispell} to look for unknown words in string constants
       (things that would otherwise be colored green).}

@item{@defmenuitem{Complete Word} Completes the word at the
insertion point, using the manuals as a source of completions.}

 @item{@defmenuitem{Preferences...} Opens the preferences dialog. See
  @secref["prefs-explanation"]. (On Mac OS X, this menu item is in
  the Apple menu.)}  ]

@; ----------------------------------------

@section[#:tag "menu:view"]{@onscreen{View}}

One each of the following show/hide pairs of menu items
appears at any time.

@itemize[

 @item{@defmenuitem{Toolbar} 
        @itemize[
          @item{@defmenuitem{Toolbar on Left} Moves the tool bar (on the top of DrRacket's window by default) to the left-hand side, organized vertically.}
          @item{@defmenuitem{Toolbar on Top} Moves the toolbar to the top of the DrRacket window.}
          @item{@defmenuitem{Toolbar on Right} Moves the tool bar to the right-hand side, organized vertically.}
          @item{@defmenuitem{Toolbar Hidden} Hides the toolbar entirely.}]}

 @item{@defmenuitem{Split} Splits the current window in half to
  allow for two different portions of the current window to
  be visible simultaneously.}

 @item{@defmenuitem{Collapse} If the window has been split before, this
   menu item becomes enabled, allowing you to collapse the split
   window.}

         
 @item{@defmenuitem{Show Definitions} Shows the definitions window.}

 @item{@defmenuitem{Hide Definitions} Hides the definitions window.}

 @item{@defmenuitem{Show Interactions} Shows interactions window.}

 @item{@defmenuitem{Hide Interactions} Hides interactions window.}

 @item{@defmenuitem{Use Vertical Layout} and @defmenuitem{Use Horizontal Layout}
        adjust the definitions and interactions window so they
        are either beside each other or with the definitions above
        the interactions window.}
 
 @item{@defmenuitem{Show Log} Shows the current log messages.}
 @item{@defmenuitem{Hide Log} Hides the current log messages.}

 @item{@defmenuitem{Show Tracing} Shows a trace of functions called since
   the last time @onscreen{Run} was clicked. This menu is useful only if
   you have enabled tracing in the @onscreen{Choose Language...} dialog's
   @onscreen{Details} section. Profiling does not apply to all languages.}
   
 @item{@defmenuitem{Hide Tracing} Hides the tracing display.}

 @item{@defmenuitem{Show Profile} Shows the current profiling
   report. This menu is useful only if you have enabled profiling in
   the @onscreen{Choose Language...} dialog's @onscreen{Details}
   section. Profiling does not apply to all languages.}
   
 @item{@defmenuitem{Hide Profile} Hides any profiling
   information currently displayed in the DrRacket window.}
 
 @item{@defmenuitem{Show Program Contour} Shows a ``20,000 foot''
   overview window along the edge of the DrRacket
   window. Each pixel in this window corresponds to a letter
   in the program text.}

 @item{@defmenuitem{Hide Program Contour} Hides the contour window.}
 
 @item{@defmenuitem{Show Line Numbers} Shows line numbers in the 
        definitions window.}
 
 @item{@defmenuitem{Hide Line Numbers} Hides the line numbers in the 
        definitions window.}

  @item{@defmenuitem{Show Column Width Guide at 102 Characters} 
         Shows the column width guide when the current file's width
         is greater than 102 characters.
         
         The number 102 is controlled in the @onscreen{General}
         tab of the @onscreen{Editing} tab in the preferences dialog.}
  @item{@defmenuitem{Hide Column Width Guide for 102 Characters} 
         Hides the column width guide, even with the file's width
         is greater than 102 characters.
         
         The number 102 is controlled in the @onscreen{General}
         tab of the @onscreen{Editing} tab in the preferences dialog.}

 
 @item{@defmenuitem{Show Module Browser} Shows the module DAG rooted
   at the currently opened file in DrRacket.
   
   See also @secref["module-browser"].}

 @item{@defmenuitem{Hide Module Browser} Hides the module browser.
        
          See also @secref["module-browser"].}

]

Note: whenever a program is run, the interactions window is made
 visible if it is hidden.

@; ----------------------------------------

@section{@onscreen{Language}}

@itemize[

 @item{@defmenuitem{Choose Language...} Opens a dialog for selecting
  the current evaluation language. Click @onscreen{Run} to make the
  language active in the interactions window. See
  @secref["choose-language"] for more information about the
  languages.}


 @item{@defmenuitem{Add Teachpack...} Opens a find-file dialog for
  choosing a teachpack to extend the current language. Click
  @onscreen{Run} to make the teachpack available in the interactions
  windows. See @secref["extending-drracket"] for information on
  creating teachpacks.}

@item{@defmenuitem{Clear All Teachpacks} Clears all of the current
  teachpacks.  Click @onscreen{Run} to clear the teachpack from the
  interactions window.}

]

In addition to the above items, a menu item for each teachpack that
clears only the corresponding teachpack.

@; ----------------------------------------

@section[#:tag "menu:racket"]{@onscreen{Racket}}

@itemize[

 @item{@defmenuitem{Run} Resets the interactions window and runs the
  program in the definitions window.}

 @item{@defmenuitem{Break} Breaks the current evaluation.}

 @item{@defmenuitem{Kill} Terminates the current evaluation.}

@item{@defmenuitem{Limit Memory...} Allow you to specify a
limit on the amount of memory that a program running in
DrRacket is allowed to consume.}

@item{@defmenuitem{Clear Error Highlight} Removes the red
background that signals the source location of an error.
Also removes the highlighting for uncovered (untested)
portions of the program.}

 @item{@defmenuitem{Create Executable...} Creates a separate launcher
   for running your program. See @secref["create-exe"] for more
   info.}

@item{@defmenuitem{Module Browser...} Prompts for a file and
  then opens a window showing the module 
  DAG starting at the module in the selected file.
  
  See also @secref["module-browser"].
  }

@item{@defmenuitem{Module Browser on @italic{file}} 
       Opens a separate window showing the module graph rooted at the
       file currently being edited in DrRacket, but
       using the saved file on the disk, instead of the 
       version in DrRacket.
       
  See also @secref["module-browser"].
  }

 @item{@defmenuitem{Reindent} Indents the selected text according to
  the standard Racket formatting conventions. (Pressing the Tab key
  has the same effect.)}
  
 @item{@defmenuitem{Reindent All} Indents all of the text in either
  the definitions or interactions window, depending on the location of
  the selection or blinking caret.}
  
 @item{@defmenuitem{Comment Out with Semicolons} Puts @litchar{;}
  characters at each of the beginning of each selected line of text.}

 @item{@defmenuitem{Comment Out with a Box} Boxes the selected
  text with a comment box.}
  
 @item{@defmenuitem{Uncomment} Removes all @litchar{;} characters at
  the start of each selected line of text or removes a comment box
  around the text. Uncommenting only removes a @litchar{;} if it
  appears at the start of a line and it only removes the first
  @litchar{;} on each line.}
           
 @item{@defmenuitem{Disable Tests} Stops tests written in the definitions
  window from evaluating when the program is Run. Tests can be enabled
  using the @onscreen{Enable Tests} menu item. Disabling tests freezes
  the contents of any existing test report window.
  }

 @item{@defmenuitem{Enable Tests} Allows tests written in the definitions
  window to evaluate when the program is Run. Tests can be disabled using
  the @onscreen{Disable Tests} menu item.
  }

]

@section{@onscreen{Insert}}

@itemize[

 @item{@defmenuitem{Insert Comment Box} Inserts a box that is ignored
  by DrRacket; use it to write comments for people who read your
  program.}

 @item{@defmenuitem{Insert Image...} Opens a find-file dialog for
  selecting an image file in GIF, BMP, XBM, XPM, PNG, or JPG
  format. The image is treated as a value.}

 @item{@defmenuitem{Insert Fraction...} Opens a dialog for a
   mixed-notation fraction, and inserts the given fraction into the
   current editor.}
  
 @item{@defmenuitem{Insert Large Letters...} Opens a dialog for a line of
   text, and inserts a large version of the text (using semicolons and
   spaces).
          
   Most of the dialog is self-explanatory: type in the top space to 
   preview the semicolons in the bottom area. The numbers in the font
   choice item show the (relative) widths of the letter ``w'' in the
   given font to help you pick out the more boldfaced fonts (which
   tend to look better).}

 @item{@defmenuitem{Insert @|lam-str|} Inserts the symbol @|lam-str|
   (as a Unicode character) into the program. The @|lam-str| symbol is
   normally bound the same as @racket[lambda].}

 @item{@defmenuitem{Insert XML Box} Inserts an XML; see
   @secref["xml-boxes"] for more information.}

 @item{@defmenuitem{Insert Racket Box} Inserts a box to contain Racket
   code, typically used inside an XML box; see @secref["xml-boxes"].}

 @item{@defmenuitem{Insert Racket Splice Box} Inserts a box to contain Racket
   code, typically used inside an XML box; see also @secref["xml-boxes"].}

]

@; ----------------------------------------

@section{@onscreen{Windows}}

@itemize[

 @item{@defmenuitem{Bring Frame to Front...}  Opens a window that lists
   all of the opened DrRacket frames. Selecting one of them brings the
   window to the front.}

 @item{@defmenuitem{Most Recent Window} Toggles between the currently
   focused window and the one that most recently had the focus.}

]

Additionally, after the above menu items, this menu contains
an entry for each window in DrRacket. Selecting a menu item
brings the corresponding window to the front.

@; ----------------------------------------

@section{@onscreen{Help}}

@itemize[

 @item{@defmenuitem{Help Desk} Opens the Help Desk. This is the clearing 
 house for all documentation about DrRacket and its language.}
 
 @item{@defmenuitem{About DrRacket...} Shows the credits for DrRacket.}

 @item{@defmenuitem{Check for Updates...} Checks to see if a new version
        of DrRacket has been released.}
 
 @item{@defmenuitem{Related Web Sites} Provides links to related web sites.}

 @item{@defmenuitem{Tool Web Sites} Provides links to web sites for
   installed tools.}

 @item{@defmenuitem{Submit Bug Report...} The preferred mechanism for sending
        in bug reports to the Racket development team. It automatically collects 
        information about your Racket installation that sometimes can help
        diagnose problems.}
 
 @item{@defmenuitem{Saved Bug Reports} A submenu that holds bug reports that
        you've started editing but haven't yet sent in.}
 
 @item{@defmenuitem{Saved Bug Reports} A submenu that holds bug reports that
        you've started editing but haven't yet sent in.}

 @item{@defmenuitem{Configure Command Line for Racket...} This menu item is
        available only under Mac OS X. After prompting you for your password,
        it adds a file named @filepath{racket} to @filepath{/etc/init.d/}
        that contains the location of the @filepath{bin} directory. 
        This has the effect of adding that path to the default PATH environment
        variable (unless your shell explicitly changes teh default behavior).}
  
 @item{@defmenuitem{Interact with DrRacket in English} Changes DrRacket's
   interface to use English; the menu item appears only when the
   current language is not English. Additional menu items switch
   DrRacket to other languages.}

]

