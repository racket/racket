#lang scribble/doc
@(require "common.ss")
@(tools-title "unit")

@definterface[drscheme:unit:tab<%> (drscheme:rep:context<%>)]{
@defmethod[(break-callback) void?]{
@methspec{
This method is called when the break button is clicked and
this tab is the active tab.
}

@methimpl{
By default, breaks any evaluation that may be happening at
this point.
}}

@defmethod[#:mode pubment (can-close?) boolean?]{
@methspec{
This method is called to determine if it is okay to close this tab.
}

@methimpl{
Calls the definitions text's and interactions text's
@method[editor:basic<%> can-close?] method.
}}

@defmethod[#:mode override 
           (disable-evaluation)
           void?]{
Disables the Run button, and the Run menu item and
@method[editor<%> lock]s the interactions window, and the definitions window.
}

@defmethod[#:mode override 
           (enable-evaluation)
           void?]{
Enables the Run button, and the Run menu item and unlocks
(via the
@method[editor<%> lock] method) the interactions window and the definitions window.}

@defmethod[#:mode override 
           (get-breakables)
           (values (or/c thread? false/c) (or/c custodian? false/c))]{}

@defmethod[(get-defs)
           (is-a?/c drscheme:unit:definitions-text<%>)]{
This text is initially the top half of the drscheme window and
contains the users program.

This text defaults to a @scheme[text%]
object, but if you change
@scheme[drscheme:get/extend:extend-definitions-text] procedure, it will use the extended class to create the text.

}

@defmethod[#:mode override 
           (get-directory)
           (or/c string? false/c)]{

This is the directory that the file is saved in, or the
directory DrScheme started up in, if the file has not been
saved.


}

@defmethod[(get-enabled)
           boolean?]{
Indicates if evaluation is currently enabled in this
tab. Evaluation is typically disabled when some evaluation
is already running (in another thread).

}

@defmethod[(get-frame)
           (is-a?/c drscheme:unit:frame%)]{
Returns the frame that this tab is inside.

}

@defmethod[(get-ints)
           (is-a?/c drscheme:rep:text%)]{
This text is initially the bottom half of the drscheme window and
contains the users interactions with the REPL.

This text defaults to a @scheme[drscheme:rep:text%]
object, but if you use the
@scheme[drscheme:get/extend:extend-interactions-text] procedure,
it will use the extended class to create the text.

}

@defmethod[(is-current-tab?)
           boolean?]{
Indicates if this tab is the currently active tab.

}

@defmethod[(is-running?)
           boolean?]{

Indicates if the running message in the bottom right of
drscheme's frame should be ``running'' or ``not running''
when this frame is active.



}

@defmethod[#:mode pubment 
           (on-close)
           void?]{
@methspec{

This method is called when the tab is closed.

}

@methimpl{

Calls the definitions text's
@method[editor:basic<%> on-close] and interactions text's
@method[drscheme:rep:text% on-close] methods.


}}

@defmethod[#:mode override 
           (reset-offer-kill)
           void?]{}

@defmethod[#:mode override 
           (set-breakables [thread (or/c thread? false/c)]
                           [custodian (or/c custodian? false/c)])
           void?]{}}


@defclass[drscheme:unit:tab% object% (drscheme:unit:tab<%>)]{

The base class that implements the tab's functionality.



@defconstructor/make[()]{}

@defmethod[#:mode override 
           (clear-annotations)
           void?]{

Clears any error highlighting.




}}


@defmixin[drscheme:unit:program-editor-mixin (text% editor:basic<%>) ()]{

This mixes in the ability to reset the highlighting for
error message when the user modifies the buffer. Use it for
editors that have program text where errors can occur.



@defmethod[#:mode override 
           (after-delete [start number]
                         [len number])
           void?]{

Calls the super method.

Resets an error highlighting.



}

@defmethod[#:mode override 
           (after-insert [start number]
                         [len number])
           void?]{

Calls the super method.

Resets an error highlighting.


}}


@defclass[drscheme:unit:interactions-canvas% canvas:wide-snip% ()]{





@defconstructor/auto-super[()]{
Passes all arguments to @scheme[super-init].
}}


@defclass[drscheme:unit:frame% (drscheme:frame:basics-mixin (drscheme:frame:mixin frame:searchable%)) (drscheme:unit:frame<%>)]{

This frame inserts the Scheme and Language menus into the menu bar as it is initialized.




@defconstructor/auto-super[()]{
Passes all arguments to @scheme[super-init].
}

@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}

@defmethod[#:mode override 
           (add-show-menu-items [show-menu (is-a?/c menu%)])
           void?]{

Adds the ``Show Definitions'', ``Show Interactions'' and
``Show Contour'' menu items.


}

@defmethod[(break-callback)
           void?]{
@methspec{

This method is called when the user clicks on the break
button or chooses the break menu item.

}

@methimpl{

Breaks the user's evaluation started by the Run button
(or possibly a queued callback in the user's eventspace).


}}

@defmethod[(change-to-file [file string?])
           void?]{

Loads this file into this already created frame. In normal
DrScheme use, this method is only called if this is the
first frame opened and no editing has occurred. It should be
safe to call this at anytime, however.


}

@defmethod[#:mode override 
           (edit-menu:between-select-all-and-find)
           void?]{

Adds the @scheme["Split"] and @scheme["Collapse"] menu items.


}

@defmethod[(execute-callback)
           void?]{
@methspec{

This method is called when the user clicks on the Run
button or chooses the Run menu item.

}

@methimpl{

It calls 
@method[drscheme:rep:context<%> ensure-rep-shown] and then it calls
@method[drscheme:rep:text% do-many-text-evals] passing in the result of
@method[drscheme:unit:frame<%> get-interactions-text] and its entire range, unless the first two characters are 
@litchar{#!} in which case, it skips the first line.


}}

@defmethod[#:mode override 
           (file-menu:between-open-and-revert)
           void?]{

Calls the super method and adds a
@scheme[separator-menu-item%] to the menu.


}

@defmethod[#:mode override 
           (file-menu:between-print-and-close)
           void?]{

Adds a menu item for printing the interactions.


}

@defmethod[#:mode override 
           (file-menu:between-save-as-and-print)
           void?]{

Adds a submenu that contains various save options:
@itemize[
@item{save definitions as text}
@item{save interactions}
@item{save interactions as}
@item{save interactions as text}
]

and adds a separator item.


}

@defmethod[#:mode override 
           (file-menu:print-string)
           void?]{

returns @scheme["Definitions"]


}

@defmethod[#:mode override 
           (file-menu:save-as-string)
           void?]{

Returns @scheme["Definitions"].


}

@defmethod[#:mode override 
           (file-menu:save-string)
           void?]{

Returns @scheme["Definitions"].


}

@defmethod[(get-break-button)
           (is-a?/c button%)]{
Returns the break button. Mostly used for test suites.

}

@defmethod[(get-button-panel)
           (is-a?/c horizontal-panel%)]{
This panel goes along the top of the drscheme window and has buttons
for important actions the user frequently executes. 

A tool can add a button to this panel to make some new functionality
easily accessible to the user.

See also mrlib's @scheme[switchable-button%].

}

@defmethod[#:mode override 
           (get-canvas)
           (is-a?/c editor-canvas%)]{

Returns the result of
@method[drscheme:unit:frame<%> get-definitions-canvas].


}

@defmethod[#:mode override
           (get-canvas%)
           (is-a?/c canvas%)]{

Returns the result of
@scheme[drscheme:get/extend:get-definitions-canvas].


}

@defmethod*[([(get-definitions/interactions-panel-parent)
              (is-a?/c vertical-panel%)]
             [(get-definitions/interactions-panel-parent)
              void?])]{
@methspec{

This method is provided so that tools can add 
@scheme[area-container<%>]s to the drscheme frame. Override this method so that it
returns a child of the super-classes's result and insert new 
children in between.

}

@methimpl{

First case:


Returns the result of 
@method[frame:basic<%> get-area-container] 


Second case:



}}

@defmethod[#:mode override 
           (get-editor)
           (is-a?/c editor<%>)]{

Returns the result of
@method[drscheme:unit:frame<%> get-definitions-text].



}

@defmethod[#:mode override
           (get-editor%)
           (is-a?/c editor<%>)]{

Returns the result of
@scheme[drscheme:get/extend:get-definitions-text].


}

@defmethod[(get-execute-button)
           (is-a?/c button%)]{
Returns the Run button. Mostly used for test suites.

}

@defmethod[#:mode override 
           (get-text-to-search)
           (is-a?/c text:searching%)]{

returns the text that is active in the last canvas passed to
@method[drscheme:unit:frame% make-searchable] 

}

@defmethod[(make-searchable [canvas (is-a?/c drscheme:unit:interactions-canvas%)])
           void?]{

stores the canvas, until 
@method[drscheme:unit:frame% get-text-to-search] is called.


}

@defmethod[#:mode override 
           (on-close)
           void?]{

Sends the result of
@method[drscheme:unit:frame<%> get-interactions-text] the
@method[drscheme:rep:text% shutdown] and 
@method[drscheme:rep:text% on-close] methods.

Calls the super method.


}

@defmethod[#:mode override 
           (on-size)
           void?]{

Updates the preferences for the window width and height
so next time a drscheme window is opened, it will be this
width and height.


}

@defmethod[(still-untouched?)
           boolean?]{
@methspec{

determines if the definitions window has not been
modified. Used in conjunction with
@method[drscheme:unit:frame% change-to-file].

}

@methimpl{

Returns @scheme[#t] if the buffer is empty, it has not been
saved and it is unmodified.


}}

@defmethod[(update-save-button [modified? any/c])
           void?]{

This method hides or shows the save button, based on
the @scheme[modified?] argument. 

If the save button has not been created yet, it remembers
the @scheme[modified?] argument as an initial visibility for
the save button.

This method is called by the
@method[drscheme:unit:definitions-text% set-modified] method.


}

@defmethod[(update-save-message [name string?])
           void?]{

Updates the save message on the drscheme frame. This method is called by
the 
@method[drscheme:unit:definitions-text% set-filename] method.


}

@defmethod[#:mode override 
           (update-shown)
           void?]{

Updates the interactions, definitions, and contour menu
items based on the contents of the windows.


}}


@definterface[drscheme:unit:frame<%> ()]{

@defmethod[(get-language-menu) (is-a?/c menu%)]{ Returns the
  language-specific menu. This menu is called the
  @onscreen{Scheme} menu in the Scheme language but is, in general,
  controlled by the @scheme['drscheme:language-menu-title] 
  capability (see @scheme[drscheme:language:register-capability]
  for details on capabilities).
 }

@defmethod[(ensure-defs-shown)
           void?]{
Ensures that the definitions window is visible.

}

@defmethod[(ensure-rep-hidden)
           void?]{

Makes sure the rep is hidden (by making the definitions window
visible).


}

@defmethod[(ensure-rep-shown)
           void?]{

Shows the interactions window


}

@defmethod[(get-current-tab)
           (is-a?/c drscheme:unit:tab<%>)]{
Returns the currently active tab.

}

@defmethod[(get-tab-filename [i (<=/c 0 (#,(method drscheme:unit:frame<%> get-tab-count)))]) string?]{
  Returns a string naming the file in the @scheme[i]th tab or, if
  the file is not saved, something like ``Untitled''.
}

@defmethod[(get-tab-count) exact-positive-integer?]{
  Returns the number of open tabs in the frame.                                                    
}

@defmethod[(open-in-new-tab [filename (or/c path-string? #f)]) void?]{
  Opens a new tab in this frame. If @scheme[filename] is a @scheme[path-string?],
  It loads that file in the definitions window of the new tab.
}

@defmethod[#:mode public-final (close-current-tab) void?]{
  Closes the current tab, making some other tab visible.
  If there is only one tab open, this method does nothing.
}

@defmethod[(get-definitions-canvas)
           (is-a?/c drscheme:unit:definitions-canvas%)]{

This canvas is the canvas containing the 
@method[drscheme:unit:frame<%> get-definitions-text]. It is initially the top half of the drscheme window.

This canvas defaults to a @scheme[drscheme:unit:definitions-canvas%]
object, but if you change the
@scheme[drscheme:get/extend:extend-definitions-canvas] procedure, it will use the class in the parameter to create the canvas.


}

@defmethod[(get-definitions-text)
           (is-a?/c drscheme:unit:definitions-text%)]{

Calls result of
@method[drscheme:unit:frame<%> get-current-tab]'s 
@method[drscheme:unit:tab<%> get-defs] method.


}

@defmethod[(get-insert-menu)
           (is-a?/c menu%)]{
@methspec{

Returns the Insert menu.

}}

@defmethod[(get-interactions-canvas)
           (instanceof (derivedfrom drscheme:unit:interactions-canvas%))]{

This canvas is the canvas containing the 
@method[drscheme:unit:frame<%> get-interactions-text]. It is initially the bottom half of the drscheme window.

This canvas defaults to a @scheme[drscheme:unit:interactions-canvas%]
object, but if you use the
@scheme[drscheme:get/extend:extend-interactions-canvas] procedure,
it will use the extended class to create the canvas.


}

@defmethod[(get-interactions-text)
           (instanceof (derivedfrom drscheme:rep:text%))]{

Calls result of
@method[drscheme:unit:frame<%> get-current-tab]'s 
@method[drscheme:unit:tab<%> get-ints] method.


}

@defmethod[(get-tabs)
           (listof drscheme:unit:tab<%>)]{
Returns the list of tabs in this frame.

}

@defmethod[#:mode pubment 
           (on-tab-change [from-tab (is-a?/c drscheme:unit:tab<%>)]
                          [to-tab (is-a?/c drscheme:unit:tab<%>)])
           void?]{
@methspec{

Called after a new tab becomes the selected tab in the frame.

}

@methimpl{

The @scheme[from-tab] argument is the previously selected tab, and the
 @scheme[to-tab] argument is the newly selected tab.



}}

@defmethod[(register-capability-menu-item [key symbol]
                                          [menu (is-a? menu%)])
           void?]{
Registers the menu item that was most recently added as
being controlled by the capability @scheme[key]. This means
that the (boolean) value of the capability determines if the
menu item is present in the menu (the capability is checked
when the menus are cliked on).

This assumes that the menu items in this menu are not moved
around, except by the this capability. If they are, things
can go funny (i.e., no good checks are in place).

Note that the capability must be registered separately, via
@scheme[drscheme:language:register-capability].


}

@defmethod[(register-toolbar-button [tb (is-a?/c switchable-button%)]) void?]{
Registers the toolbar button @scheme[tb]. This is required
so that the toolbar buttons properly switch orientation when 
the toolbar's position is moved.
}

@defmethod[(register-toolbar-buttons [tbs (listof (is-a?/c switchable-button%))]) void?]{
Simultaneously registers the toolbar buttons @scheme[tbs]. This is required
so that the toolbar buttons properly switch orientation when 
the toolbar's position is moved.
}

@defmethod[(unregister-toolbar-button [tb (is-a?/c switchable-button%)]) void?]{
Unregisters the toolbar button @scheme[tb]. Use this method to ensure
that the button is not referenced by this frame and thus can be gc'd.
}

}


@defclass[drscheme:unit:definitions-text% (drscheme:rep:drs-bindings-keymap-mixin (drscheme:unit:program-editor-mixin (scheme:text-mixin text:info%))) (drscheme:unit:definitions-text<%>)]{

@defconstructor[()]{
Passes all arguments to @scheme[super-init].
}

@defmethod[#:mode override 
           (set-filename)
           void?]{

Calls
@method[drscheme:unit:frame% update-save-message].
}

@defmethod[#:mode override 
           (set-modified)
           void?]{

Calls
@method[drscheme:unit:frame% update-save-button].
}}


@definterface[drscheme:unit:definitions-text<%> ()]{

This interface is implemented by the definitions text. 



@defmethod[#:mode pubment 
           (after-set-next-settings [language-settings language-settings])
           void?]{
@methspec{

Called when the next settings changes. See also
@method[drscheme:unit:definitions-text<%> get-next-settings].

}

@methimpl{




}}

@defmethod[(begin-metadata-changes)
           void?]{
Augment this method to be notified when DrScheme is changing
the buffer to insert metadata. The metadata is only inserted
during saving, so tools that track changes to DrScheme will
need to ignore changes that occur after this method is
called, and before
@method[drscheme:unit:definitions-text<%> end-metadata-changes] is called.

A call to @scheme[begin-metadata-changes] will always be
followed with a call to @scheme[end-metadata-changes] (ie,
the calls cannot be nested).

}

@defmethod[(end-metadata-changes)
           void?]{
Called when the changes to insert metadata are done, and the
editor is back to its state at the time of the call to
@method[drscheme:unit:definitions-text<%> begin-metadata-changes].

A call to @scheme[begin-metadata-changes] will always be
followed with a call to @scheme[end-metadata-changes] (ie,
the calls cannot be nested).

}

@defmethod[(get-next-settings)
           language-settings]{
This method returns the language-settings that will be used
when the user next clicks Run in this DrScheme window.

}

@defmethod[(get-port-name-identifier)
           symbol]{

Returns an identifier that can be used as a port's name when
the editor is not saved. (If it is saved, the filename of
the editor should be used.)


}

@defmethod[(get-tab)
           (instanceof drscheme:unit:tab%)]{
Returns the editor's enclosing tab.

}

@defmethod[(port-name-matches? [id any])
           boolean?]{

Indicates if the name of a port (which is also saved in the
source field of an exception record) matches this editor.


}

@defmethod[(set-needs-execution-message [msg string?])
           void?]{
@methspec{

This method, when called, puts this drscheme window in a
state such that interactions submitted to the REPL will
trigger a yellow warning message. The state is reset when
the program is next Run.

}

@methimpl{

Records @scheme[msg] and uses it the next time the user submits
an interaction (unless the Runs first).


}}

@defmethod[(set-next-settings [language-settings language-settings]
                              [update-prefs? any/c #t])
           void?]{

Changes the language settings for this window. If
@scheme[update-prefs?] is a true value, the preference is
changed, which affects newly created windows.

See also
@method[drscheme:unit:definitions-text<%> after-set-next-settings] and
@method[drscheme:unit:definitions-text<%> get-next-settings].


}}


@defclass[drscheme:unit:definitions-canvas% editor-canvas% ()]{

Initializes the visibility of the save button.

}

@(tools-include "unit")
