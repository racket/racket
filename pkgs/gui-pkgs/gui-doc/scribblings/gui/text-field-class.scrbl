#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/text-field}}

@defclass/title[text-field% object% (control<%>)]{

A @racket[text-field%] object is an editable text field with an
 optional label displayed in front of it. There are two text field
 styles:

@itemize[

 @item{A single line of text is visible, and a special control event
 is generated when the user presses Return or Enter (when the text field has the
 focus) and the event is not handled by the text field's frame or
 dialog (see @xmethod[top-level-window<%> on-traverse-char] ).}

 @item{Multiple lines of text are visible, and Enter is not handled
 specially.}

]

Whenever the user changes the content of a text field, its callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each text field is created.

The text field is implemented using a @racket[text%] editor (with an
 inaccessible display). Thus, whereas @racket[text-field%] provides
 only @method[text-field% get-value] and @method[text-field%
 set-value] to manipulate the text in a text field, the
 @method[text-field% get-editor] returns the field's editor, which
 provides a vast collection of methods for more sophisticated
 operations on the text.

The keymap for the text field's editor is initialized by calling the
 current keymap initializer procedure, which is determined by the
 @racket[current-text-keymap-initializer] parameter.


@defconstructor[([label (or/c label-string? #f)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c text-field%) (is-a?/c control-event%) 
                            . -> . any) 
                           (lambda (t e) (void))]
                 [init-value string? ""]
                 [style (listof (or/c 'single 'multiple 'hscroll 'password 
                                      'vertical-label 'horizontal-label 
                                      'deleted)) 
                        '(single)]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c (memq 'multiple style)])]{

If @racket[label] is not @racket[#f], it is used as the text field
 label.  Otherwise, the text field does not display its label.

@labelstripped[@racket[label] @elem{} @elem{move the keyboard focus to the text field}]

The @racket[callback] procedure is called when the user changes the
 text in the text field or presses the Enter key (and Enter is not
 handled by the text field's frame or dialog; see
 @xmethod[top-level-window<%> on-traverse-char]). If the user presses
 Enter, the type of event passed to the callback is
 @indexed-racket['text-field-enter], otherwise it is
 @indexed-racket['text-field].

If @racket[init-value] is not @racket[""], the @tech{graphical minimum size} for the
 text item is made wide enough to show @racket[init-value]. Otherwise,
 a built-in default width is selected. For a text field in single-line
 mode, the @tech{graphical minimum size} is set to show one line, and only the
 control's width is stretchable by default. For a multiple-line text field, the
 @tech{graphical minimum size} shows three lines of text, and it is stretchable in both
 directions by default.

The style must contain exactly one of @racket['single] or
 @racket['multiple]; the former specifies a single-line field and the
 latter specifies a multiple-line field. The @racket['hscroll] style
 applies only to multiple-line fields; when @racket['hscroll] is
 specified, the field has a horizontal scrollbar and autowrapping is
 disabled; otherwise, the field has no horizontal scrollbar and
 autowrapping is enabled. A multiple-line text field always has a
 vertical scrollbar. The @racket['password] style indicates that the
 field should draw each character of its content using a generic
 symbol instead of the actual character.  @HVLabelNote[@racket[style]]{text field}
 @DeletedStyleNote[@racket[style] @racket[parent]]{text field}.

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

}


@defmethod[(get-editor)
           (is-a?/c text%)]{

Returns the editor used to implement the text field.

For a text field, the most useful methods of a @racket[text%] object
 are the following:
@itemize[
 
 @item{@racket[(send a-text @#,method[text% get-text])] returns
 the current text of the editor.}

 @item{@racket[(send a-text @#,method[text% erase])] deletes all text from
 the editor.}

 @item{@racket[(send a-text @#,method[text% insert] _str)] inserts
 @racket[_str] into the editor at the current caret position.}

]
}


@defmethod[(get-field-background) (is-a?/c color%)]{

Gets the background color of the field's editable area.}


@defmethod[(get-value)
           string?]{

Returns the text currently in the text field.

}


@defmethod[(set-field-background [color (is-a?/c color%)])
           void?]{

Sets the background color of the field's editable area.}


@defmethod[(set-value [val string?])
           void?]{

Sets the text currently in the text field. (The control's callback
 procedure is @italic{not} invoked.)

@MonitorCallback[@elem{A text field's value} @elem{the user typing into the control} @elem{value}]

}}
