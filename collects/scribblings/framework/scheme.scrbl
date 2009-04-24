#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Scheme}

@definterface[scheme:sexp-snip<%> ()]{
  @defmethod*[(((get-saved-snips) (listof snip%)))]{
    This returns the list of snips hidden by the sexp snip.

  }
}
@defclass[scheme:sexp-snip% snip% (scheme:sexp-snip<%> readable-snip<%>)]{

  @defmethod*[#:mode override (((get-text (offset number) (num number) (flattened? boolean |#f|)) string))]{

    Returns the concatenation of the text for all of the hidden
    snips.
  }
  @defmethod*[#:mode override (((copy) (is-a?/c scheme:sexp-snip%)))]{

    Returns a copy of this snip that includes the hidden snips.
  }
  @defmethod*[#:mode override (((write (stream-out editor-stream-out%)) void))]{

    Saves the embedded snips
  }
  @defmethod*[#:mode override (((draw (dc dc<%>) (x real) (y real) (left real) (top real) (right real) (bottom real) (dx real) (dy real) (draw-caret symbol?)) void))]{

    Draws brackets with a centered ellipses between them.
  }
  @defmethod*[#:mode override (((get-extent (dc (is-a?/c dc<%>)) (x real) (y real) (w boxed |#f|) (h boxed |#f|) (descent boxed |#f|) (space boxed |#f|) (lspace boxed |#f|) (rspace boxed |#f|)) void))]{

    Returns a size corresponding to what this snip draws.
  }
}
@definterface[scheme:text<%> (text:basic<%> mode:host-text<%> color:text<%>)]{
  Texts matching this interface support Scheme mode operations.
  @defmethod*[(((get-limit (start exact-integer)) int))]{

    Returns a limit for backward-matching parenthesis starting at position
    @scheme[start].

  }
  @defmethod*[(((balance-parens (key-event (instance key-event%))) void))]{
    This function is called when the user types a close parenthesis in the
    @scheme[text%]. If the close parenthesis that the user inserted does not match the 
    corresponding open parenthesis and the @scheme['framework:fixup-parens] preference is
    @scheme[#t] (see
    @scheme[preferences:get]) the correct closing parenthesis is inserted.
    If the @scheme['framework:paren-match] preference is
    @scheme[#t] (see
    @scheme[preferences:get]) the matching open parenthesis is flashed.

  }
  @defmethod*[(((tabify-on-return?) boolean?))]{
    The result of this method is used to determine if the return key
    automatically tabs over to the correct position.

    Override it to change its behavior.


  }
  @defmethod*[(((tabify (start-pos exact-integer (send this text get-start-position))) void))]{

    Tabs the line containing by @scheme[start-pos]

  }
  @defmethod*[(((tabify-selection (start exact-integer) (end exact-integer)) void))]{

    Sets the tabbing for the lines containing positions @scheme[start]
    through @scheme[end].
  }
  @defmethod*[(((tabify-all) void))]{

    Tabs all lines.
  }
  @defmethod*[(((insert-return) void))]{

    Inserts a newline into the buffer. If 
    @method[scheme:text<%> tabify-on-return?]
    returns @scheme[#t], this will tabify the new line.
  }
  @defmethod*[(((box-comment-out-selection (start-pos (or/c (symbols 'start) exact-integer?)) (end-pos (or/c (symbols 'end) exact-integer?))) void))]{
    This method comments out a selection in the text by putting it into a comment box.


    Removes the region from @scheme[start-pos] to @scheme[end-pos]
    from the editor and inserts a comment box with that region
    of text inserted into the box.

    If @scheme[start-pos] is @scheme['start], the starting point of
    the selection is used. If @scheme[end-pos] is @scheme['end],
    the ending point of the selection is used.
  }
  @defmethod*[(((comment-out-selection (start exact-integer) (end exact-integer)) void))]{

    Comments the lines containing positions @scheme[start] through @scheme[end]
    by inserting a semi-colon at the front of each line.
  }
  @defmethod*[(((uncomment-selection (start int) (end int)) void))]{

    Uncomments the lines containing positions @scheme[start] through @scheme[end].

  }
  @defmethod*[(((get-forward-sexp (start exact-integer)) (union |#f| exact-integer)))]{

    Returns the position of the end of next S-expression after position
    @scheme[start], or @scheme[#f] if there is no appropriate answer.

  }
  @defmethod*[(((remove-sexp (start exact-integer)) void))]{

    Forward-deletes the S-expression starting after the position @scheme[start].

  }
  @defmethod*[(((forward-sexp (start |#t|)) exact-integer))]{

    Moves forward over the S-expression starting at position @scheme[start].
  }
  @defmethod*[(((flash-forward-sexp (start-pos exact-integer)) void))]{

    Flashes the parenthesis that closes the sexpression at
    @scheme[start-pos].


  }
  @defmethod*[(((get-backward-sexp (start exact-integer)) (union exact-integer |#f|)))]{


    Returns the position of the start of the S-expression before or
    containing @scheme[start], or @scheme[#f] if there is no appropriate
    answer.
  }
  @defmethod*[(((flash-backward-sexp (start-pos exact-integer)) void))]{

    Flashes the parenthesis that opens the sexpression at
    @scheme[start-pos].

  }
  @defmethod*[(((backward-sexp (start-pos exact-integer)) void))]{
    Move the caret backwards one sexpression


    Moves the caret to the beginning of the sexpression that ends at
    @scheme[start-pos].
  }
  @defmethod*[(((find-up-sexp (start-pos exact-integer)) (union |#f| exact-integer)))]{

    Returns the position of the beginning of the next sexpression outside
    the sexpression that contains @scheme[start-pos]. If there is no such
    sexpression, it returns @scheme[#f].

  }
  @defmethod*[(((up-sexp (start exact-integer)) void))]{

    Moves backward out of the S-expression containing the position @scheme[start].

  }
  @defmethod*[(((find-down-sexp (start-pos exact-integer)) (union |#f| exact-integer)))]{

    Returns the position of the beginning of the next sexpression inside
    the sexpression that contains @scheme[start-pos]. If there is no such
    sexpression, it returns @scheme[#f].
  }
  @defmethod*[(((down-sexp (start exact-integer)) void))]{

    Moves forward into the next S-expression after the position @scheme[start].
  }
  @defmethod*[(((remove-parens-forward (start exact-integer)) void))]{

    Removes the parentheses from the S-expression starting after the
    position @scheme[start].

  }
  @defmethod*[(((select-forward-sexp (start exact-integer)) |#t|))]{

    Selects the next S-expression, starting at position @scheme[start].
  }
  @defmethod*[(((select-backward-sexp (start exact-integer)) |#t|))]{

    Selects the previous S-expression, starting at position @scheme[start].

  }
  @defmethod*[(((select-up-sexp (start exact-integer)) |#t|))]{

    Selects the region to the enclosing S-expression, starting at position @scheme[start].

  }
  @defmethod*[(((select-down-sexp (start exact-integer)) |#t|))]{

    Selects the region to the next contained S-expression, starting at position @scheme[start].

  }
  @defmethod*[(((transpose-sexp (start exact-integer)) void))]{

    Swaps the S-expression beginning before the position @scheme[start] with
    the next S-expression following @scheme[start].
  }
  @defmethod*[(((mark-matching-parenthesis (pos exact-positive-integer)) void))]{
    If the paren after @scheme[pos] is matched, this method
    highlights it and its matching counterpart in dark green.

  }
  @defmethod*[(((get-tab-size) exact-integer))]{
    This method returns the current size of the tabs for scheme mode.
    See also
    @method[scheme:text<%> set-tab-size].

  }
  @defmethod*[(((set-tab-size (new-size exact-integer)) void))]{
    This method sets the tab size for this 
    text.

  }
  @defmethod*[(((introduce-let-ans) void))]{
    Adds a let around the current s-expression and a printf into the body
    of the let.

  }
  @defmethod*[(((move-sexp-out) void))]{
    Replaces the sexpression surrounding the insertion point with the
    sexpression following the insertion point.

  }
}
@defmixin[scheme:text-mixin (text:basic<%> mode:host-text<%> color:text<%> text:autocomplete<%>) (scheme:text<%>)]{
  This mixin adds functionality for editing Scheme files.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.
  @defmethod*[#:mode override (((get-word-at (pos positive-exact-integer)) string))]{
    Returns the word just before @scheme[pos], which is then used
    as the prefix for auto-completion.

  }
}
@definterface[scheme:text-mode<%> ()]{
  The result of
  @scheme[scheme:text-mode-mixin]
  implements this interface.
}
@defmixin[scheme:text-mode-mixin (color:text-mode<%> mode:surrogate-text<%>) (scheme:text-mode<%>)]{
  This mixin adds Scheme mode functionality
  to the mode that it is mixed into. The resulting
  mode assumes that it is only set to an editor
  that is the result of
  @scheme[scheme:text-mixin].
  @defmethod*[#:mode override (((on-disable-surrogate) void))]{

    Removes the scheme keymap (see also
    @scheme[scheme:get-keymap]) and disables any parenthesis
    highlighting in the host editor.
  }
  @defmethod*[#:mode override (((on-enable-surrogate) void))]{

    Adds the scheme keymap (see also
    @scheme[scheme:get-keymap]) and enables a parenthesis
    highlighting in the host editor. 

  }
}
@defmixin[scheme:set-mode-mixin (scheme:text<%> mode:host-text<%>) ()]{
  This mixin creates a new instance of
  @scheme[scheme:text-mode%]
  and installs it, by calling its own
  @method[mode:host-text<%> set-surrogate]
  method with the object.
}
@defclass[scheme:text% (scheme:set-mode-mixin (scheme:text-mixin (text:autocomplete-mixin (mode:host-text-mixin color:text%)))) ()]{}
@defclass[scheme:text-mode% (scheme:text-mode-mixin color:text-mode%) ()]{}

@(include-previously-extracted "main-extracts.ss" #rx"^scheme:")
