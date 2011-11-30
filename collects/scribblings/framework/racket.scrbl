#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Racket}

@definterface[racket:sexp-snip<%> ()]{
  @defmethod*[(((get-saved-snips) (listof (is-a?/c snip%))))]{
    This returns the list of snips hidden by the sexp snip.
  }
}
@defclass[racket:sexp-snip% snip% (racket:sexp-snip<%> readable-snip<%>)]{
  @defmethod*[#:mode override
              (((get-text (offset number?) (num number?)
                          (flattened? boolean? #f))
                string?))]{
    Returns the concatenation of the text for all of the hidden snips.
  }
  @defmethod*[#:mode override (((copy) (is-a?/c racket:sexp-snip%)))]{
    Returns a copy of this snip that includes the hidden snips.
  }
  @defmethod*[#:mode override (((write (stream-out (is-a?/c editor-stream-out%))) void?))]{
    Saves the embedded snips
  }
  @defmethod*[#:mode override
              (((draw (dc dc<%>) (x real?) (y real?)
                      (left real?) (top real?) (right real?) (bottom real?)
                      (dx real?) (dy real?) (draw-caret symbol?))
                void?))]{
    Draws brackets with a centered ellipses between them.
  }
  @defmethod*[#:mode override
              (((get-extent (dc (is-a?/c dc<%>)) (x real?) (y real?)
                            (w (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                            (h (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                            (descent (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                            (space (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                            (lspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                            (rspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f))
                void?))]{
    Returns a size corresponding to what this snip draws.
  }
}
@definterface[racket:text<%> (text:basic<%> mode:host-text<%> color:text<%>)]{
  Texts matching this interface support Racket mode operations.

  @defmethod*[(((get-limit (start exact-integer?)) exact-integer?))]{
    Returns a limit for backward-matching parenthesis starting at position
    @racket[start].
  }

  @defmethod*[(((balance-parens (key-event (is-a?/c key-event%))) void?))]{
    This function is called when the user types a close parenthesis in the
    @racket[text%].  If the close parenthesis that the user inserted does not
    match the corresponding open parenthesis and the
    @racket['framework:fixup-parens] preference is @racket[#t] (see
    @racket[preferences:get]) the correct closing parenthesis is inserted.  If
    the @racket['framework:paren-match] preference is @racket[#t] (see
    @racket[preferences:get]) the matching open parenthesis is flashed.
  }

  @defmethod*[(((tabify-on-return?) boolean?))]{
    The result of this method is used to determine if the return key
    automatically tabs over to the correct position.

    Override it to change its behavior.
  }

  @defmethod*[(((tabify (start-pos exact-integer?
                                   (send this get-start-position)))
                void?))]{
    Tabs the line containing by @racket[start-pos]
  }

  @defmethod*[(((tabify-selection (start exact-integer? (send this get-start-position))
                                  (end exact-integer? (send this get-end-position)))
                void?))]{
    Sets the tabbing for the lines containing positions @racket[start]
    through @racket[end].
  }

  @defmethod*[(((tabify-all) void?))]{
    Tabs all lines.
  }

  @defmethod*[(((insert-return) void?))]{
    Inserts a newline into the buffer.  If @method[racket:text<%>
    tabify-on-return?] returns @racket[#t], this will tabify the new line.
  }

  @defmethod*[(((box-comment-out-selection
                 (start-pos (or/c (symbols 'start) exact-integer?))
                 (end-pos (or/c (symbols 'end) exact-integer?)))
                void?))]{
    This method comments out a selection in the text by putting it into a
    comment box.

    Removes the region from @racket[start-pos] to @racket[end-pos] from the
    editor and inserts a comment box with that region of text inserted into the
    box.

    If @racket[start-pos] is @racket['start], the starting point of the
    selection is used.  If @racket[end-pos] is @racket['end], the ending point
    of the selection is used.
  }

  @defmethod*[(((comment-out-selection (start exact-integer?)
                                       (end exact-integer?))
                void?))]{
    Comments the lines containing positions @racket[start] through @racket[end]
    by inserting a semi-colon at the front of each line.
  }

  @defmethod*[(((uncomment-selection (start exact-integer?) (end exact-integer?)) void?))]{
    Uncomments the lines containing positions @racket[start] through
    @racket[end].
  }

  @defmethod*[(((get-forward-sexp (start exact-integer?))
                (or/c #f exact-integer?)))]{
    Returns the position of the end of next S-expression after position
    @racket[start], or @racket[#f] if there is no appropriate answer.
  }

  @defmethod*[(((remove-sexp (start exact-integer?)) void?))]{
    Forward-deletes the S-expression starting after the position
    @racket[start].
  }

  @defmethod*[(((forward-sexp (start exact-integer?)) void?))]{
    Moves forward over the S-expression starting at position @racket[start].
  }

  @defmethod*[(((flash-forward-sexp (start-pos exact-integer?)) void?))]{
    Flashes the parenthesis that closes the sexpression at @racket[start-pos].
  }

  @defmethod*[(((get-backward-sexp (start exact-integer?))
                (or/c exact-integer? #f)))]{
    Returns the position of the start of the S-expression before or containing
    @racket[start], or @racket[#f] if there is no appropriate answer.
  }

  @defmethod*[(((flash-backward-sexp (start-pos exact-integer?)) void?))]{
    Flashes the parenthesis that opens the sexpression at @racket[start-pos].
  }

  @defmethod*[(((backward-sexp (start-pos exact-integer?)) void?))]{
    Move the caret backwards one sexpression

    Moves the caret to the beginning of the sexpression that ends at
    @racket[start-pos].
  }

  @defmethod*[(((find-up-sexp (start-pos exact-integer?))
                (or/c #f exact-integer?)))]{
    Returns the position of the beginning of the next sexpression outside the
    sexpression that contains @racket[start-pos].  If there is no such
    sexpression, it returns @racket[#f].
  }

  @defmethod*[(((up-sexp (start exact-integer?)) void?))]{
    Moves backward out of the S-expression containing the position
    @racket[start].
  }

  @defmethod*[(((find-down-sexp (start-pos exact-integer?))
                (or/c #f exact-integer?)))]{
    Returns the position of the beginning of the next sexpression inside the
    sexpression that contains @racket[start-pos].  If there is no such
    sexpression, it returns @racket[#f].
  }

  @defmethod*[(((down-sexp (start exact-integer?)) void?))]{
    Moves forward into the next S-expression after the position @racket[start].
  }

  @defmethod*[(((remove-parens-forward (start exact-integer?)) void?))]{
    Removes the parentheses from the S-expression starting after the position
    @racket[start].
  }

  @defmethod*[(((select-forward-sexp) void?))]{
    Selects the next S-expression, starting at the start of the current selection.
  }

  @defmethod*[(((select-backward-sexp) void?))]{
    Selects the previous S-expression, starting at the start of the current selection.
  }

  @defmethod*[(((select-up-sexp) void?))]{
    Selects the region to the enclosing S-expression, starting at the start of the current selection.
  }

  @defmethod*[(((select-down-sexp) void?))]{
    Selects the region to the next contained S-expression, starting at the
    start of the current selection.
  }

  @defmethod*[(((transpose-sexp (start exact-integer?)) void?))]{
    Swaps the S-expression beginning before the position @racket[start] with
    the next S-expression following @racket[start].
  }

  @defmethod*[(((mark-matching-parenthesis (pos exact-positive-integer?)) void?))]{
    If the paren after @racket[pos] is matched, this method highlights it and
    its matching counterpart in dark green.
  }

  @defmethod*[(((get-tab-size) exact-integer?))]{
    This method returns the current size of the tabs for scheme mode.
    See also @method[racket:text<%> set-tab-size].
  }

  @defmethod*[(((set-tab-size (new-size exact-integer?)) void?))]{
    This method sets the tab size for this text.
  }

  @defmethod*[(((introduce-let-ans (start-pos exact-integer?)) void?))]{
    Adds a let around the current s-expression and a printf into the
    body of the let.
  }

  @defmethod*[(((move-sexp-out (start-pos exact-integer?)) void?))]{
    Replaces the sexpression surrounding the insertion point with the
    sexpression following the insertion point.
  }
}
@defmixin[racket:text-mixin
          (text:basic<%> mode:host-text<%> color:text<%> text:autocomplete<%>)
          (racket:text<%>)]{
  This mixin adds functionality for editing Racket files.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.
  @defmethod*[#:mode override
              (((get-word-at (pos exact-positive-integer?)) string?))]{
    Returns the word just before @racket[pos], which is then used as the prefix
    for auto-completion.
  }

  @defmethod[#:mode override
             (get-start-of-line [pos exact-nonnegative-integer?])
             exact-nonnegative-integer?]{
     Returns the first non-whitespace character in the paragraph containing
     @racket[pos], unless the position is already there, in which case it
     returns the first position of the paragraph.
  }
}

@definterface[racket:text-mode<%> ()]{
  The result of @racket[racket:text-mode-mixin] implements this
  interface.
}

@defmixin[racket:text-mode-mixin
          (color:text-mode<%> mode:surrogate-text<%>)
          (racket:text-mode<%>)]{
  This mixin adds Racket mode functionality to the mode that it is mixed into.
  The resulting mode assumes that it is only set to an editor that is the
  result of @racket[racket:text-mixin].

  @defmethod*[#:mode override (((on-disable-surrogate) void?))]{
    Removes the scheme keymap (see also @racket[racket:get-keymap]) and
    disables any parenthesis highlighting in the host editor.
  }

  @defmethod*[#:mode override (((on-enable-surrogate) void?))]{
    Adds the scheme keymap (see also @racket[racket:get-keymap]) and enables a
    parenthesis highlighting in the host editor.
  }
}

@defmixin[racket:set-mode-mixin (racket:text<%> mode:host-text<%>) ()]{
  This mixin creates a new instance of @racket[racket:text-mode%] and installs
  it, by calling its own @method[mode:host-text<%> set-surrogate] method with
  the object.
}

@defclass[racket:text%
          (racket:set-mode-mixin
           (racket:text-mixin
            (text:autocomplete-mixin (mode:host-text-mixin color:text%))))
          ()]{}

@defclass[racket:text-mode% (racket:text-mode-mixin color:text-mode%) ()]{}

@(include-previously-extracted "main-extracts.rkt" #rx"^racket:")
