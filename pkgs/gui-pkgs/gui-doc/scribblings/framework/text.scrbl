#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Text}

@definterface[text:basic<%> (editor:basic<%> text%)]{
  Classes matching this interface are expected to implement the basic
  functionality needed by the framework.
  @defmethod[(highlight-range [start exact-nonnegative-integer?]
                              [end exact-nonnegative-integer?]
                              [color (or/c string? (is-a?/c color%))]
                              [caret-space boolean? #f]
                              [priority (or/c 'high 'low) 'low]
                              [style (or/c 'rectangle 'ellipse 'hollow-ellipse 'dot) 'rectangle]
                              [#:adjust-on-insert/delete adjust-on-insert/delete boolean? #f]
                              [#:key key any/c #f])
             (if adjust-on-insert/delete
                 void?
                 (-> void?))]{
    This function highlights a region of text in the buffer.

    The range between @racket[start] and @racket[end] will be highlighted with
    the color in color, if the style is @racket['rectangle] (the default).  If
    the style is @racket['ellipse], then an ellipse is drawn around the range
    in the editor, using the color.  If the style is @racket['hollow-ellipse],
    then the outline of an ellipse is drawn around the range in the editor,
    using the color.

    If the style is @racket['dot], then @racket[start] and @racket[end] must be
    the same, and a dot is drawn at the bottom of that position in the editor.

    If @racket[caret-space?] is not @racket[#f], the left edge of the range
    will be one pixel short, to leave space for the caret. The caret does not
    interfere with the right hand side of the range. Note that under some
    platforms, the caret is drawn with XOR, which means almost anything can
    happen. So if the caret is in the middle of the range it may be hard to
    see, or if it is on the left of the range and @racket[caret-space?] is
    @racket[#f] it may also be hard to see.

    The @racket[priority] argument indicates the relative priority for drawing
    overlapping regions. If two regions overlap and have different priorities,
    the region with @racket['high] priority will be drawn second and only it
    will be visible in the overlapping region.

    If @racket[adjust-on-insert/delete?] is @racket[#t], then insertions
    and deletions to the text will adjust the @racket[start] and @racket[end]
    of the range. Insertions and deletions before the range move the range forward
    and backward; insertions and deletions after the range will be ignored. An insertion
    in the middle of the range will enlarge the range and a deletion that overlaps
    the range adjusts the range to reflect the deleted portion of the range and its
    new position.
    
    The @racket[key] argument can be used with 
    @method[text:basic<%> unhighlight-ranges/key]
    and 
    @method[text:basic<%> unhighlight-ranges]
    to identify ranges whose start and end positions may have changed.
    Symbols whose names begin with @litchar{plt:} are reserved
    for internal use.
    
    If this method returns a thunk, invoking the thunk will turn off the
    highlighting from this range.

    Note that if @racket[adjust-on-insert/delete] is a true value, then
    the result is not a thunk and instead 
    @method[text:basic<%> unhighlight-range],
    @method[text:basic<%> unhighlight-ranges/key], or
    @method[text:basic<%> unhighlight-ranges]
    must be called directly to remove the highlighting.
  }

  @defmethod[(unhighlight-range
              (start exact-nonnegative-integer?)
              (end exact-nonnegative-integer?)
              (color (or/c string? (is-a?/c color%)))
              (caret-space boolean? #f)
              (style (or/c 'rectangle 'ellipse 'hollow-ellipse) 'rectangle))
             void?]{
    This method removes the highlight from a region of text in the buffer.

    The region must match up to a region specified from an earlier call to
    @method[text:basic<%> highlight-range].
    
    This method does a linear scan over all of the regions currently set.
    If you expect to call this method many times (when there are many 
    ranges set)
    consider instead calling @method[text:basic<%> unhighlight-ranges].
  }

  @defmethod[(unhighlight-ranges/key [key any/c]) void?]{
    This method removes the highlight from regions in the buffer
    that have the key @racket[key] 
    (as passed to @method[text:basic<%> highlight-range]).
  }

  @defmethod[(unhighlight-ranges
              [pred? (-> exact-nonnegative-integer?
                         exact-nonnegative-integer?
                         (is-a?/c color%)
                         boolean?
                         (or/c 'rectangle 'ellipse 'hollow-ellipse)
                         (or/c boolean? exact-nonnegative-integer?)
                         any/c
                         boolean?)])
               void?]{
    This method removes the highlight from regions in the buffer as 
    selected by @racket[pred?]. The arguments to @racket[pred?] are the
    same as the arguments to 
    @method[text:basic<%> highlight-range] when it was originally called,
    unless the @racket[_adjust-on-insert/delete] argument was a true value, in which case the 
    first two arguments to the predicate will reflect the current state
    of the bubble, if it is changed.

  }

  @defmethod*[(((get-highlighted-ranges) (listof text:range?)))]{
    Returns a list of (opaque) values representing the active ranges in the
    editor.
  }

  @defmethod*[(((get-styles-fixed) boolean?))]{
    If the result of this function is @racket[#t], the styles in this
    @racket[text:basic<%>] will be fixed. This means that any text inserted to
    this editor has its style set to this editor's @racket[style-list%]'s
    @racket["Standard"] style.

    See also @method[text:basic<%> set-styles-fixed].
  }

  @defmethod*[(((get-fixed-style) (is-a?/c style<%>)))]{
    Returns the style used by @method[text:basic<%> set-styles-fixed]when
    setting the styles.
  }

  @defmethod*[(((set-styles-fixed (fixed? boolean?)) void?))]{
    Sets the styles fixed parameter of this @racket[text%]. See also
    @method[text:basic<%> get-styles-fixed] and @method[text:basic<%>
    get-fixed-style].
  }

  @defmethod[(move/copy-to-edit [dest-text (is-a?/c text%)]
                                [start exact-integer?]
                                [end exact-integer?]
                                [dest-pos exact-integer?]
                                [#:try-to-move? try-to-move? boolean? #t])
             void?]{
    This moves or copies text and snips to another edit.

    Moves or copies from the edit starting at @racket[start] and ending at
    @racket[end]. It puts the copied text and snips in @racket[dest-text]
    starting at location @racket[dest-pos].

    If @racket[try-to-move] is @racket[#t], then the snips are removed;
    and if it is @racket[#f], then they are copied.
    
    If a snip refused to be moved, it will be copied and deleted from the editor,
    otherwise it will be moved. A snip may refuse to be moved by returning
    @racket[#f] from @method[snip% release-from-owner].
  }

  @defmethod*[(((initial-autowrap-bitmap) (or/c #f (is-a?/c bitmap%))))]{
    The result of this method is used as the initial autowrap bitmap. Override
    this method to change the initial @racket[bitmap%]. See also @method[text%
    set-autowrap-bitmap]

    Returns the result of @racket[icon:get-autowrap-bitmap] by default.
  }

  @defmethod*[(((get-port-name) (or/c path-string? symbol? #f)))]{
    The result of this method is a symbol that identifies this editor and that
    is used as the port-name of a port that is read from this editor if this
    editor is used in DrRacket.  See also @method[text:basic<%>
    port-name-matches?].
  }

  @defmethod[(port-name-matches? (id any/c)) boolean?]{
    Indicates if @racket[id] matches the port name of this file. If the file is
    saved, the port name matches when the save file is the path as
    @racket[id]. If the file has not been saved, the port name matches if the
    symbol is the same as the result of @method[text:basic<%> get-port-name].

    This method calls @racket[normalize-path] and thus can be very expensive on
    some filesystems. If it is called many times in a loop, cache the results
    to avoid calling it too often.
  }

  @defmethod[(get-edition-number) exact-nonnegative-integer?]{
     Returns a number that increments every time something in the editor
     changes.

     The number is updated in @xmethod[text% after-insert] and
     @xmethod[text% after-delete].
  }

  @defmethod[(get-start-of-line [pos exact-nonnegative-integer?]) exact-nonnegative-integer?]{
    This method is used by @racket[keymap:setup-global] to implement a
    keybinding for the @racket["home"] key and for @racket["c:a"].

    Its default implementation is
    @racket[(#,(method text% line-start-position) (#,(method text% position-line) pos))].
  }
}

@defmixin[text:basic-mixin (editor:basic<%> text%) (text:basic<%>)]{
  This mixin implements the basic functionality needed for @racket[text%]
  objects in the framework.

  The class that this mixin produces uses the same initialization arguments as
  its input.

  @defmethod*[#:mode override (((on-paint (before? any/c)
                                          (dc (is-a?/c dc<%>))
                                          (left real?)
                                          (top real?)
                                          (right real?)
                                          (bottom real?)
                                          (dx real?)
                                          (dy real?)
                                          (draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret))) 
                                void?))]{
    Draws the rectangles installed by @method[text:basic<%> highlight-range].
  }

  @defmethod*[#:mode augment (((on-insert (start exact-nonnegative-integer?) (end exact-nonnegative-integer?)) void?))]{
    See @method[text:basic<%> set-styles-fixed].
  }

  @defmethod*[#:mode augment (((after-insert (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    See @method[text:basic<%> set-styles-fixed].
  }

  @defmethod*[#:mode override (((put-file (directory (or/c path? #f)) (default-name string?)) (or/c path? #f)))]{
    Like @method[editor<%> put-file] but uses @racket[finder:put-file] instead
    of @racket[put-file].
  }
}

@definterface[text:line-spacing<%> (text:basic<%>)]{
   Objects implementing this interface adjust their
   spacing based on the @racket['framework:line-spacing-add-gap?]
   preference.
}

@defmixin[text:line-spacing-mixin (text:basic<%>) (text:line-spacing<%>)]{
  Calls @method[text% set-line-spacing] to either @racket[0] or @racket[1]
  when an object is created, based
  on the @racket['framework:line-spacing-add-gap?]
  preference.
  
  Also registers a callback (via @racket[preferences:add-callback]) to call
  @method[text% set-line-spacing] when the @racket['framework:line-spacing-add-gap?]
  preference changes.
}

@definterface[text:first-line<%> (text%)]{

  Objects implementing this interface, when @method[text:first-line<%>
  highlight-first-line] is invoked with @racket[#t], always show their first
  line, even with scrolled (as long as @method[text:first-line<%>
  first-line-currently-drawn-specially?]  returns @racket[#t]).

  @defmethod[#:mode public-final (highlight-first-line [on? boolean?]) void?]{
    Call this method to enable special treatment of the first line in the editor.
  }

  @defmethod[#:mode public-final (first-line-currently-drawn-specially?) boolean?]{
    Returns @racket[#t] if @method[text:first-line<%> is-special-first-line?]
    returned @racket[#t] for the current first line and if the buffer is
    scrolled down so that the first line would not (ordinarily) be visible.
  }

  @defmethod[#:mode public-final (get-first-line-height) number?]{
    Returns the height, in pixels, of the first line.
  }

  @defmethod[(is-special-first-line? [line string?]) boolean?]{
    Override this method to control when the first line is always visible. The
    argument is the first line, as a string.
  }
}

@defmixin[text:first-line-mixin (text%) (text:first-line<%>)]{
  Provides the implementation of @racket[text:first-line<%>].  Does so by just
  painting the text of the first line over top of what is already there and
  overriding @method[text:first-line-mixin scroll-editor-to] to patch up
  scrolling and @method[text:first-line-mixin on-event] to patch up mouse
  handling.

  @defmethod[#:mode override
             (on-paint [before? any/c]
                       [dc (is-a?/c dc<%>)]
                       [left real?]
                       [top real?]
                       [right real?]
                       [bottom real?]
                       [dx real?]
                       [dy real?]
                       [draw-caret (one-of/c 'no-caret 'show-inactive-caret 'show-caret)])
             void?]{
    Based on the various return values of the methods in
    @racket[text:first-line], draws the first actual line of the editor over
    top of the first visible line in the editor.
  }

  @defmethod[#:mode override
             (on-event [event (is-a?/c mouse-event%)])
             void?]{
    Clicks in the first line cause the editor to scroll to the actual first
    line.
  }

  @defmethod[#:mode override
             (scroll-editor-to [localx real?]
                               [localy real?]
                               [width (and/c real? (not/c negative?))]
                               [height (and/c real? (not/c negative?))]
                               [refresh? any/c]
                               [bias (one-of/c 'start 'end 'none)])
             void?]{
    Scrolls a little bit more, when a scroll would be requested that scrolls
    something so that it is line underneath the first line.
  }
}

@definterface[text:foreground-color<%> (text:basic<%> editor:standard-style-list<%>)]{
}

@defmixin[text:foreground-color-mixin (text:basic<%> editor:standard-style-list<%>) (text:foreground-color<%>)]{
  This mixin changes the default text style to have the foreground color
  controlled by @racket[editor:set-default-font-color].

  @defmethod*[#:mode override (((default-style-name) string?))]{
    Returns the result of @racket[editor:get-default-color-style-name].
  }

  @defmethod*[#:mode override (((get-fixed-style) (is-a?/c style<%>)))]{
    Returns the style named by @racket[editor:get-default-color-style-name].
  }
}

@definterface[text:hide-caret/selection<%> (text:basic<%>)]{
  This class hides the caret, except when the selection is active.

  Instances of this class are useful for editors that used for displaying
  purposes, but still allow users to copy their text.
}

@defmixin[text:hide-caret/selection-mixin (text:basic<%>) (text:hide-caret/selection<%>)]{
  @defmethod*[#:mode augment (((after-set-position) void?))]{
    Calls @method[text% hide-caret] to hide the caret when there is only a
    caret and no selection.
  }
}

@definterface[text:nbsp->space<%> (text%)]{
  Classes that implement this interface silently change non-breaking spaces, ie
  the character @racket[(integer->char 160)], to regular spaces when inserted
  into the editor.
}

@defmixin[text:nbsp->space-mixin (text%) (text:nbsp->space<%>)]{
  @defmethod*[#:mode augment (((on-insert (start exact-nonnegative-integer?) (end exact-nonnegative-integer?)) void?))]{
    Starts an edit-sequence by calling @method[editor<%> begin-edit-sequence].
  }

  @defmethod*[#:mode augment (((after-insert (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    Replaces all non-breaking space characters @racket[(integer->char 160)] by
    @racket[#\space] characters.

    Ends the edit sequence (by calling @method[editor<%> end-edit-sequence])
    started in @method[text:nbsp->space-mixin on-insert].
  }
}

@definterface[text:column-guide<%> (text%)]{
  Classes that implement this interface show a vertical line
  at a specified column width (when the content in the
  text has any lines wider than that column width).
  
  The column width is determined by the @racket['framework:column-guide-width]
  preference; that preference is a list of length two where the
  first element is a boolean indicating if the line should be
  visible at all, and the second is the width where the line
  would be visible (if the first is @racket[#t]).
  
  The position of the line is determined by taking the
  width of the @litchar{x} character in the @racket["Standard"]
  style (or, if there is no @racket["Standard"] style, then
  the @racket["Basic"] style) and multiplying that by the
  preference value.
  
}

@defmixin[text:column-guide-mixin (text%) (text:column-guide<%>)]{
 @defmethod[#:mode extend (on-paint [before? any/]
                                    [dc (is-a?/c dc<%>)]
                                    [left real?]
                                    [top real?]
                                    [right real?]
                                    [bottom real?]
                                    [dx real?]
                                    [dy real?]
                                    [draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret
                                                      (cons/c exact-nonnegative-integer?
                                                              exact-nonnegative-integer?))])
                   void?]{
    Draws the column guide (if appropriate; see @racket[text:column-guide<%>]).
  }
 @defmethod[#:mode augment (on-change) void?]{
   Checks to see if any of the state that would cause the line to draw
   in a different place has changed (via calls to @method[editor<%> get-extent] and
   @method[text% get-padding]; if so makes (up to) two calls to
   @method[editor<%> invalidate-bitmap-cache] with rectangles that cover the
   old and new locations of the line.
 }
}

@definterface[text:normalize-paste<%> (text:basic<%>)]{
  @defmethod[(ask-normalize?) boolean?]{
    Prompts the user if the pasted text should be normalized (and updates
    various preferences based on the response).

    Override this method in the mixin to avoid all GUI and preferences
    interactions.
  }

  @defmethod[(string-normalize [s string?]) string?]{
    Normalizes @racket[s]. Defaults to:
    @racketblock[(regexp-replace* #rx"\u200b"
                                  (string-normalize-nfkc s)
                                  "")]
  }

}

@defmixin[text:normalize-paste-mixin (text:basic<%>) (text:normalize-paste<%>)]{
  @defmethod[#:mode override
             (do-paste [start exact-nonnegative-integer?] [time exact-integer?])
             void?]{
    Overridden to detect when insertions are due to pasting. Sets some internal
    state and calls the super.
  }

  @defmethod[#:mode augment (on-insert [start exact-nonnegative-integer?] [len exact-nonnegative-integer?]) void?]{
    Calls @method[editor<%> begin-edit-sequence].
  }

  @defmethod[#:mode augment (after-insert [start exact-nonnegative-integer?] [len exact-nonnegative-integer?]) void?]{
    Normalizes any next text and calls @method[editor<%> end-edit-sequence].
  }
}

@definterface[text:searching<%> (editor:keymap<%> text:basic<%>)]{
  Any object matching this interface can be searched.

  @defmethod[(set-searching-state [str (or/c #f string?)]
                                  [cs? boolean?]
                                  [replace-mode? boolean?]
                                  [notify-frame? boolean?])
             void?]{
    If @racket[str] is not @racket[#f], then this method initiates a search for
    every occurrence of @racket[str] in the editor. If @racket[str] is @racket[#f],
    then it clears all of the search highlighting in the buffer.

    If @racket[cs?] is @racket[#f], the search is case-insensitive, and otherwise
    it is case-sensitive.

    The @racket[replace-mode?] boolean determines if the resulting search should
    be tracking the next-to-replace search hit as the insertion point moves
    around in the editor. Also, when @racket[replace-mode?] is @racket[#f], then
    the bubbles are are uniform medium purple color (@racket["plum"] in
    @racket[the-color-database]) and otherwise they are either a lighter
    purple or a darker purple, with every bubble except the one just following
    the insertion the lighter color.

    The search does not complete before @method[text:searching<%> set-searching-state]
    returns. Accordingly, @method[text:searching<%> get-search-hit-count] may
    have out-of-date results for a while, until the search process is finished.
    If @racket[notify-frame?] is @racket[#t] then 
    @method[frame:searchable<%> search-hits-changed]
    is called when the search completes.
  }

  @defmethod[(set-search-anchor [position (or/c #f number?)]) void?]{
    Sets the anchor's position in the editor. Only takes effect if the
    @racket['framework:anchored-search] preference is on.
  }

  @defmethod[(get-search-hit-count) number?]{
    Returns the number of hits for the search in the buffer, based on the count
    found last time that a search completed.
  }

  @defmethod[(get-replace-search-hit) (or/c number? #f)]{
    Returns the position of the nearest search hit that comes after the
    insertion point.
  }

  @defmethod[(set-replace-start [pos (or/c number? #f)]) void?]{
    This method is ignored. (The next replacement start is now
    tracked via the @method[text% after-set-position] method.)
  }

  @defmethod[(get-search-bubbles)
             (listof (list/c (cons/c number? number?)
                             (or/c 'normal-search-color
                                   'dark-search-color
                                   'light-search-color)))]{
    Returns information about the search bubbles in the editor. Each item in
    the outermost list corresponds to a single bubble. The pair of numbers is
    the range of the bubble and the symbol is the color of the
    bubble.

    This method is intended for use in test suites.
  }
}

@defmixin[text:searching-mixin (editor:keymap<%> text:basic<%>) (text:searching<%>)]{
  This @racket[text%] can be searched.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.

  @defmethod*[#:mode override (((get-keymaps) (listof (is-a?/c keymap%))))]{
    This returns a list containing the super-class's keymaps, plus the result
    of @racket[keymap:get-search].
  }

  @defmethod[#:mode augment (after-insert [start exact-nonnegative-integer?] [len exact-nonnegative-integer?]) void?]{
    Re-does any search now that the contents of the window have changed.
  }

  @defmethod[#:mode augment (after-delete [start exact-nonnegative-integer?] [len exact-nonnegative-integer?]) void?]{
    Re-does any search now that the contents of the window have changed.
  }

  @defmethod[#:mode override (on-focus [on? boolean?]) void?]{
    Tells the frame containing the editor to search based on this editor via
    the @method[frame:searchable<%> set-text-to-search] method.
  }
}

@definterface[text:return<%> (text%)]{
  Objects supporting this interface were created by @racket[text:return-mixin].
}

@defmixin[text:return-mixin (text%) (text:return<%>)]{
  Use this buffer to perform some special action when return is typed.

  @defconstructor[((return (-> boolean?)))]{
  }

  @defmethod*[#:mode override
              (((on-local-char (event (is-a?/c key-event%))) void?))]{
    If @racket[key] is either return or newline, only invoke the
    @racket[return] thunk (initialization argument) and do nothing else.
  }
}

@definterface[text:wide-snip<%> (text:basic<%>)]{

  @defmethod*[(((add-wide-snip (snip (is-a?/c snip%))) void?))]{
    Registers a snip in this editor to be resized when its viewing area
    changes. Ensures the snip is as wide as the viewing area.

    This method should only be called by
    @xmethod[canvas:wide-snip<%> add-wide-snip].
  }

  @defmethod*[(((add-tall-snip (snip (is-a?/c snip%))) void?))]{
    Registers a snip in this editor. It is resized when the viewing area of the
    editor changes.

    This method should only be called by
    @xmethod[canvas:wide-snip<%> add-tall-snip].
  }
}

@defmixin[text:wide-snip-mixin (text:basic<%>) (text:wide-snip<%>)]{
}

@definterface[text:delegate<%> (text:basic<%>)]{
  Implementations of this interface copy all of the changes to this editor to
  the result of @method[text:delegate<%> get-delegate] except instead of
  regular string and tab snips, instead instances of
  @racket[text:1-pixel-string-snip%] and @racket[text:1-pixel-tab-snip%] are
  created.

  The contents of the two editor are kept in sync, as modifications to this
  object happen.

  @defmethod*[(((get-delegate) (or/c #f (is-a?/c text%))))]{
    The result of this method is the @racket[text%] object that the contents of
    this editor are being delegated to, or @racket[#f], if there is none.
  }

  @defmethod*[(((set-delegate (delegate (or/c #f (is-a?/c text%)))) void?))]{
    This method sets the current delegate.

    When it is set, all of the snips are copied from this object to
    @racket[delegate]. Additionally, if this object implements
    @racket[racket:text<%>] the tab settings of @racket[delegate] are updated
    to match this objects.
  }
}

@defclass[text:1-pixel-string-snip% string-snip% ()]{
  This class re-uses the implementation of @racket[string-snip%] to implement a
  string snip that just draws a single pixel for each character in the string.

  See also @racket[text:1-pixel-tab-snip%] for a similar extension to the
  @racket[tab-snip%] class.

  This snip is used in conjunction with the @racket[frame:delegate<%>] and
  @racket[text:delegate<%>] interfaces.

  @defmethod*[#:mode override (((split (position exact-nonnegative-integer?)
                                       (first (box/c (is-a?/c snip%)))
                                       (second (box/c (is-a?/c snip%))))
                                void?))]{
    Fills the boxes with instance of @racket[text:1-pixel-string-snip%]s.
  }

  @defmethod*[#:mode override (((copy) (is-a?/c snip%)))]{
    Creates and returns an instance of @racket[text:1-pixel-string-snip%].
  }

  @defmethod*[#:mode override
              (((get-extent
                 (dc (is-a?/c dc<%>))
                 (x real?) (y real?)
                 (w (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                 (h (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                 (descent (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                 (space (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                 (lspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f)
                 (rspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f))
                void?))]{

    Sets the descent, space, lspace, and rspace to zero. Sets the height to
    1. Sets the width to the number of characters in the string.
  }

  @defmethod*[#:mode override (((insert (s string?) (len exact-nonnegative-integer?) (pos exact-nonnegative-integer? 0)) void?))]{
  }

  @defmethod*[#:mode override (((draw (dc (is-a?/c dc<%>))
                                      (x real?) 
                                      (y real?)
                                      (left real?)
                                      (top real?)
                                      (right real?)
                                      (bottom real?)
                                      (dx real?)
                                      (dy real?)
                                      (draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret)))
                                void?))]{
    Draws black pixels for non-whitespace characters and draws nothing for
    whitespace characters.
  }
}

@defclass[text:1-pixel-tab-snip% tab-snip% ()]{
  This class re-uses the implementation of @racket[tab-snip%] to implement a
  string snip that is always one pixel high.

  See also @racket[text:1-pixel-string-snip%] for a similar extension to the
  @racket[string-snip%] class.

  This snip is used in conjunction with the @racket[frame:delegate<%>] and
  @racket[text:delegate<%>] interfaces.

  @defmethod*[#:mode override (((split (position exact-nonnegative-integer?)
                                       (first (box/c (is-a?/c snip%)))
                                       (second (box/c (is-a?/c snip%))))
                                void?))]{
    Fills the boxes with instance of @racket[text:1-pixel-tab-snip%]s.
  }

  @defmethod*[#:mode override (((copy) (is-a?/c snip%)))]{
    Creates and returns an instance of @racket[text:1-pixel-tab-snip%].
  }

  @defmethod*[#:mode override (((get-extent (dc (is-a?/c dc<%>))
                                            (x real?)
                                            (y real?)
                                            (w (or/c (box/c (and/c real? (not/c negative?)) #f)) #f)
                                            (h (or/c (box/c (and/c real? (not/c negative?)) #f)) #f)
                                            (descent (or/c (box/c (and/c real? (not/c negative?)) #f)) #f)
                                            (space (or/c (box/c (and/c real? (not/c negative?)) #f)) #f)
                                            (lspace (or/c (box/c (and/c real? (not/c negative?)) #f)) #f)
                                            (rspace (or/c (box/c (and/c real? (not/c negative?)) #f)) #f))
                                void?))]{
    Sets the descent, space, lspace, and rspace to zero. Sets the height to
    1. Sets the width to the width of tabs as returned in the
    @racket[tab-width] parameter of the @method[text% get-tabs] method.
  }

  @defmethod*[#:mode override (((draw (dc (is-a?/c dc<%>))
                                      (x real?)
                                      (y real?)
                                      (left real?)
                                      (top real?)
                                      (right real?)
                                      (bottom real?)
                                      (dx real?)
                                      (dy real?)
                                      (draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret)))
                                void?))]{
    Draws nothing.
  }
}

@defmixin[text:delegate-mixin (text:basic<%>) (text:delegate<%>)]{
  This mixin provides an implementation of the @racket[text:delegate<%>]
  interface.

  @defmethod*[#:mode override (((highlight-range (start exact-integer?)
                                 (end exact-nonnegative-integer?)
                                 (color (or/c string? (is-a?/c color%)))
                                 (caret-space boolean? #f)
                                 (priority (or/c 'high 'low) 'low)
                                 (style (or/c 'rectangle 'ellipse 'hollow-ellipse 'dot) 'rectangle))
                                 (-> void?)))]{
    In addition to calling the super method, @method[text:basic<%>
    highlight-range], this method forwards the highlighting to the delegatee.
  }

  @defmethod[#:mode override
             (unhighlight-range
                   (start exact-nonnegative-integer?)
                   (end exact-nonnegative-integer?)
                   (color (or/c string? (is-a?/c color%)))
                   (caret-space boolean? #f)
                   (style (or/c 'rectangle 'ellipse 'hollow-ellipse) 'rectangle))
               void?]{
    This method propagates the call to the delegate and calls the super method.
  }
  @defmethod*[#:mode override
              (((on-paint (before? any/c)
                          (dc (is-a?/c dc<%>))
                          (left real?)
                          (top real?)
                          (right real?)
                          (bottom real?)
                          (dx real?)
                          (dy real?)
                          (draw-caret (one-of/c 'no-caret 'show-inactive-caret 'show-caret)))
                void?))]{
    Draws a blue region in the delegatee editor that shows where the visible
    region of the delegate editor is.
  }

  @defmethod*[#:mode augment (((on-edit-sequence) void?))]{
    starts an edit sequence in the delegate.
  }

  @defmethod*[#:mode augment (((after-edit-sequence) void?))]{
    ends an edit sequence in the delegate.
  }

  @defmethod*[#:mode override (((resized (snip (is-a?/c snip%)) (redraw-now? boolean?)) void?))]{
    Sends a message to the delegate to update the size of the copied snip, if
    there is one.
  }

  @defmethod*[#:mode augment (((after-insert (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    forwards the change to the delegate
  }

  @defmethod*[#:mode augment (((after-delete (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    forwards the change to the delegate.
  }

  @defmethod*[#:mode augment (((after-change-style (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    forwards the changed style to the delegate.
  }

  @defmethod*[#:mode augment (((on-load-file (filename string?) (format symbol?)) void?))]{
    remembers the filename, for use in
    @method[text:delegate-mixin after-load-file].
  }

  @defmethod*[#:mode augment (((after-load-file (success? boolean?)) void?))]{
    updates the delegate with the new contents of the text.
  }
}

@definterface[text:info<%> (text:basic<%>)]{
  Objects supporting this interface are expected to send information about
  themselves to the frame that is displaying them.
}
@defmixin[text:info-mixin (editor:keymap<%> text:basic<%>) (text:info<%>)]{
  This mixin adds support for supplying information to objects created with
  @racket[frame:info-mixin]. When this @racket[editor:basic<%>] is displayed in
  a frame, that frame must have been created with @racket[frame:info-mixin].

  @defmethod*[#:mode override (((set-anchor (on? any/c)) void?))]{
    Calls the @method[frame:text-info<%> anchor-status-changed] method of the
    frame that is viewing this object. It uses @method[editor<%> get-canvas] to
    get the canvas for this frame, and uses that canvas's
    @racket[top-level-window<%>] as the frame.
  }

  @defmethod*[#:mode override (((set-overwrite-mode (on? any/c)) void?))]{
    Calls the @method[frame:text-info<%> overwrite-status-changed]method of the
    frame that is viewing this object. It uses @method[editor<%> get-canvas] to
    get the canvas for this frame, and uses that canvas's
    @racket[top-level-window<%>] as the frame.
  }

  @defmethod*[#:mode augment (((after-set-position) void?))]{
    Calls the @method[frame:text-info<%> editor-position-changed] method of the
    frame that is viewing this object. It uses @method[editor<%> get-canvas] to
    get the canvas for this frame, and uses that canvas's
    @racket[top-level-window<%>] as the frame.
  }

  @defmethod*[#:mode augment (((after-insert (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    Calls the @method[frame:text-info<%> editor-position-changed] method of the
    frame that is viewing this object. It uses @method[editor<%> get-canvas] to
    get the canvas for this frame, and uses that canvas's
    @racket[top-level-window<%>] as the frame.
  }

  @defmethod*[#:mode augment (((after-delete (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) void?))]{
    Calls the @method[frame:text-info<%> editor-position-changed] method of the
    frame that is viewing this object. It uses @method[editor<%> get-canvas] to
    get the canvas for this frame, and uses that canvas's
    @racket[top-level-window<%>] as the frame.
  }
}

@definterface[text:clever-file-format<%> (text%)]{
  Objects supporting this interface are expected to support a clever
  file format when saving.
}

@defmixin[text:clever-file-format-mixin (text%) (text:clever-file-format<%>)]{
  The result of this mixin uses the same initialization arguments as the
  mixin's argument.

  When files are saved from this @racket[text%], a check is made to see if
  there are any non-@racket[string-snip%] objects in the @racket[text%]. If so,
  it is saved using the file format @racket['std]. (see @method[text%
  set-file-format] for more information. If not, the file format passed to
  @method[editor<%> save-file] is used.

  @defmethod*[#:mode augment (((on-save-file (filename path?)
                                             (format (or/c 'guess 'standard 'text 'text-force-cr 'same 'copy)))
                               void?))]{
    If the method @method[text% get-file-format] returns @racket['standard] and
    the text has only @racket[string-snip%]s, the file format is set to
    @racket['text].

    If the method @method[text% get-file-format] returns @racket['text] and the
    text has some non @racket[string-snip%]s, the file format is set to
    @racket['standard].

    Depending on the user's preferences, the user may also be queried.

    Also, the changes to the file format only happen if the argument
    @racket[file-format] is @racket['copy] or @racket['same].
  }
}

@definterface[text:crlf-line-endings<%> (text%)]{
  Objects supporting this interface use 
  @method[editor<%> use-file-text-mode] to
  change the line ending style under windows. See 
  @method[text:crlf-line-endings-mixin after-load-file] for more information.
}


@defmixin[text:crlf-line-endings-mixin (text%) (text:crlf-line-endings<%>)]{
  @defmethod[#:mode override (after-load-file [success? any/c]) void?]{
    Checks to see if the newly loaded file has any lines terminated with 
    @racket["\n"] (i.e., not @racket["\r\n"]) or if the file is empty. 
    If so, and if the @racket[system-type] returns @racket['windows], then
    this method calls @method[editor<%> use-file-text-mode], passing @racket[#f]. 
    
    Otherwise, calls @method[editor<%> use-file-text-mode] with @racket[#t].
  }
}

@definterface[text:file<%> (editor:file<%> text:basic<%>)]{
  Mixins that implement this interface lock themselves when the file they are
  editing is read only.

  @defmethod*[(((get-read-write?) boolean?))]{
    Indicates whether or not this editor is in read-write mode.
  }

  @defmethod*[(((while-unlocked (thunk (-> any/c))) any/c))]{
    Unlocks the editor, calls the thunk, and then relocks the editor, all using
    a @racket[dynamic-wind].
  }
}

@defmixin[text:file-mixin (editor:file<%> text:basic<%>) (text:file<%>)]{
  @defmethod*[#:mode augment
              (((can-insert? (start exact-nonnegative-integer?)
                             (len exact-nonnegative-integer?))
                boolean?))]{

    Returns false if the result of @method[text:file<%> get-read-write?]  is
    true, otherwise returns the result of calling @racket[inner].
  }

  @defmethod*[#:mode augment (((can-delete? (start exact-nonnegative-integer?) (len exact-nonnegative-integer?)) boolean?))]{
    Returns false if the result of @method[text:file<%> get-read-write?]  is
    true, otherwise returns the result of calling @racket[inner].
  }

  @defmethod*[#:mode augment (((after-save-file) void?))]{
    Checks if the newly saved file is write-only in the filesystem. If so,
    locks the editor with the @method[editor<%> lock] method. Otherwise unlocks
    the buffer

    For each canvas returned from @method[editor<%> get-canvases] it checks to
    see if the @racket[canvas%]'s @method[area<%> get-top-level-window] matches
    the @racket[frame:editor<%>] interface. If so, it calls
    @method[frame:editor-mixin set-label] with the last part of the filename
    (ie, the name of the file, not the directory the file is in).
  }

  @defmethod*[#:mode augment (((after-load-file) void?))]{
    Checks if the newly loaded file is write-only in the filesystem. If so,
    locks the editor with the @method[editor<%> lock] method. Otherwise unlocks
    the buffer

    For each canvas returned from @method[editor<%> get-canvases] it checks to
    see if the @racket[canvas%]'s @method[area<%> get-top-level-window] matches
    the @racket[frame:editor<%>] interface. If so, it calls
    @method[frame:editor-mixin set-label] with the last part of the filename
    (ie, the name of the file, not the directory the file is in).
  }
}

@definterface[text:ports<%> ()]{
  Classes implementing this interface (via the associated mixin) support input
  and output ports that read from and to the editor.

  There are two input ports: the normal input port just reads from the editor's
  contents directly and the box input port inserts an editor snip into this
  text and uses input typed into the box as input into the port.

  There are three output ports, designed to match stdout, stderr, and a special
  port for printing values. The only difference between them is the output is
  rendered in different colors when it comes in via the different ports.

  They create three threads to mediate access to the input and output ports
  (one for each input port and one for all of the output ports).

  @defmethod*[(((delete/io (start exact-integer?) (end exact-integer?)) void?))]{
    Deletes the text between @racket[start] and @racket[end] without changing
    the behavior of the ports (otherwise, deleting the text would break
    internal invariants of the port).

    Both @racket[start] and @racket[end] must be less than
    @method[text:ports<%> get-insertion-point] (or else it is safe to delete
    them so you don't need this method).
  }
  
  @defmethod[(do-submission) void?]{
    Triggers a submission to the input port with what is currently pending 
    in the editor.
  }

  @defmethod*[(((get-insertion-point) exact-integer?))]{
    Returns the position where characters put into the output port will appear.
  }

  @defmethod*[(((set-insertion-point (ip exact-integer?)) void?))]{
    Sets the position where the output port will insert characters.  See also
    @method[text:ports<%> get-insertion-point].
  }

  @defmethod*[(((get-unread-start-point) exact-integer?))]{
    Returns the position where input will be taken into the input port (after
    the next time return is typed).
  }

  @defmethod*[(((set-unread-start-point (usp exact-integer?)) void?))]{
    Sets the position where input will be taken into the input port (after the
    next time return is typed).

    See also @method[text:ports<%> get-unread-start-point].
  }

  @defmethod*[(((set-allow-edits (allow-edits? boolean?)) void?))]{
    Enables or disables editing in the buffer. Be sure to update the unread
    start point (via @method[text:ports<%> set-unread-start-point]) and the
    insertion point (via @method[text:ports<%> set-insertion-point]) after
    making changes to the buffer.
  }

  @defmethod*[(((get-allow-edits) boolean?))]{
    Indicates if editing is allowed in the buffer at this point.
  }

  @defmethod*[(((insert-between (str (or/c (is-a?/c snip%) string?))) void?))]{
    Inserts some text between the unread start point and the insertion point
    (and updates them properly). To insert before the two points, see
    @method[text:ports<%> insert-before].

    See also @method[text:ports<%> set-unread-start-point] and
    @method[text:ports<%> set-insertion-point].
  }

  @defmethod*[(((insert-before (str (or/c (is-a?/c snip%) string?))) void?))]{
    Inserts some text before the unread start point and updates it and the
    insertion point properly. To insert between the two points, see
    @method[text:ports<%> insert-between].

    See also @method[text:ports<%> set-unread-start-point] and
    @method[text:ports<%> set-insertion-point].
  }

  @defmethod*[(((submit-to-port? (key (is-a?/c key-event%))) boolean?))]{
    Augment this method to help control when characters should be submitted to
    the input port.

    Return @racket[#t] or the result of calling @racket[inner].
  }

  @defmethod*[(((on-submit) void?))]{
    This method is called when text is sent into the input port.

    Does nothing.
  }

  @defmethod*[(((send-eof-to-in-port) void?))]{
    This method puts an eof into the input port.
  }

  @defmethod*[(((send-eof-to-box-in-port) void?))]{
    This method puts an eof into the box input port.
  }

  @defmethod*[(((reset-input-box) void?))]{
    This method removes the current input box from the editor (and all input in
    it is lost).
  }

  @defmethod*[(((clear-output-ports) void?))]{
    Flushes all of the data in all of the output ports that hasn't appeared in
    the editor yet.
  }

  @defmethod*[(((clear-input-port) void?))]{
    Flushes all of the data in the input port that hasn't yet been
    read. Reading will now block.
  }

  @defmethod*[(((clear-box-input-port) void?))]{
    Flushes all of the data in the box input port that hasn't yet been
    read. Reading will now block.
  }

  @defmethod*[(((get-out-style-delta) (or/c (is-a?/c style-delta%) string?)))]{
    The result of this method is the style that is used to color text submitted
    to the result of @method[text:ports<%> get-out-port].

    If the result is a string that is not mapped in the editor's style list,
    the style named @racket["Standard"] is used and if that isn't mapped, the
    style named @racket["Basic"] is used.

    This method is called during the initialization of the class.

    By default, returns @racket["text:ports out"] which is mapped to a blue
    style in the style list returned by @racket[editor:get-standard-style-list].
  }

  @defmethod*[(((get-err-style-delta) (or/c (is-a?/c style-delta%) string?)))]{
    The result of this method is the style that is used to color text submitted
    to the result of @method[text:ports<%> get-err-port].

    If the result is a string that is not mapped in the editor's style list,
    the style named @racket["Standard"] is used and if that isn't mapped, the
    style named @racket["Basic"] is used.

    This method is called during the initialization of the class.

    By default, returns @racket["text:ports err"] which is mapped to a red
    italic style in the style list returned by
    @racket[editor:get-standard-style-list].
  }

  @defmethod*[(((get-value-style-delta) (or/c (is-a?/c style-delta%) string?)))]{
    The result of this method is the style (or the name of the style) that is
    used to color text submitted to the result of @method[text:ports<%>
    get-value-port].

    If the result is a string that is not mapped in the editor's style list,
    the style named @racket["Standard"] is used and if that isn't mapped, the
    style named @racket["Basic"] is used.

    This method is called during the initialization of the class.

    By default, returns @racket["text:ports value"] which is mapped to a blue
    style in the style list returned by
    @racket[editor:get-standard-style-list].
  }

  @defmethod*[(((get-in-port) input-port?))]{
    Returns the input port that data in this editor is sent to.
  }

  @defmethod*[(((get-in-box-port) input-port?))]{
    Returns the box input port that data in this editor is sent to.
  }

  @defmethod*[(((get-out-port) output-port?))]{
    Returns an output port that writes into this editor.  The only difference
    between this port and the ports returned by @method[text:ports<%>
    get-err-port] and @method[text:ports<%> get-value-port] is the font style
    and color.
  }

  @defmethod*[(((get-err-port) output-port?))]{
    Returns an output port that writes into this editor.  The only difference
    between this port and the ports returned by @method[text:ports<%>
    get-err-port] and @method[text:ports<%> get-out-port] is the font style and
    color.
  }

  @defmethod*[(((get-value-port) output-port?))]{
    Returns an output port that writes into this editor.  The only difference
    between this port and the ports returned by @method[text:ports<%>
    get-err-port] and @method[text:ports<%> get-out-port] is the font style and
    color.
  }

  @defmethod*[(((after-io-insertion) void?))]{
    This method is called after an insertion due to IO occurs.
  }

  @defmethod*[(((get-box-input-editor-snip%) (subclass?/c editor-snip%)))]{
    The result of this method is used as the class of editor snips that is
    inserted by the box port in this editor.
  }

  @defmethod*[(((get-box-input-text%) (is-a?/c text:input-box<%>)))]{
    The result of this method is instantiated and placed inside the result of
    @method[text:ports<%> get-box-input-editor-snip%].
  }
}

@defmixin[text:ports-mixin (text:wide-snip<%>) (text:ports<%>)]{

  @defmethod*[#:mode augment (((can-insert? (start exact-integer?) (len exact-integer?)) boolean?))]{
    Returns the results of the @racket[inner] call, unless
    @method[text:ports<%> get-allow-edits] returns @racket[#f].
  }

  @defmethod*[#:mode augment (((can-delete? (start exact-integer?) (len exact-integer?)) boolean?))]{
    Returns the results of the @racket[inner] call, unless
    @method[text:ports<%> get-allow-edits] returns @racket[#f].
  }

  @defmethod*[#:mode override (((on-local-char (event (is-a?/c key-event%))) void?))]{
    Sends the data between the last position and the result of
    @method[text:ports<%> get-unread-start-point] to the input port, unless
    @method[text:ports<%> submit-to-port?]  returns @racket[#f].

    Also calls @method[text:ports<%> on-submit].
  }

  @defmethod*[#:mode augment (((on-display-size) void?))]{
    Adjusts the embedded editor-snip (used for reading input to the
    @method[text:ports<%> get-in-box-port]) to match the width of the editor.
  }
}

@definterface[text:input-box<%> (text%)]{
  Classes that implement this interface are used as the editors for the box
  input port in @racket[text:ports%].
}

@defmixin[text:input-box-mixin (text%) (text:input-box<%>)]{
  This mixin provides an implementation of @racket[text:input-box<%>] for use
  with @racket[text:ports<%>].

  @defmethod*[#:mode override
              (((on-default-char (event (is-a?/c key-event%))) void?))]{
    Notifies the @racket[text:ports<%>] enclosing this editor that a new line
    of input has been provided.
  }
}

@definterface[text:autocomplete<%> (text%)]{
  The mixin implementing this interface provides an unintrusive autocompletion
  menu when a particular (configurable) keystroke is pressed.

  @defmethod*[(((auto-complete) void?))]{
    Starts a completion.
  }

  @defmethod*[(((get-autocomplete-border-color) (or/c string? (is-a?/c color%))))]{
    The border color for the autocomplete menu. Defaults to @racket["black"].
  }

  @defmethod*[(((get-autocomplete-background-color) (or/c string? (is-a?/c color%))))]{
    The background color for the non-selected menu items. Defaults to
    @racket["lavender"].
  }

  @defmethod*[(((get-autocomplete-selected-color) (or/c string? (is-a?/c color%))))]{
    The background color for the selected menu item. Defaults to
    @racket[(make-object color% 204 153 255)].
  }

  @defmethod*[(((completion-mode-key-event? (key-event (is-a?/c key-event%))) boolean?))]{
    Returns true when the key event passed to it should initiate the
    completions menu.
  }

  @defmethod*[(((get-all-words) (listof string?)))]{
    Returns the list of the words that autocompletion should
    choose from.
  }

  @defmethod*[(((get-word-at (pos exact-positive-integer?)) string?))]{
    Given an editor location, returns the prefix ending at that location
    that autocompletion should try to complete. 
  }
}

@defmixin[text:autocomplete-mixin (text%) (text:autocomplete<%>)]{

  @defmethod*[#:mode override (((on-paint) void?))]{
    Draws the completion menu (when it is popped up).
  }

  @defmethod*[#:mode override (((on-char) void?))]{
    Takes over the handling of key events when the completions menu is
    visible. Also, when the completions menu is not visible, it calls the
    @method[text:completion<%> completion-mode-key-event?]  method to see if it
    should start completing.
  }

  @defmethod*[#:mode override (((on-event) void?))]{
    This method is overridden to allow mouse access of the completions menu. It
    only handles events when there is a menu open and the mouse is in the menu,
    in which case it makes the menu trace the mouse.

    The only time it does not call the super method is when the mouse is button
    is pushed.
  }
}

@defclass[text:basic% (text:basic-mixin (editor:basic-mixin text%)) ()]{}
@defclass[text:line-spacing% (text:line-spacing-mixin text:basic%) ()]{}
@defclass[text:hide-caret/selection% (text:hide-caret/selection-mixin text:line-spacing%) ()]{}
@defclass[text:nbsp->space% (text:nbsp->space-mixin text:line-spacing%) ()]{}
@defclass[text:normalize-paste% (text:normalize-paste-mixin text:line-spacing%) ()]{}
@defclass[text:delegate% (text:delegate-mixin text:line-spacing%) ()]{}
@defclass[text:wide-snip% (text:wide-snip-mixin text:line-spacing%) ()]{}
@defclass[text:standard-style-list% (editor:standard-style-list-mixin text:wide-snip%) ()]{}
@defclass[text:input-box% (text:input-box-mixin text:standard-style-list%) ()]{}
@defclass[text:keymap% (editor:keymap-mixin text:standard-style-list%) ()]{}
@defclass[text:return% (text:return-mixin text:keymap%) ()]{}
@defclass[text:autowrap% (editor:autowrap-mixin text:keymap%) ()]{}
@defclass[text:file% (text:file-mixin (editor:file-mixin text:autowrap%)) ()]{}
@defclass[text:clever-file-format% (text:clever-file-format-mixin text:file%) ()]{}
@defclass[text:backup-autosave% (editor:backup-autosave-mixin text:clever-file-format%) ()]{}
@defclass[text:searching% (text:searching-mixin text:backup-autosave%) ()]{}
@defclass[text:info% (text:info-mixin (editor:info-mixin text:searching%)) ()]{}

@definterface[text:line-numbers<%> ()]{

  @defmethod*[(((show-line-numbers! (show boolean?)) void?))]{
    Enables or disables line number drawing.
  }

  @defmethod*[(((show-line-numbers?) boolean?))]{
    Returns whether or not line drawing is enabled.
  }

  @defmethod*[(((set-line-numbers-color (color string?)) void?))]{
    Sets the color of the line numbers.
  }
}

@defmixin[text:line-numbers-mixin (text%) (text:line-numbers<%>)]{

  @defmethod*[#:mode override (((on-paint) void?))]{
    Draws the line numbers.
  }

  @defmethod*[(((show-line-numbers! (show boolean?)) void?))]{
    Enables or disables line number drawing.
  }

  @defmethod*[(((show-line-numbers?) boolean?))]{
    Returns whether or not line drawing is enabled.
  }

  @defmethod*[(((set-line-numbers-color (color string?)) void?))]{
    Sets the color of the line numbers.
  }
}

@(include-previously-extracted "main-extracts.rkt" #rx"^text:")
