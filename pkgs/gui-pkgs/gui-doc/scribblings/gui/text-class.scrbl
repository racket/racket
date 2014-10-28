#lang scribble/doc
@(require "common.rkt")

@defclass/title[text% object% (editor<%>)]{

A @racket[text%] object is a standard text editor. A text editor is
 displayed on the screen through an @racket[editor-canvas%] object or
 some other @techlink{display}.


@defconstructor[([line-spacing (and/c real? (not/c negative?)) 1.0]
                 [tab-stops (listof real?) null]
                 [auto-wrap any/c #f])]{

The @racket[line-spacing] argument sets the additional amount of space
 (in DC units) inserted between each line in the editor when the
 editor is displayed. This spacing is included in the reported height
 of each line.

See @method[text% set-tabs] for information about @racket[tabstops].

If @racket[auto-wrap] is true, then auto-wrapping is enabled via
 @method[editor<%> auto-wrap].

A new @racket[keymap%] object is created for the new editor.  See also
 @method[editor<%> get-keymap] and @method[editor<%> set-keymap].

A new @racket[style-list%] object is created for the new editor.  See
 also @method[editor<%> get-style-list] and @method[editor<%>
 set-style-list].

}


@defmethod[#:mode pubment 
           (after-change-style [start exact-nonnegative-integer?]
                               [len exact-nonnegative-integer?])
           void?]{
@methspec{

Called after the style is changed for a given range (and after the
 @techlink{display} is refreshed; use @method[text% on-change-style]
 and @method[editor<%> begin-edit-sequence] to avoid extra refreshes
 when @method[text% after-change-style] modifies the editor).

See also @method[text% can-change-style?] and @method[editor<%>
 on-edit-sequence].

No internals locks are set when this method is called.

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (after-delete [start exact-nonnegative-integer?]
                         [len exact-nonnegative-integer?])
           void?]{
@methspec{

Called after a given range is deleted from the editor (and after the
 @techlink{display} is refreshed; use @method[text% on-delete] and
 @method[editor<%> begin-edit-sequence] to avoid extra refreshes when
 @method[text% after-delete] modifies the editor).

The @racket[start] argument specifies the starting @techlink{position}
 of the deleted range. The @racket[len] argument specifies number of
 deleted @techlink{item}s (so @math{@racket[start]+@racket[len]} is
 the ending @techlink{position} of the deleted range).

See also @method[text% can-delete?] and @method[editor<%>
 on-edit-sequence].

No internals locks are set when this method is called.

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (after-insert [start exact-nonnegative-integer?]
                         [len exact-nonnegative-integer?])
           void?]{
@methspec{

Called after @techlink{item}s are inserted into the editor (and after
 the @techlink{display} is refreshed; use @method[text% on-insert] and
 @method[editor<%> begin-edit-sequence] to avoid extra refreshes when
 @method[text% after-insert] modifies the editor).

The @racket[start] argument specifies the @techlink{position} of the insert. The
 @racket[len] argument specifies the total length (in @techlink{position}s) of
 the inserted @techlink{item}s.

See also @method[text% can-insert?] and @method[editor<%>
 on-edit-sequence].

No internals locks are set when this method is called.

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (after-merge-snips [pos exact-nonnegative-integer?])
           void?]{
@methspec{

Called after adjacent snips in the editor are combined into one.

The @racket[pos] argument specifies the @techlink{position} within the editor
 where the snips were merged (i.e., one old snip was just before
 @racket[pos], one old was just after @racket[pos], and the new snip spans
 @racket[pos]).

See also @method[snip% merge-with].

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (after-set-position)
           void?]{

@methspec{

Called after the start and end @techlink{position} have been moved (but not
 when the @techlink{position} is moved due to inserts or deletes).

See also
@method[editor<%> on-edit-sequence].

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (after-set-size-constraint)
           void?]{

@methspec{

Called after the editor's maximum or minimum height or width is
 changed (and after the @techlink{display} is refreshed; use
 @method[text% on-set-size-constraint] and @method[editor<%>
 begin-edit-sequence] to avoid extra refreshes when @method[text%
 after-set-size-constraint] modifies the editor).

(This callback method is provided because setting an editor's maximum
 width may cause lines to be re-flowed with soft newlines.)

See also @method[text% can-set-size-constraint?] and @method[editor<%>
 on-edit-sequence].

}
@methimpl{

Does nothing.

}}


@defmethod[#:mode pubment 
           (after-split-snip [pos exact-nonnegative-integer?])
           void?]{
@methspec{

Called after a snip in the editor is split into two, either through a
 call to @method[text% split-snip] or during some other action, such
 as inserting.

The @racket[pos] argument specifies the @techlink{position} within the editor
 where a snip was split.

}
@methimpl{

Does nothing.

}}

@defmethod[(call-clickback [start exact-nonnegative-integer?]
                           [end exact-nonnegative-integer?])
           void?]{

Simulates a user click that invokes a clickback, if the given range of
 @techlink{position}s is within a clickback's region. See also
 @|clickbackdiscuss|.

}

@defmethod[#:mode pubment 
           (can-change-style? [start exact-nonnegative-integer?]
                              [len exact-nonnegative-integer?])
           boolean?]{

@methspec{

Called before the style is changed in a given range of the editor. If
 the return value is @racket[#f], then the style change will be
 aborted.

The editor is internally locked for writing during a call to this
 method (see also @|lockdiscuss|). Use @method[text%
 after-change-style] to modify the editor, if necessary.

See also @method[text% on-change-style], @method[text%
 after-change-style], and @method[editor<%> on-edit-sequence].

}
@methimpl{

Returns @racket[#t].

}
}

@defmethod[#:mode pubment 
           (can-delete? [start exact-nonnegative-integer?]
                        [len exact-nonnegative-integer?])
           boolean?]{
@methspec{

Called before a range is deleted from the editor.
If the return value is @racket[#f], then the
delete will be aborted.

The @racket[start] argument specifies the starting @techlink{position}
 of the range to delete. The @racket[len] argument specifies number of
 @techlink{item}s to delete (so @math{@racket[start]+@racket[len]} is
 the ending @techlink{position} of the range to delete).

The editor is internally locked for writing during a call to this method
(see also @|lockdiscuss|). Use
@method[text% after-delete] to modify the editor, if necessary.

See also @method[text% on-delete], @method[text% after-delete], and
 @method[editor<%> on-edit-sequence].

}
@methimpl{

Returns @racket[#t].

}}

@defmethod[#:mode pubment 
           (can-insert? [start exact-nonnegative-integer?]
                        [len exact-nonnegative-integer?])
           boolean?]{
@methspec{

Called before @techlink{item}s are inserted into the editor.  If the
 return value is @racket[#f], then the insert will be aborted.

The @racket[start] argument specifies the @techlink{position} of the potential
 insert. The @racket[len] argument specifies the total length (in
 @techlink{position}s) of the @techlink{item}s to be inserted.

The editor is internally locked for writing during a call to this
 method (see also @|lockdiscuss|). Use @method[text% after-insert] to
 modify the editor, if necessary.

See also @method[text% on-insert], @method[text% after-insert], and
 @method[editor<%> on-edit-sequence].

}
@methimpl{

Returns @racket[#t].

}}


@defmethod[#:mode pubment 
           (can-set-size-constraint?)
           boolean?]{

@methspec{

Called before the editor's maximum or minimum height or width
is changed. If the return value is @racket[#f], then the
change will be aborted.

(This callback method is provided because setting an editor's maximum
width may cause lines to be re-flowed with soft newlines.)

See also @method[text% on-set-size-constraint], @method[text%
 after-set-size-constraint], and @method[editor<%> on-edit-sequence].

}
@methimpl{

Returns @racket[#t].

}}


@defmethod[(caret-hidden?)
           boolean?]{

Returns @racket[#t] if the caret is hidden for this editor or @racket[#f]
otherwise.

See also @method[text% hide-caret].

}


@defmethod*[([(change-style [delta (or/c (is-a?/c style-delta%) #f)]
                            [start (or/c exact-nonnegative-integer? 'start) 'start]
                            [end (or/c exact-nonnegative-integer? 'end) 'end]
                            [counts-as-mod? any/c #t])
              void?]
             [(change-style [style (or/c (is-a?/c style<%>) #f)]
                            [start (or/c exact-nonnegative-integer? 'start) 'start]
                            [end (or/c exact-nonnegative-integer? 'end) 'end]
                            [counts-as-mod? any/c #t])
              void?])]{

Changes the style for a region in the editor by applying a style delta
 or installing a specific style.  If @racket[start] is @racket['start]
 and @racket[end] is @racket['end], then the currently selected
 @techlink{item}s are changed. Otherwise, if @racket[end] is
 @racket['end], then the style is changed from @racket[start] until
 the end of the selection.  If @racket[counts-as-mod?] is @racket[#f],
 then @method[editor<%> set-modified] is not called after applying the
 style change.

To change a large collection of snips from one style to another style,
 consider providing a @racket[style<%>] instance rather than a
 @racket[style-delta%] instance. Otherwise, @method[text%
 change-style] must convert the @racket[style-delta%] instance to the
 @racket[style<%>] instance for every snip; this conversion consumes
 both time and (temporary) memory.

When @racket[style] is provided: @InStyleListNote[@racket[style]]

}


@defmethod[#:mode extend
           (copy [extend? any/c #f]
                 [time exact-integer? 0]
                 [start (or/c exact-nonnegative-integer? 'start) 'start]
                 [end (or/c exact-nonnegative-integer? 'end) 'end])
           void?]{

Copies specified range of text into the clipboard. If @racket[extend?] is
 not @racket[#f], the old clipboard contents are appended. If
 @racket[start] is @racket['start] or @racket[end] is @racket['end], then the
 current selection start/end is used.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}


@defmethod[#:mode override 
           (copy-self-to [dest (or/c (is-a?/c text%) (is-a?/c pasteboard%))])
           void?]{

In addition to the default @xmethod[editor<%> copy-self-to] work,
 this editor's file format, wordbreak function, wordbreak map,
 click-between-threshold, caret visibility state, overwrite mode
 state, and autowrap bitmap are installed into @racket[dest].

}


@defmethod[#:mode override
           (cut [extend? any/c #f]
                [time exact-integer? 0]
                [start (or/c exact-nonnegative-integer? 'start) 'start]
                [end (or/c exact-nonnegative-integer? 'end) 'end])
           void?]{

Copies and then deletes the specified range. If @racket[extend?] is not
 @racket[#f], the old clipboard contents are appended. If @racket[start] is
 @racket['start] or @racket[end] is @racket['end], then the current
 selection start/end is used.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}


@defmethod*[([(delete [start (or/c exact-nonnegative-integer? 'start)]
                      [end (or/c exact-nonnegative-integer? 'back) 'back]
                      [scroll-ok? any/c #t])
              void?]
             [(delete)
              void?])]{

Deletes the specified range or the currently selected text (when no
 range is provided) in the editor. If @racket[start] is
 @racket['start], then the starting selection @techlink{position} is
 used; if @racket[end] is @racket['back], then only the character
 preceding @racket[start] is deleted.  If @racket[scroll-ok?] is not
 @racket[#f] and @racket[start] is the same as the current caret
 @techlink{position}, then the editor's @techlink{display} may be
 scrolled to show the new selection @techlink{position}.


@MonitorMethod[@elem{The content of an editor} @elem{the
 system in response to other method
 calls} @elem{@method[text% on-delete]} @elem{content deletion}]

}

@defmethod[(do-copy [start exact-nonnegative-integer?]
                    [end exact-nonnegative-integer?]
                    [time exact-integer?]
                    [extend? any/c])
           void?]{
@methspec{

Called to copy a region of the editor into the clipboard.  This method
 is provided so that it can be overridden by subclasses.  Do not call
 this method directly; instead, call @method[text% copy].

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}
@methimpl{

Copy the data from @racket[start] to @racket[end], extending the current
 clipboard contexts if @racket[extend?] is not @racket[#f].

}}


@defmethod[(do-paste [start exact-nonnegative-integer?]
                     [time exact-integer?])
           void?]{
@methspec{

Called to paste the current contents of the clipboard into the editor.
 This method is provided so that it can be overridden by subclasses.
 Do not call this method directly; instead, call @method[text% paste].

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}
@methimpl{

Pastes into the @techlink{position} @racket[start].

}}


@defmethod[(do-paste-x-selection [start exact-nonnegative-integer?]
                                 [time exact-integer?])
           void?]{
@methspec{

Called to paste the current contents of the X11 selection on Unix (or the
 clipboard on Windows or Mac OS X) into the editor.  This method is
 provided so that it can be overridden by subclasses.  Do not call
 this method directly; instead, call @method[text% paste-x-selection].

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}
@methimpl{

Pastes into the @techlink{position} @racket[start].

}}


@defmethod[(erase)
           void?]{

Erases the contents of the editor.

See also @method[text% delete].

}

@defmethod[(extend-position [pos exact-nonnegative-integer?]) void?]{
  Updates the selection (see @method[text% set-position]) based on 
  the result of @method[text% get-extend-end-position], 
  @method[text% get-extend-start-position], and @racket[pos].
  
  If @racket[pos] is before the extend start and extend end positions,
  then the selection goes from @racket[pos] to the extend end position.
  If it is after, then the selection goes from the extend start position
  to @racket[pos].
  
  Use this method to implement shift-modified movement keys in order to
  properly extend the selection.
}

@defmethod[(find-line [y real?]
                      [on-it? (or/c (box/c any/c) #f) #f])
           exact-nonnegative-integer?]{

Given a @techlink{location} in the editor, returns the line at the
 @techlink{location}. @|LineNumbering|

@boxisfillnull[@racket[on-it?] @elem{@racket[#t] if the line actually
 touches this @techlink{position}, or @racket[#f] otherwise}] (A large
 enough @racket[y] will always return the last line number, but will
 set @racket[on-it?] to @racket[#f].)

@|OVD| @|FCA|

}


@defmethod[(find-newline [direction (or/c 'forward 'backward) 'forward]
                         [start (or/c exact-nonnegative-integer? 'start) 'start]
                         [end (or/c exact-nonnegative-integer? 'eof) 'eof])
           (or/c exact-nonnegative-integer? #f)]{

Like @method[text% find-string], but specifically finds a paragraph
break (possibly more efficiently than searching text).}


@defmethod[(find-next-non-string-snip [after (or/c (is-a?/c snip%) #f)])
           (or/c (is-a?/c snip%) #f)]{

Given a snip, returns the next snip in the editor (after the given
 one) that is not an instance of @racket[string-snip%]. If
 @racket[#f] is given as the snip, the result is the first non-string
 snip in the editor (if any). If no non-string snip is found after the
 given snip, the result is @racket[#f].

}


@defmethod[(find-position [x real?]
                          [y real?]
                          [at-eol? (or/c (box/c any/c) #f) #f]
                          [on-it? (or/c (box/c any/c) #f) #f]
                          [edge-close? (or/c (box/c real?) #f) #f])
           exact-nonnegative-integer?]{

Given a @techlink{location} in the editor, returns the @techlink{position} at the
 @techlink{location}.

See @|ateoldiscuss| for a discussion of the @racket[at-eol?] argument.
 @boxisfillnull[@racket[on-it?] @elem{@racket[#t] if the line actually touches this
 @techlink{position}, or @racket[#f] otherwise}]

@boxisfillnull[@racket[edge-close?] @elem{it will be filled in with a value
 indicating how close the point is to the vertical edges of the @techlink{item}
 when the point falls on the @techlink{item}}] If the point is closest to the left
 edge of the @techlink{item}, the value will be negative; otherwise, the value
 will be positive. In either case, then absolute value of the returned
 result is the distance from the point to the edge of the @techlink{item}. The
 values 100 and -100 indicate infinity.

@|OVD| @|FCA|

}


@defmethod[(find-position-in-line [line exact-nonnegative-integer?]
                                  [x real?]
                                  [at-eol? (or/c (box/c any/c) #f) #f]
                                  [on-it? (or/c (box/c any/c) #f) #f]
                                  [edge-close? (or/c (box/c real?) #f) #f])
           exact-nonnegative-integer?]{

Given a @techlink{location} within a line of the editor, returns the
 @techlink{position} at the @techlink{location}. @|LineNumbering|

See @|ateoldiscuss| for a discussion of the @racket[at-eol?] argument.
 @boxisfillnull[@racket[on-it?] @elem{@racket[#t] if the line actually
 touches this @techlink{position}, or @racket[#f] otherwise}]

See @method[text% find-position] for a discussion of
 @racket[edge-close?].

@|OVD| @|FCA|

}


@defmethod[(find-snip [pos exact-nonnegative-integer?]
                      [direction (or/c 'before-or-none 'before 'after 'after-or-none)]
                      [s-pos (or/c (box/c exact-nonnegative-integer?) #f) #f])
           (or/c (is-a?/c snip%) #f)]{

Returns the snip at a given @techlink{position}, or @racket[#f] if an appropriate
 snip cannot be found.

If the @techlink{position} @racket[pos] is between
two snips, @racket[direction] specifies which snip to return; @racket[direction]
can be any of the following:
@itemize[

 @item{@racket['before-or-none] --- returns the snip before the
 @techlink{position}, or @racket[#f] if @racket[pos] is @racket[0]}

 @item{@racket['before] --- returns the snip before the @techlink{position},
 or the first snip if @racket[pos] is @racket[0]}

 @item{@racket['after] --- returns the snip after the @techlink{position}, or
 the last snip if @racket[pos] is the last @techlink{position}}

 @item{@racket['after-or-none] -- returns the snip after the
 @techlink{position}, or @racket[#f] if @racket[pos] is the last @techlink{position} or larger}

]

@boxisfillnull[@racket[s-pos] @elem{the @techlink{position} where the returned snip starts}]

}


@defmethod[(find-string [str string?]
                        [direction (or/c 'forward 'backward) 'forward]
                        [start (or/c exact-nonnegative-integer? 'start) 'start]
                        [end (or/c exact-nonnegative-integer? 'eof) 'eof]
                        [get-start? any/c #t]
                        [case-sensitive? any/c #t])
           (or/c exact-nonnegative-integer? #f)]{

Finds an exact-match string in the editor and returns its @techlink{position}. 
 If the string is not found, @racket[#f] is returned.

The @racket[direction] argument can be @racket['forward] or
 @racket['backward], indicating a forward search or backward
 search respectively. In the case of a forward search, the return
 value is the starting @techlink{position} of the string; for a backward search,
 the ending @techlink{position} is returned.  However, if @racket[get-start?] is
 @racket[#f], then the other end of the string @techlink{position} will be
 returned.

The @racket[start] and @racket[end] arguments set the starting and ending
 @techlink{position}s of a forward search (use @racket[start] > @racket[end] for a
 backward search). If @racket[start] is @racket['start], then the search
 starts at the start of the selection. If @racket[end] is @racket['eof],
 then the search continues to the end (for a forward search) or start
 (for a backward search) of the editor.

If @racket[case-sensitive?] is @racket[#f], then an uppercase and lowercase
 of each alphabetic character are treated as equivalent.

}

@defmethod[(find-string-embedded [str string?]
                                 [direction (or/c 'forward 'backward) 'forward]
                                 [start (or/c exact-nonnegative-integer? 'start) 'start]
                                 [end (or/c exact-nonnegative-integer? 'eof) 'eof]
                                 [get-start? any/c #t]
                                 [case-sensitive? any/c #t])
           (or/c exact-nonnegative-integer? 
                 #f
                 (cons/c
                  (is-a?/c editor<%>)
                  (flat-rec-contract
                   nested-editor-search-result
                   (or/c (cons/c (is-a?/c editor<%>)
                                 nested-editor-search-result)
                         exact-nonnegative-integer?))))]{
  Like @method[text% find-string], but also searches in embedded editors,
       returning a series of cons pairs whose @racket[car] positions
       are the editors on the path to the editor where the search
       string occurred and whose final @racket[cdr] position is the 
       search result position.
}
                                                
@defmethod[(find-string-all [str string?]
                            [direction (or/c 'forward 'backward) 'forward]
                            [start (or/c exact-nonnegative-integer? 'start) 'start]
                            [end (or/c exact-nonnegative-integer? 'eof) 'eof]
                            [get-start? any/c #t]
                            [case-sensitive any/c #t])
           (listof exact-nonnegative-integer?)]{

Finds all occurrences of a string using @method[text% find-string]. If
 no occurrences are found, the empty list is returned.  The arguments
 are the same as for @method[text% find-string].

}

@defmethod[(find-string-embedded-all [str string?]
                                     [direction (or/c 'forward 'backward) 'forward]
                                     [start (or/c exact-nonnegative-integer? 'start) 'start]
                                     [end (or/c exact-nonnegative-integer? 'eof) 'eof]
                                     [get-start? any/c #t]
                                     [case-sensitive any/c #t])
           (listof (or/c exact-nonnegative-integer? 
                         (cons/c
                          (is-a?/c editor<%>)
                          (flat-rec-contract
                           nested-editor-search-result
                           (or/c (cons/c (is-a?/c editor<%>)
                                         nested-editor-search-result)
                                 (listof exact-nonnegative-integer?))))))]{
Like @method[text% find-string-embedded], but also searches in embedded
editors, returning search  results a list of the editors that contain
the matches.
}

@defmethod[(find-wordbreak [start (or/c (box/c exact-nonnegative-integer?) #f)]
                           [end (or/c (box/c exact-nonnegative-integer?) #f)]
                           [reason (or/c 'caret 'line 'selection 'user1 'user2)])
           void?]{

Finds wordbreaks in the editor using the current wordbreak procedure.
 See also @method[text% set-wordbreak-func].

The contents of the @racket[start] argument specifies an @techlink{position} to start
 searching backwards to the next word start; its will be filled with
 the starting @techlink{position} of the word that is found.  If @racket[start] is
 @racket[#f], no backward search is performed.

The contents of the @racket[end] argument specifies an @techlink{position} to start
 searching forwards to the next word end; its will be filled with the
 ending @techlink{position} of the word that is found.  If @racket[end] is
 @racket[#f], no forward search is performed.

The @racket[reason] argument specifies more information about what the
 wordbreak is used for. For example, the wordbreaks used to move the
 caret may be different from the wordbreaks used to break lines. The
 possible values of @racket[reason] are:

@itemize[
@item{@racket['caret] --- find a wordbreak suitable for moving the caret}
@item{@racket['line] --- find a wordbreak suitable for breaking lines}
@item{@racket['selection] --- find a wordbreak suitable for selecting the closest word}
@item{@racket['user1] --- for other (not built-in) uses}
@item{@racket['user2] --- for other (not built-in) uses}
]

The actual handling of @racket[reason] is controlled by the current
 wordbreak procedure; see @method[text% set-wordbreak-func]for
 details. The default handler and default wordbreak map treats
 alphanumeric characters the same for @racket['caret], @racket['line],
 and @racket['selection]. Non-alphanumeric, non-space, non-hyphen
 characters do not break lines, but do break caret and selection
 words.  For example a comma should not be counted as part of the
 preceding word for moving the caret past the word or double-clicking
 the word, but the comma should stay on the same line as the word (and
 thus counts in the same ``line word'').

}


@defmethod[(flash-off)
           void?]{

Turns off the hiliting and shows the normal selection range again; see
 @method[text% flash-on]. There is no effect if this method is called
 when flashing is already off.

}


@defmethod[(flash-on [start exact-nonnegative-integer?]
                     [end exact-nonnegative-integer?]
                     [at-eol? any/c #f]
                     [scroll? any/c #t]
                     [timeout exact-nonnegative-integer? 500])
           void?]{

Temporarily hilites a region in the editor without changing the
 current selection.

See @|ateoldiscuss| for a discussion of the @racket[at-eol?] argument. If
 @racket[scroll?] is not @racket[#f], the editor's @techlink{display} will be scrolled
 if necessary to show the hilited region. If @racket[timeout] is greater
 than 0, then the hiliting will be automatically turned off after the
 given number of milliseconds.

See also  @method[text% flash-off].

}


@defmethod[(get-anchor)
           boolean?]{

Returns @racket[#t] if the selection is currently auto-extending. See
 also @method[text% set-anchor].

}

@defmethod[(get-autowrap-bitmap-width) (and/c real? (not/c negative?))]{
  Returns the width of the bitmap last passed to @method[text% set-autowrap-bitmap]
  or @racket[zero?] if no bitmap has been passed to @method[text% set-autowrap-bitmap] or
  if @racket[#f] was most recently passed.
}

@defmethod[(get-between-threshold)
           (and/c real? (not/c negative?))]{

Returns an amount used to determine the meaning of a user click. If
 the click falls within the threshold of a position between two
 @techlink{item}s, then the click registers on the space between the
 @techlink{item}s rather than on either @techlink{item}.

See also @method[text% set-between-threshold].

}


@defmethod[(get-character [start exact-nonnegative-integer?])
           char?]{

Returns the character following the @techlink{position}
 @racket[start]. The character corresponds to getting non-flattened
 text from the editor.

If @racket[start] is greater than or equal to the last
 @techlink{position}, @racket[#\nul] is returned.

}


@defmethod[(get-end-position)
           exact-nonnegative-integer?]{

Returns the ending @techlink{position} of the current selection. See
 also @method[text% get-position].

}

@defmethod[(get-extend-start-position) exact-nonnegative-integer?]{
  Returns the beginning of the ``extend'' region if the selection
  is currently being extended via, e.g., shift and a cursor movement key; 
  otherwise returns the same value as @method[text% get-end-position].
}

@defmethod[(get-extend-end-position) exact-nonnegative-integer?]{
  Returns the beginning of the ``extend'' region if the selection
  is currently being extended via, e.g., shift and a cursor movement key; 
  otherwise returns the same value as @method[text% get-start-position].
}

@defmethod[(get-file-format)
           (or/c 'standard 'text 'text-force-cr)]{

Returns the format of the last file saved from or loaded into this
 editor. See also @method[editor<%> load-file].

}


@defmethod[(get-line-spacing)
           (and/c real? (not/c negative?))]{

Returns the spacing inserted by the editor between each line. This
 spacing is included in the reported height of each line.

}

@defmethod[(get-overwrite-mode)
           boolean?]{

Returns @racket[#t] if the editor is in overwrite mode, @racket[#f]
 otherwise. Overwrite mode only affects the way that @method[editor<%>
 on-default-char] handles keyboard input for insertion characters. See
 also @method[text% set-overwrite-mode].

}


@defmethod[(get-padding) (values (and/c real? (not/c negative?))
                                 (and/c real? (not/c negative?))
                                 (and/c real? (not/c negative?))
                                 (and/c real? (not/c negative?)))]{

Returns the editor's padding for its left, top, right, and bottom
sides (in that order).

See also @method[text% set-padding].}


@defmethod[(get-position [start (or/c (box/c exact-nonnegative-integer?) #f)]
                         [end (or/c (box/c exact-nonnegative-integer?) #f) #f])
           void?]{

Returns the current selection range in @techlink{position}s.  If
nothing is selected, the @racket[start] and @racket[end] will be
the same number and that number will be where the insertion point is.

See also @method[text% get-start-position] 
and @method[text% get-end-position].

@boxisfillnull[@racket[start] @elem{the starting @techlink{position} of the selection}]
@boxisfillnull[@racket[end] @elem{the ending @techlink{position} of the selection}]

}


@defmethod[(get-region-data [start exact-nonnegative-integer?]
                            [end exact-nonnegative-integer?])
           (or/c (is-a?/c editor-data%) #f)]{

Gets extra data associated with a given region. See
 @|editordatadiscuss| for more information.

This method is @italic{not} called when the whole editor is saved to a
 file. In such cases, the information can be stored in the header or
 footer; see @|globaleditordatadiscuss|.

This method is meant to be overridden; the default @method[text%
 set-region-data] method does not store information to be retrieved by
 this method.

}


@defmethod[(get-revision-number)
           (and/c real? (not/c negative?))]{

Returns an inexact number that increments every time the editor is
 changed in one of the following ways: a snip is inserted (see
 @method[text% after-insert]), a snip is deleted (see @method[text%
 after-delete]), a snip is split (see @method[text%
 after-split-snip]), snips are merged (see @method[text%
 after-merge-snips]), or a snip changes its count (which is rare; see
 @method[snip-admin% recounted]).

}


@defmethod[(get-snip-position [snip (is-a?/c snip%)])
           (or/c exact-nonnegative-integer? #f)]{

Returns the starting @techlink{position} of a given snip or
 @racket[#f] if the snip is not in this editor.

}

@defmethod[#:mode public-final
           (get-snip-position-and-location [snip (is-a?/c snip%)]
                                           [pos (or/c (box/c exact-nonnegative-integer?) #f)]
                                           [x (or/c (box/c real?) #f) #f]
                                           [y (or/c (box/c real?) #f) #f])
           boolean?]{

Gets a snip's @techlink{position} and top left @techlink{location} in editor
 coordinates.  The return value is @racket[#t] if the snip is found,
 @racket[#f] otherwise.

@boxisfillnull[@racket[pos] @elem{starting @techlink{position} of @racket[snip]}]
@boxisfillnull[@racket[x] @elem{left @techlink{location} of @racket[snip] in editor coordinates}]
@boxisfillnull[@racket[y] @elem{top @techlink{location} of @racket[snip] in editor coordinates}]

When @techlink{location} information is requested: @|OVD| @|FCA|

}


@defmethod[(get-start-position)
           exact-nonnegative-integer?]{

Returns the starting @techlink{position} of the current selection. See also
 @method[text% get-position].

}


@defmethod[(get-styles-sticky)
           boolean?]{

In the normal mode for a text editor, style settings are sticky. With
 sticky styles, when a string or character is inserted into an editor,
 it gets the style of the snip preceding the insertion point (or the
 snip that includes the insertion point if text is inserted into an
 exiting string snip). Alternatively, if @method[text% change-style]
 is called to set the style at the caret @techlink{position} (when it
 is not a range), then the style is remembered; if the editor is not
 changed before text is inserted at the caret, then the text gets the
 remembered style.

With non-sticky styles, text inserted into an editor always gets the
 style in the editor's style list named by @method[editor<%>
 default-style-name].

See also @method[text% set-styles-sticky].

}


@defmethod[(get-tabs [length (or/c (box/c exact-nonnegative-integer?) #f) #f]
                     [tab-width (or/c (box/c real?) #f) #f]
                     [in-units (or/c (box/c any/c) #f) #f])
           (listof real?)]{

Returns the current tab-position array as a list.

@boxisfillnull[@racket[length] @elem{the length of the tab array (and therefore the returned 
list)}]
@boxisfillnull[@racket[tab-width] @elem{the width used for tabs past the 
end of the tab array}]
@boxisfillnull[@racket[in-units] @elem{@racket[#t] if the tabs are specified in
canvas units or @racket[#f] if they are specified in space-widths}]

See also 
@method[text% set-tabs].

}


@defmethod[(get-text [start exact-nonnegative-integer? 0]
                     [end (or/c exact-nonnegative-integer? 'eof) 'eof]
                     [flattened? any/c #f]
                     [force-cr? any/c #f])
           string?]{

Gets the text from @racket[start] to @racket[end]. If @racket[end] is
 @racket['eof], then the contents are returned from @racket[start] until the
 end of the editor.

If @racket[flattened?] is not @racket[#f], then flattened text is returned.
 See @|textdiscuss| for a discussion of flattened vs. non-flattened
 text.

If @racket[force-cr?] is not @racket[#f] and @racket[flattened?] is not
 @racket[#f], then automatic newlines (from word-wrapping) are
 written into the return string as real newlines.

}


@defmethod[(get-top-line-base)
           (and/c real? (not/c negative?))]{

Returns the distance from the top of the editor to the alignment
 baseline of the top line. This method is primarily used when an
 editor is an @techlink{item} within another editor.
The reported baseline distance includes the editor's
 top padding (see @method[text% set-padding]).

@|OVD| @FCAME[]

}


@defmethod[(get-visible-line-range [start (or/c (box/c exact-nonnegative-integer?) #f)]
                                   [end (or/c (box/c exact-nonnegative-integer?) #f)]
                                   [all? any/c #t])
           void?]{

Returns the range of lines which are currently visible (or partially
 visible) to the user. @|LineNumbering|

@boxisfillnull[@racket[start] @elem{first line visible to the user}]
@boxisfillnull[@racket[end] @elem{last line visible to the user}]

If the editor is displayed by multiple canvases and @racket[all?] is
 @racket[#t], then the computed range includes all visible lines in all
 @techlink{display}s. Otherwise, the range includes only the visible lines in the
 current @techlink{display}.

@|OVD| @|FCA|

}


@defmethod[(get-visible-position-range [start (or/c (box/c exact-nonnegative-integer?) #f)]
                                       [end (or/c (box/c exact-nonnegative-integer?) #f)]
                                       [all? any/c #t])
           void?]{

Returns the range of @techlink{position}s that are currently visible (or
 partially visible) to the user.

@boxisfillnull[@racket[start] @elem{first @techlink{position} visible to the user}]
@boxisfillnull[@racket[end] @elem{last @techlink{position} visible to the user}]

If the editor is displayed by multiple canvases and @racket[all?] is
 @racket[#t], then the computed range includes all visible @techlink{position}s in
 all @techlink{display}s. Otherwise, the range includes only the visible
 @techlink{position}s in the current @techlink{display}.

@|OVD| @|FCA|

}


@defmethod[(get-wordbreak-map)
           (or/c (is-a?/c editor-wordbreak-map%) #f)]{

Returns the wordbreaking map that is used by the standard wordbreaking
 function. See @method[text% set-wordbreak-map] and 
 @racket[editor-wordbreak-map%] for more information.

}


@defmethod[(hide-caret [hide? any/c])
           void?]{

Determines whether the caret is shown when the editor has the keyboard
 focus.

If @racket[hide?] is not @racket[#f], then the caret or selection hiliting
 will not be drawn for the editor. The editor can still own the
 keyboard focus, but no caret will be drawn to indicate the focus.

See also @method[text% caret-hidden?] and @method[editor<%> lock].

}


@defmethod*[#:mode override 
            ([(insert [str string?]
                      [start exact-nonnegative-integer?]
                      [end (or/c exact-nonnegative-integer? 'same) 'same]
                      [scroll-ok? any/c #t])
              void?]
             [(insert [n (and/c exact-nonnegative-integer?
                                (<=/c (string-length str)))]
                      [str string?]
                      [start exact-nonnegative-integer?]
                      [end (or/c exact-nonnegative-integer? 'same) 'same]
                      [scroll-ok? any/c #t])
              void?]
             [(insert [str string?])
              void?]
             [(insert [n (and/c exact-nonnegative-integer?
                                (<=/c (string-length str)))]
                      [str string?])
              void?]
             [(insert [snip (is-a?/c snip%)]
                      [start exact-nonnegative-integer?]
                      [end (or/c exact-nonnegative-integer? 'same) 'same]
                      [scroll-ok? any/c #t])
              void?]
             [(insert [snip (is-a?/c snip%)])
              void?]
             [(insert [char char?])
              void?]
             [(insert [char char?]
                      [start exact-nonnegative-integer?]
                      [end (or/c exact-nonnegative-integer? 'same) 'same])
              void?])]{

Inserts text or a snip into @this-obj[] at @techlink{position}
 @racket[start].  If @racket[n] is provided, the only the first
 @racket[n] characters of @racket[str] are inserted. 

When a @racket[snip] is provided: The snip cannot be inserted into
 multiple editors or multiple times within a single editor. As the
 snip is inserted, its current style is converted to one in the
 editor's style list; see also @method[style-list% convert].

When a @racket[char] is provided: @|insertcharundos|

When @racket[start] is not provided, the current selection start is
 used. If the current selection covers a range of @techlink{item}s,
 then @racket[char] replaces the selected text. The selection's start
 and end @techlink{position}s are moved to the end of the inserted
 character.

For a case where @racket[end] is not provided and has no default, the
 current selection end is used. Otherwise, if @racket[end] is not
 @racket['same], then the inserted value replaces the region from
 @racket[start] to @racket[end], and the selection is left at the end
 of the inserted text. Otherwise, if the insertion @techlink{position}
 is before or equal to the selection's start/end @techlink{position},
 then the selection's start/end @techlink{position} is incremented by
 the length of @racket[str].

If @racket[scroll-ok?] is not @racket[#f] and @racket[start] is the
 same as the current selection's start @techlink{position}, then the
 editor's @techlink{display} is scrolled to show the new selection
 @techlink{position}.

See also @method[text% get-styles-sticky].

}


@defmethod*[#:mode override
            ([(kill [time exact-integer? 0])
              void?]
             [(kill [time exact-integer?]
                    [start exact-nonnegative-integer?]
                    [end exact-nonnegative-integer?])
              void?])]{

Cuts the text in the given region. If @racket[start] and @racket[end]
 are not supplied, then the selected region plus all whitespace to the
 end of line is cut; the newline is also cut if only whitespace exists
 between the selection and the end of line.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}


@defmethod[(last-line)
           exact-nonnegative-integer?]{

Returns the number of the last line in the editor. Lines are numbered
 starting with @racket[0], so this is one less than the number of lines
 in the editor.

@LineToPara[@racket[last-paragraph]]

@|FCAMW| @|EVD|
}


@defmethod[(last-paragraph)
           exact-nonnegative-integer?]{

Returns the number of the last paragraph in the editor. Paragraphs are
 numbered starting with @racket[0], so this is one less than the
 number of paragraphs in the editor.

@|FCAMW|

}

@defmethod[(last-position)
           exact-nonnegative-integer?]{

Returns the last selection @techlink{position} in the editor. This is
 also the number of @techlink{item}s in the editor.

}

@defmethod[(line-end-position [line exact-nonnegative-integer?]
                              [visible? any/c #t])
           exact-nonnegative-integer?]{

Returns the last @techlink{position} of a given line. @|LineNumbering|

If there are fewer than @math{@racket[line]-1} lines, the end of the
 last line is returned. If @racket[line] is less than 0, then the end
 of the first line is returned.

If the line ends with @tech{invisible} @techlink{item}s (such as a
 newline) and @racket[visible?] is not @racket[#f], the first
 @techlink{position} before the @tech{invisible} @techlink{item}s is
 returned.

@LineToPara[@racket[paragraph-end-position]]

@|FCAMW| @|EVD|

}


@defmethod[(line-length [i exact-nonnegative-integer?])
           exact-nonnegative-integer?]{

Returns the number of @techlink{item}s in a given
line. @|LineNumbering|

@|FCAMW| @|EVD|

}


@defmethod[(line-location [line exact-nonnegative-integer?]
                          [top? any/c #t])
           real?]{

Given a line number, returns the @techlink{location} of the line. @|LineNumbering|

If @racket[top?] is not @racket[#f], the @techlink{location} for the
 top of the line is returned; otherwise, the @techlink{location} for
 the bottom of the line is returned.

@LineToPara[@racket[paragraph-location]]

@|OVD| @|FCA|

}

@defmethod[(line-paragraph [start exact-nonnegative-integer?])
           exact-nonnegative-integer?]{

Returns the paragraph number of the paragraph containing the line.
 @|LineNumbering| @|ParagraphNumbering|

@|FCAMW| @|EVD|

}

@defmethod[(line-start-position [line exact-nonnegative-integer?]
                                [visible? any/c #t])
           exact-nonnegative-integer?]{

Returns the first @techlink{position} of the given line. @|LineNumbering|

If there are fewer than @math{@racket[line]-1} lines, the start of the
last line is returned. If @racket[line] is less than 0, then
the start of the first line is returned.

If the line starts with @tech{invisible} @techlink{item}s and @racket[visible?] is not
 @racket[#f], the first @techlink{position} past the @tech{invisible} @techlink{item}s is
 returned.

@LineToPara[@racket[paragraph-start-position]]

@|FCAMW|

To calculate lines, if the following are true:
@itemize[

 @item{the editor is not displayed (see @secref["tb:miaoverview"]),}

 @item{a maximum width is set for the editor, and} 

 @item{the editor has never been viewed}

]

then this method ignores the editor's maximum width and any automatic
 line breaks it might imply.  If the first two of the above conditions
 are true and the editor was @italic{formerly} displayed, this method
 uses the line breaks from the most recent display of the
 editor. (Insertions or deletions since the display shift line breaks
 within the editor in the same way as @techlink{item}s.)

}


@defmethod[(move-position [code (or/c 'home 'end 'right 'left 'up 'down)]
                          [extend? any/c #f]
                          [kind (or/c 'simple 'word 'page 'line) 'simple])
           void?]{

Moves the current selection. 

The possible values for @racket[code] are:

@itemize[
@item{@racket['home] --- go to start of file}
@item{@racket['end] --- go to end of file}
@item{@racket['right] --- move right}
@item{@racket['left] --- move left}
@item{@racket['up] --- move up}
@item{@racket['down] --- move down}
]

If @racket[extend?] is not @racket[#f], the selection range is
 extended instead of moved.  If anchoring is on (see @method[text%
 get-anchor] and @method[text% set-anchor]), then @racket[extend?] is
 effectively forced to @racket[#t]. See also @method[text% get-extend-start-position]
 and @method[text% get-extend-end-position].

The possible values for @racket[kind] are:

@itemize[
@item{@racket['simple] --- move one item or line}
@item{@racket['word] --- works with @racket['right] or @racket['left]}
@item{@racket['page] --- works with @racket['up] or @racket['down]}
@item{@racket['line] --- works with @racket['right] or @racket['left]; moves to the start or end of the line}
]

See also @method[text% set-position].

}


@defmethod[#:mode pubment 
           (on-change-style [start exact-nonnegative-integer?]
                            [len exact-nonnegative-integer?])
           void?]{

@methspec{

Called before the style is changed in a given range of the editor,
 after @method[text% can-change-style?] is called to verify that the
 change is ok. The @method[text% after-change-style] method is
 guaranteed to be called after the change has completed.

The editor is internally locked for writing during a call to this method
 (see also @|lockdiscuss|). Use 
@method[text% after-change-style] to modify the editor, if necessary.

See also @method[editor<%> on-edit-sequence].

}
@methimpl{

Does nothing.

}
}


@defmethod[#:mode override 
           (on-default-char [event (is-a?/c key-event%)])
           void?]{

Handles the following:

@itemize[

 @item{Delete and Backspace --- calls @method[text% delete].} 

 @item{The arrow keys, Page Up, Page Down, Home, and End (including
  shifted versions) --- moves the selection @techlink{position} with
  @method[text% move-position].}

 @item{Any other character in the range @racket[(integer->char 32)] to
 @racket[(integer->char 255)] --- inserts the character into the
 editor.}

]

Note that an editor's @racket[editor-canvas%] normally handles mouse
 wheel events (see also @method[editor-canvas% on-char] ).

}


@defmethod[#:mode override 
           (on-default-event [event (is-a?/c mouse-event%)])
           void?]{

Tracks clicks on a clickback (see @method[text% set-clickback]) of
 changes the selection. Note that @method[editor<%> on-event]
 dispatches to a caret-owning snip and detects a click on an
 event-handling snip before calling to this method.

@itemize[

 @item{Clicking on a clickback region starts clickback tracking. See
 @method[text% set-clickback] for more information. Moving over a
 clickback changes the shape of the mouse cursor.}

 @item{Clicking anywhere else moves the caret to the closest @techlink{position}
 between @techlink{item}s. Shift-clicking extends the current selection.}

 @item{Dragging extends the selection, scrolling if possible when the
 selection is dragged outside the editor's visible region.}

]
}


@defmethod[#:mode pubment 
           (on-delete [start exact-nonnegative-integer?]
                      [len exact-nonnegative-integer?])
           void?]{
@methspec{

Called before a range is deleted from the editor, after @method[text%
 can-delete?] is called to verify that the deletion is ok. The
 @method[text% after-delete] method is guaranteed to be called after
 the delete has completed.

The @racket[start] argument specifies the starting @techlink{position}
 of the range to delete. The @racket[len] argument specifies number of
 @techlink{item}s to delete (so @math{@racket[start]+@racket[len]} is
 the ending @techlink{position} of the range to delete).

The editor is internally locked for writing during a call to this
 method (see also @|lockdiscuss|). Use @method[text% after-delete] to
 modify the editor, if necessary.

See also @method[editor<%> on-edit-sequence].


}
@methimpl{

Does nothing.

}}


@defmethod[#:mode pubment 
           (on-insert [start exact-nonnegative-integer?]
                      [len exact-nonnegative-integer?])
           void?]{
@methspec{

Called before @techlink{item}s are inserted into the editor, after
 @method[text% can-insert?] is called to verify that the insertion is
 ok. The @method[text% after-insert] method is guaranteed to be called
 after the insert has completed.

The @racket[start] argument specifies the @techlink{position} of the insert. The
 @racket[len] argument specifies the total length (in @techlink{position}s) of the
 @techlink{item}s to be inserted.

The editor is internally locked for writing during a call to this
 method (see also @|lockdiscuss|). Use @method[text% after-insert] to
 modify the editor, if necessary.

See also @method[editor<%> on-edit-sequence].

}
@methimpl{

Does nothing.

}}


@defmethod[(on-new-string-snip)
           (is-a?/c string-snip%)]{

@methspec{

Called by @method[text% insert] when a string or character is inserted
into the editor, this method creates and returns a new instance of
@racket[string-snip%] to store inserted text. The returned string snip
is empty (i.e., its @techlink{count} is zero).

}
@methimpl{

Returns a @racket[string-snip%] instance.

}}

@defmethod[(on-new-tab-snip)
           (is-a?/c tab-snip%)]{

@methspec{

Creates and returns a new instance of @racket[tab-snip%] to store an
 inserted tab. The returned tab snip is empty (i.e., its @techlink{count}
 is zero).

}
@methimpl{

Returns a @racket[tab-snip%] instance.

}}


@defmethod[#:mode pubment 
           (on-reflow)
           void?]{

@methspec{
Called after @tech{locations} have changed and are recomputed for the editor.
}
@methimpl{

Does nothing.
}}

@defmethod[#:mode pubment 
           (on-set-size-constraint)
           void?]{

@methspec{

Called before the editor's maximum or minimum height or width is
 changed, after @method[text% can-set-size-constraint?] is called to
 verify that the change is ok. The @method[text%
 after-set-size-constraint] method is guaranteed to be called after
 the change has completed.

(This callback method is provided because setting an editor's maximum
 width may cause lines to be re-flowed with soft newlines.)

See also @method[editor<%> on-edit-sequence].

}
@methimpl{

Does nothing.

}}


@defmethod[(paragraph-end-line [paragraph exact-nonnegative-integer?])
           exact-nonnegative-integer?]{

Returns the ending line of a given paragraph. @|ParagraphNumbering| @|LineNumbering|

@|FCAMW| @|EVD|

}


@defmethod[(paragraph-end-position [paragraph exact-nonnegative-integer?]
                                   [visible? any/c #t])
           exact-nonnegative-integer?]{

Returns the ending @techlink{position} of a given paragraph. @|ParagraphNumbering|

If there are fewer than @math{@racket[paragraph]-1} paragraphs, the
 end of the last paragraph is returned. If @racket[paragraph] is less
 than 0, then the end of the first paragraph is returned.

If the paragraph ends with @tech{invisible} @techlink{item}s (such as a newline)
 and @racket[visible?] is not @racket[#f], the first @techlink{position}
 before the @tech{invisible} @techlink{item}s is returned.

}


@defmethod[(paragraph-start-line [paragraph exact-nonnegative-integer?])
           exact-nonnegative-integer?]{

Returns the starting line of a given paragraph. If @racket[paragraph]
is greater than the highest-numbered paragraph, then the editor's end
@tech{position} is returned. @|ParagraphNumbering| @|LineNumbering|

@|FCAMW| @|EVD|

}


@defmethod[(paragraph-start-position [paragraph exact-nonnegative-integer?]
                                     [visible? any/c #t])
           exact-nonnegative-integer?]{

Returns the starting @techlink{position} of a given paragraph. @|ParagraphNumbering|

If there are fewer than @math{@racket[paragraph]-1} paragraphs, the
 start of the last paragraph is returned.

If the paragraph starts with @tech{invisible} @techlink{item}s and @racket[visible?] is
 not @racket[#f], the first @techlink{position} past the @tech{invisible} @techlink{item}s is
 returned.

}


@defmethod[#:mode override
           (paste [time exact-integer? 0]
                  [start (or/c exact-nonnegative-integer? 'start 'end) 'start]
                  [end (or/c exact-nonnegative-integer? 'same) 'same])
           void?]{

Pastes into the specified range. If @racket[start] is @racket['start],
 then the current selection start @techlink{position} is used. If
 @racket[start] is @racket['end], then the current selection end
 @techlink{position} is used. If @racket[end] is @racket['same], then
 @racket[start] is used for @racket[end], unless @racket[start] is
 @racket['start], in which case the current selection end
 @techlink{position} is used.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}


@defmethod[(paste-next)
           void?]{

Editors collectively maintain a copy ring that holds up to 30 previous
 copies (and cuts) among the editors. When it is called as the next
 method on an editor after a paste, the @method[text% paste-next]
 method replaces the text from a previous paste with the next data in
 the copy ring, incrementing the ring pointer so that the next
 @method[text% paste-next] pastes in even older data.

It is a copy ``ring'' because the ring pointer wraps back to the most
 recent copied data after the oldest remembered data is pasted. Any
 cut, copy, or (regular) paste operation resets the copy ring pointer
 back to the beginning.

If the previous operation on the editor was not a paste, calling
 @method[text% paste-next] has no effect.

}


@defmethod[#:mode override
           (paste-x-selection [time exact-integer? 0]
                              [start (or/c exact-nonnegative-integer? 'start 'end) 'start]
                              [end (or/c exact-nonnegative-integer? 'same) 'same])
           void?]{

Pastes into the specified range. If @racket[start] is @racket['start],
 then the current selection start @techlink{position} is used. If
 @racket[start] is @racket['end], then the current selection end
 @techlink{position} is used. If @racket[end] is @racket['same], then
 @racket[start] is used for @racket[end], unless @racket[start] is
 @racket['start], in which case the current selection end
 @techlink{position} is used.

See @|timediscuss| for a discussion of the @racket[time] argument. If
 @racket[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(position-line [start exact-nonnegative-integer?]
                          [at-eol? any/c #f])
           exact-nonnegative-integer?]{

Returns the line number of the line containing a given @techlink{position}. @|LineNumbering|

@LineToPara[@racket[position-paragraph]]

See @|ateoldiscuss| for a discussion of @racket[at-eol?].

@|FCAMW| @|EVD|

}


@defmethod[(position-location [start exact-nonnegative-integer?]
                              [x (or/c (box/c real?) #f) #f]
                              [y (or/c (box/c real?) #f) #f]
                              [top? any/c #t]
                              [at-eol? any/c #f]
                              [whole-line? any/c #f])
           void?]{

Returns the @techlink{location} of a given @techlink{position}. See also @method[text% position-locations].

@boxisfillnull[@racket[x] @elem{the x-@techlink{location} of the @techlink{position} @racket[start] in editor
coordinates} ]
@boxisfillnull[@racket[y] @elem{the y-@techlink{location} (top or bottom; see below) of the
@techlink{position} @racket[start] in editor coordinates}]

See @|ateoldiscuss| for a discussion of @racket[at-eol?].

If @racket[top?] is not @racket[#f], the top coordinate of the @techlink{location}
is returned, otherwise the bottom coordinate of the
@techlink{location} is returned.

The top @racket[y] @techlink{location} may be different for different @techlink{position}s
within a line when different-sized graphic objects are used. If
@racket[whole-line?] is not @racket[#f], the minimum top @techlink{location} or
maximum bottom @techlink{location} for the whole line is returned in @racket[y].

@|OVD| @|FCA|

}


@defmethod[(position-locations [start exact-nonnegative-integer?]
                               [top-x (or/c (box/c real?) #f) #f]
                               [top-y (or/c (box/c real?) #f) #f]
                               [bottom-x (or/c (box/c real?) #f) #f]
                               [bottom-y (or/c (box/c real?) #f) #f]
                               [at-eol? any/c #f]
                               [whole-line? any/c #f])
           void?]{

Like @method[text% position-location], but returns both the ``top''
and ``bottom'' results at once.

@|OVD| @|FCA|

}

@defmethod[(position-paragraph [start exact-nonnegative-integer?]
                               [at-eol? any/c #f])
           exact-nonnegative-integer?]{

See @|ateoldiscuss| for a discussion of @racket[at-eol?].

Returns the paragraph number of the paragraph containing a given @techlink{position}.

}


@defmethod*[#:mode extend
            ([(read-from-file [stream (is-a?/c editor-stream-in%)]
                              [start (or/c exact-nonnegative-integer? 'start)]
                              [overwrite-styles? any/c #f])
              boolean?]
             [(read-from-file [stream (is-a?/c editor-stream-in%)]
                              [overwrite-styles? any/c #f])
              boolean?])]{

New data is inserted at the @techlink{position} indicated by @racket[start], or at
 the current @techlink{position} if @racket[start] is @racket['start].

}


@defmethod[(remove-clickback [start exact-nonnegative-integer?]
                             [end exact-nonnegative-integer?])
           void?]{

Removes all clickbacks installed for exactly the range @racket[start]
 to @racket[end]. See also @|clickbackdiscuss|.

}


@defmethod[(scroll-to-position [start exact-nonnegative-integer?]
                               [at-eol? any/c #f]
                               [end (or/c exact-nonnegative-integer? 'same) 'same]
                               [bias (or/c 'start 'end 'none) 'none])
           boolean?]{

Scrolls the editor so that a given @techlink{position} is visible. 

If @racket[end] is @racket['same] or equal to @racket[start], then @techlink{position}
 @racket[start] is made visible.  See @|ateoldiscuss| for a discussion of
 @racket[at-eol?].

If @racket[end] is not @racket['same] and not the same as @racket[start],
 then the range @racket[start] to @racket[end] is made visible and
 @racket[at-eol?] is ignored.

When the specified range cannot fit in the visible area, @racket[bias]
 indicates which end of the range to display. When @racket[bias] is
 @racket['start], then the start of the range is displayed. When
 @racket[bias] is @racket['end], then the end of the range is
 displayed. Otherwise, @racket[bias] must be @racket['none].

If the editor is scrolled, then the editor is redrawn and the return
 value is @racket[#t]; otherwise, the return value is @racket[#f].  If
 refreshing is delayed (see @method[editor<%> refresh-delayed?]), then
 the scroll request is saved until the delay has ended. The scroll is
 performed (immediately or later) by calling @method[editor<%>
 scroll-editor-to].

Scrolling is disallowed when the editor is internally locked for
 reflowing (see also @|lockdiscuss|).

The system may scroll the editor without calling this method. For
 example, a canvas displaying an editor might scroll the editor to
 handle a scrollbar event.

}


@defmethod[(set-anchor [on? any/c])
           void?]{

Turns anchoring on or off. This method can be overridden to affect or
 detect changes in the anchor state. See also
 @method[text% get-anchor].

If @racket[on?] is not @racket[#f], then the selection will be
 automatically extended when cursor keys are used (or, more generally,
 when @method[text% move-position] is used to move the selection or the
 @racket[_keep-anchor?] argument to @method[text% set-position] is a true value),
 otherwise anchoring is turned off. Anchoring is automatically turned
 off if the user does anything besides cursor movements.

}


@defmethod[(set-autowrap-bitmap [bitmap (or/c (is-a?/c bitmap%) #f)])
           (or/c (is-a?/c bitmap%) #f)]{

Sets the bitmap that is drawn at the end of a line when it is
 automatically line-wrapped.

If @racket[bitmap] is @racket[#f], no autowrap indicator is drawn
 (this is the default). The previously used bitmap (possibly
 @racket[#f]) is returned.

Setting the bitmap is disallowed when the editor is internally locked
 for reflowing (see also @|lockdiscuss|).

}


@defmethod[(set-between-threshold [threshold (and/c real? (not/c negative?))])
           void?]{

Sets the graphical distance used to determine the meaning of a user
 click. If a click falls within @racket[threshold] of a position
 between two @techlink{item}s, then the click registers on the space
 between the @techlink{item}s rather than on either @techlink{item}.

See also 
@method[text% get-between-threshold].

}


@defmethod[(set-clickback [start exact-nonnegative-integer?]
                          [end exact-nonnegative-integer?]
                          [f (-> (is-a?/c text%) 
                                 exact-nonnegative-integer?
                                 exact-nonnegative-integer?
                                 any)]
                          [hilite-delta (or/c (is-a?/c style-delta%) #f) #f]
                          [call-on-down? any/c #f])
           void?]{

Installs a clickback for a given region. If a clickback is already
 installed for an overlapping region, this clickback takes precedence.

The callback procedure @racket[f] is called when the user selects the
 clickback. The arguments to @racket[f] are this editor and the starting
 and ending range of the clickback.

The @racket[hilite-delta] style delta is applied to the clickback text
 when the user has clicked and is still holding the mouse over the
 clickback. If @racket[hilite-delta] is @racket[#f], then the clickback
 region's style is not changed when it is being selected.

If @racket[call-on-down?] is not @racket[#f], the clickback is called
 immediately when the user clicks the mouse button down, instead of
 after a mouse-up event. The @racket[hilite-delta] argument is not used
 in this case.

See also @|clickbackdiscuss|.
 }

@defmethod[(set-file-format [format (or/c 'standard 'text 'text-force-cr)])
           void?]{

Set the format of the file saved from this editor. 

The legal formats are:

@itemize[
@item{@racket['standard] ---  a standard editor  file}
@item{@racket['text] --- a text file}
@item{@racket['text-force-cr] --- a text file; when writing, change 
automatic newlines (from word-wrapping) into real newlines}
]

@MonitorMethod[@elem{The file format of an editor} @elem{the
 system in response to file loading and saving
 method calls} @elem{@method[editor<%> on-load-file] and
 @method[editor<%> on-save-file]} @elem{such file format}]
}


@defmethod[(set-line-spacing [space (and/c real? (not/c negative?))])
           void?]{

Sets the spacing inserted by the editor between each line. This
 spacing is included in the reported height of each line.

}

@defmethod[(set-overwrite-mode [on? any/c])
           void?]{

Enables or disables overwrite mode. See @method[text%
 get-overwrite-mode]. This method can be overridden to affect or
 detect changes in the overwrite mode.

}

@defmethod[(set-padding [left (and/c real? (not/c negative?))]
                        [top (and/c real? (not/c negative?))]
                        [right (and/c real? (not/c negative?))]
                        [bottom (and/c real? (not/c negative?))])
           void?]{

Sets padding that insets the editor's content when drawn within its
@techlink{display}.

Unlike any margin that may be applied by the editor's
@techlink{display}, padding is counted in @techlink{location}
information that is reported by methods such as @method[text%
position-location]. For example, with a @racket[left] padding of 17.0
and a @racket[top] padding of 9.0, the location of position 0 will be
(17.0, 9.0) rather than (0, 0). Padding also contributes to the
editor's size as reported by @method[editor<%> get-extent].}


@defmethod[(set-paragraph-alignment [paragraph exact-nonnegative-integer?]
                                    [alignment (or/c 'left 'center 'right)])
           void?]{

Sets a paragraph-specific horizontal alignment. The alignment is only
 used when the editor has a maximum width, as set with
 @method[editor<%> set-max-width]. @|ParagraphNumbering|

@italic{This method is experimental.} It works reliably only when the
 paragraph is not merged or split. Merging or splitting a paragraph
 with alignment settings causes the settings to be transferred
 unpredictably (although other paragraphs in the editor can be safely
 split or merged). If the last paragraph in an editor is empty,
 settings assigned to it are ignored.

}


@defmethod[(set-paragraph-margins [paragraph exact-nonnegative-integer?]
                                  [first-left (and/c real? (not/c negative?))]
                                  [left (and/c real? (not/c negative?))]
                                  [right (and/c real? (not/c negative?))])
           void?]{

Sets a paragraph-specific margin. @|ParagraphNumbering|

The first line of the paragraph is indented by @racket[first-left] points
 within the editor. If the paragraph is line-wrapped (when the editor
 has a maximum width), subsequent lines are indented by @racket[left]
 points.  If the editor has a maximum width, the paragraph's maximum
 width for line-wrapping is @racket[right] points smaller than the
 editor's maximum width.

@italic{This method is experimental.} See @method[text%
 set-paragraph-alignment] for more information.

}


@defmethod[(set-position [start exact-nonnegative-integer?]
                         [end (or/c exact-nonnegative-integer? 'same) 'same]
                         [at-eol? any/c #f]
                         [scroll? any/c #t]
                         [seltype (or/c 'default 'x 'local) 'default])
           void?]{

Sets the current selection in the editor. 

If @racket[end] is @racket['same] or less than or equal to @racket[start],
 the current start and end @techlink{position}s are both set to
 @racket[start]. Otherwise the given range is selected.

See @|ateoldiscuss| for a discussion of @racket[at-eol?]. If
 @racket[scroll?]  is not @racket[#f], then the @techlink{display} is
 scrolled to show the selection if necessary.

The @racket[seltype] argument is only used when the X Window System
 selection mechanism is enabled. The possible values are:
@itemize[

 @item{@racket['default] --- if this window has the keyboard focus
 and given selection is non-empty, make it the current X selection}

 @item{@racket['x] --- if the given selection is non-empty, make
 it the current X selection}

 @item{@racket['local] --- do not change the
 current X selection}

]

Setting the @techlink{position} is disallowed when the editor is internally
 locked for reflowing (see also @|lockdiscuss|).

The system may change the selection in an editor without calling this
 method (or any visible method).

See also @racket[editor-set-x-selection-mode].

}

@defmethod[(set-position-bias-scroll [bias (or/c 'start-only 'start 'none 'end 'end-only)]
                                     [start exact-nonnegative-integer?]
                                     [end (or/c exact-nonnegative-integer? 'same) 'same]
                                     [ateol? any/c #f]
                                     [scroll? any/c #t]
                                     [seltype (or/c 'default 'x 'local) 'default])
           void?]{

Like  @method[text% set-position], but a scrolling bias can be specified.

The possible values for @racket[bias] are:
@itemize[
@item{@racket['start-only] --- only insure that the starting @techlink{position} is visible}
@item{@racket['start] --- if the range doesn't fit in the visible area, show the starting @techlink{position}}
@item{@racket['none] --- no special scrolling instructions}
@item{@racket['end] --- if the range doesn't fit in the visible area, show the ending @techlink{position}}
@item{@racket['end-only] --- only insure that the ending @techlink{position} is visible}
]

See also @method[text% scroll-to-position].

}


@defmethod[(set-region-data [start exact-nonnegative-integer?]
                            [end exact-nonnegative-integer?]
                            [data (is-a?/c editor-data%)])
           void?]{

@methspec{

Sets extra data associated with a given region. See
 @|editordatadiscuss| and @method[text% get-region-data] for more
 information.

This method is meant to be overridden in combination with
 @method[text% get-region-data] .

}
@methimpl{

Does nothing.

}}


@defmethod[(set-styles-sticky [sticky? any/c])
           void?]{

See @method[text% get-styles-sticky] for information about sticky
 styles.

}


@defmethod[(set-tabs [tabs (listof real?)]
                     [tab-width real? 20]
                     [in-units? any/c #t])
           void?]{

Sets the tabbing array for the editor.

The @racket[tabs] list determines the tabbing array. The tabbing array
 specifies the x-@techlink{location}s where each tab occurs. Tabs beyond the last
 specified tab are separated by a fixed amount @racket[tab-width].  If
 @racket[in-units?] is not @racket[#f], then tabs are specified in canvas
 units; otherwise, they are specified as a number of spaces. (If tabs
 are specified in spaces, then the graphic tab positions will change
 with the font used for the tab.)

Setting tabs is disallowed when the editor is internally locked for
 reflowing (see also @|lockdiscuss|).

}


@defmethod[(set-wordbreak-func [f ((is-a?/c text%) (or/c (box/c exact-nonnegative-integer?) #f)
                                                   (or/c (box/c exact-nonnegative-integer?) #f)
                                                   symbol?
                                   . -> . any)])
           void?]{

Sets the word-breaking function for the editor.  For information about
 the arguments to the word-breaking function, see @method[text%
 find-wordbreak].

The standard wordbreaking function uses the editor's
 @racket[editor-wordbreak-map%] object to determine which characters
 break a word. See also @racket[editor-wordbreak-map%] and
 @method[text% set-wordbreak-map].

Since the wordbreak function will be called when line breaks are being
 determined (in an editor that has a maximum width), there is a
 constrained set of @racket[text%] methods that the wordbreak
 function is allowed to invoke. It cannot invoke a member function
 that uses information about @techlink{location}s or lines (which are
 identified in this manual with ``@|OVD|''), but it can still invoke
 member functions that work with snips and @techlink{item}s.

}


@defmethod[(set-wordbreak-map [map (or/c (is-a?/c editor-wordbreak-map%) #f)])
           void?]{

Sets the wordbreaking map that is used by the standard wordbreaking
 function. See @racket[editor-wordbreak-map%] for more information.

If @racket[map] is @racket[#f], then the standard map
 (@racket[the-editor-wordbreak-map]) is used.

}


@defmethod[(split-snip [pos exact-nonnegative-integer?])
           void?]{

Given a @techlink{position}, splits the snip that includes the
 @techlink{position} (if any) so that the @techlink{position} is
 between two snips. The snip may refuse to split, although none of the
 built-in snip classes will ever refuse.

Splitting a snip is disallowed when the editor is internally locked
 for reflowing (see also @|lockdiscuss|).

}


@defmethod[#:mode extend
           (write-to-file [stream (is-a?/c editor-stream-out%)]
                          [start exact-nonnegative-integer? 0]
                          [end (or/c exact-nonnegative-integer? 'eof) 'eof])
           boolean?]{

If @racket[start] is 0 and @racket[end] is @racket['eof] negative,
 then the entire contents are written to the stream. If @racket[end]
 is @racket['eof], then the contents are written from @racket[start]
 until the end of the editor. Otherwise, the contents of the given
 range are written.

}}
