#lang scribble/doc
@(require "common.rkt" scribble/bnf)

@title{Editor Functions}


@defproc[(add-editor-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @racket[keymap%] object, the keymap is loaded with mappable
 functions that apply to all @racket[editor<%>] objects:

@itemize[ 
@item{@racket["copy-clipboard"]}
@item{@racket["copy-append-clipboard"]}
@item{@racket["cut-clipboard"]}
@item{@racket["cut-append-clipboard"]}
@item{@racket["paste-clipboard"]}
@item{@racket["paste-x-selection"]}
@item{@racket["delete-selection"]}
@item{@racket["clear-selection"]}
@item{@racket["undo"]}
@item{@racket["redo"]}
@item{@racket["select-all"]}
]

}

@defproc[(add-pasteboard-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @racket[keymap%] object, the table is loaded with mappable
 functions that apply to @racket[pasteboard%] objects. Currently,
 there are no such functions.

See also
@racket[add-editor-keymap-functions].

}

@defproc[(add-text-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @racket[keymap%] object, the table is loaded with functions
 that apply to all @racket[text%] objects:
@itemize[ 
@item{@racket["forward-character"]}
@item{@racket["backward-character"]}
@item{@racket["previous-line"]}
@item{@racket["next-line"]}
@item{@racket["previous-page"]}
@item{@racket["next-page"]}
@item{@racket["forward-word"]}
@item{@racket["backward-word"]}
@item{@racket["forward-select"]}
@item{@racket["backward-select"]}
@item{@racket["select-down"]}
@item{@racket["select-up"]}
@item{@racket["select-page-up"]}
@item{@racket["select-page-down"]}
@item{@racket["forward-select-word"]}
@item{@racket["backward-select-word"]}
@item{@racket["beginning-of-file"]}
@item{@racket["end-of-file"]}
@item{@racket["beginning-of-line"]}
@item{@racket["end-of-line"]}
@item{@racket["select-to-beginning-of-file"]}
@item{@racket["select-to-end-of-file"]}
@item{@racket["select-to-beginning-of-line"]}
@item{@racket["select-to-end-of-line"]}
@item{@racket["copy-clipboard"]}
@item{@racket["copy-append-clipboard"]}
@item{@racket["cut-clipboard"]}
@item{@racket["cut-append-clipboard"]}
@item{@racket["paste-clipboard"]}
@item{@racket["paste-x-selection"]}
@item{@racket["delete-selection"]}
@item{@racket["delete-previous-character"]}
@item{@racket["delete-next-character"]}
@item{@racket["clear-selection"]}
@item{@racket["delete-to-end-of-line"]}
@item{@racket["delete-next-word"]}
@item{@racket["delete-previous-word"]}
@item{@racket["delete-line"]}
@item{@racket["undo"]}
@item{@racket["redo"]}
]

See also
@racket[add-editor-keymap-functions].

}

@defproc[(append-editor-font-menu-items [menu (or/c (is-a?/c menu%) (is-a?/c popup-menu%))])
         void?]{
Appends menu items to @racket[menu] to implement a
 standard set of font-manipulation operations, such as changing the
 font face or style. The callback for each menu item uses
@xmethod[top-level-window<%> get-edit-target-object] (finding the frame by following a chain of parents until a frame is
 reached); if the result is an @racket[editor<%>] object,
@xmethod[text% change-style] or @xmethod[pasteboard% change-style] is called on the editor.

}

@defproc[(append-editor-operation-menu-items [menu (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
                                             [text-only? any/c #t]
                                             [#:popup-position 
                                              popup-position 
                                              (or/c #f (list/c (is-a?/c text%) exact-nonnegative-integer?))
                                              #f])
         void?]{
Appends menu items to @racket[menu] to implement the
 standard editor operations, such as cut and paste. The callback for
 each menu item uses
@xmethod[top-level-window<%> get-edit-target-object] (finding the frame by following a chain of parents until a frame is
 reached); if the result is an @racket[editor<%>] object,
@xmethod[editor<%> do-edit-operation] is called on the editor.

If @racket[text-only?] is @racket[#f], then menu items that insert
 non-text snips (such as @onscreen{Insert Image...}) are appended to
 the menu.

If @racket[popup-position] is not @racket[#f], then @racket[append-editor-operation-menu-items]
is expected to have been called to build a popup menu and the two elements
of the list should be the @racket[text%] object where the mouse was clicked
for the popup menu and the position where the click happened. In that case,
the @onscreen{Copy} and @onscreen{Cut} menus are enabled when the click
lands on a snip that is not a @racket[string-snip%], and the corresponding
callbacks will copy and cut that one snip.

}

@defparam[current-text-keymap-initializer proc ((is-a?/c keymap%) . -> . any/c)]{

Parameter that specifies a keymap-initialization procedure. This
 procedure is called to initialize the keymap of a
 @racket[text-field%] object or a @racket[text%] object created by
 @racket[graphical-read-eval-print-loop].

The initializer takes a keymap object and returns nothing. The default
 initializer chains the given keymap to an internal keymap that
 implements standard text editor keyboard and mouse bindings for cut,
 copy, paste, undo, and select-all. The right mouse button is mapped
 to popup an edit menu when the button is released. On Unix,
 start-of-line (Ctl-A) and end-of-line (Ctl-E) are also mapped.

}

@defproc[(editor-set-x-selection-mode [on any/c])
         void?]{

On Unix, editor selections conform to the X11 Windows selection
conventions. If @racket[on] is
@racket[#f], the behavior is switched exclusively to the clipboard-based convention
(where copy must be explicitly requested before a paste).

}

@defproc[(get-the-editor-data-class-list)
         (is-a?/c editor-data-class-list<%>)]{

Gets the editor data class list instance for the current eventspace.



}

@defproc[(get-the-snip-class-list)
         (is-a?/c snip-class-list<%>)]{

Gets the snip class list instance for the current eventspace.



}

@defproc*[([(map-command-as-meta-key [on? any/c])
            void?]
           [(map-command-as-meta-key)
            boolean?])]{
Determines the interpretation of @litchar{m:} for a @racket[keymap%]
mapping on Mac OS X. See also
@xmethod[keymap% map-function].


First case:


If @racket[on?] is @racket[#t], @litchar{m:} corresponds to the Command key. If
@racket[on?] is @racket[#f], then @litchar{m:} corresponds to no key on Mac OS
X.



Second case:


Returns @racket[#t] if @litchar{m:} corresponds to Command,
 @racket[#f] otherwise.

}

@defproc[(open-input-graphical-file [filename string?])
         input-port?]{

Opens @racket[filename] (in @racket['binary] mode) and checks whether it looks
 like a ``graphical'' file in editor format. If the file does not
 appear to be an editor file, the file port is returned with line
 counting enabled. Otherwise, the file is loaded into an editor, and
 the result port is created with
@racket[open-input-text-editor].


}

@defproc[(open-input-text-editor [text-editor (is-a?/c text%)]
                                 [start-position exact-nonnegative-integer? 0]
                                 [end-position (or/c exact-nonnegative-integer? 'end) 'end]
                                 [snip-filter ((is-a?/c snip%) . -> . any/c) (lambda (s) s)]
                                 [port-name any/c text-editor]
                                 [expect-to-read-all? any/c #f]
                                 [#:lock-while-reading? lock-while-reading? any/c #f])
         input-port]{

Creates an input port that draws its content from @racket[text-editor].
 The editor content between positions @racket[start-position] and
 @racket[end-position] is the content of the port. If @racket[end-position]
 is @racket['end], the content runs until the end of the editor. If a
 snip that is not a @racket[string-snip%] object spans
 @racket[start-position] or @racket[end-position], the entire snip
 contributes to the port. If a @racket[string-snip%] instance spans
 @racket[start-position], only the part of the snip after
 @racket[start-position] contributes, and if a @racket[string-snip%]
 object spans @racket[end-position], only the part before
 @racket[end-position] contributes.

An instance of @racket[string-snip%] in @racket[text-editor] generates
 a character sequence in the resulting port. All other kinds of snips
 are passed to @racket[snip-filter] to obtain a ``special'' value for
 the port.  If a snip is returned as the first result from
 @racket[snip-filter], and if the snip is an instance of
 @racket[readable-snip<%>], the snip generates a special value for the
 port through the @method[readable-snip<%> read-special] method. If
 @racket[snip-filter] returns any other kind of snip, it is copied for
 the special result. Finally, a non-snip first result from
 @racket[snip-filter] is used directly as the special result.

The @racket[port-name] argument is used for the input port's name. The
 @racket[expect-to-read-all?] argument is a performance hint; use
 @racket[#t] if the entire port's stream will be read.

The result port must not be used if @racket[text-editor] changes in any
 of the following ways: a snip is inserted (see
@method[text% after-insert]), a snip is deleted  (see
@method[text% after-delete]), a snip is split  (see
@method[text% after-split-snip]), snips are merged  (see
@method[text% after-merge-snips]), or a snip changes its count (which is rare; see
@method[snip-admin% recounted]). The
@method[text% get-revision-number] method can be used to detect any of these changes.

To help guard against such uses, if @racket[lock-while-reading?] argument is
a true value, then @racket[open-input-text-editor] will 
@method[editor<%> lock] the @racket[text-editor] and call
@method[editor<%> begin-edit-sequence]
before it returns and un@method[editor<%> lock] it and
call @method[editor<%> end-edit-sequence]
after it is safe to use the above methods. (In some
cases, it will not @method[editor<%> lock] the editor 
or put it in an edit sequence at all, 
if using those methods are always safe.)

}

@defproc[(open-output-text-editor [text-editor (is-a?/c text%)]
                                  [start-position (or/c exact-nonnegative-integer? (one/of 'end)) 'end]
                                  [special-filter (any/c . -> . any/c) (lambda (x) x)]
                                  [port-name any/c text-editor]
                                  [#:eventspace eventspace (or/c eventspace? #f) (current-eventspace)])
         output-port]{

Creates an output port that delivers its content to @racket[text-editor].
 The content is written to @racket[text-editor] starting at the position
 @racket[start-position], where @racket['end] indicates that output should
 start at the text editor's current end position.

If @racket[special-filter] is provided, it is applied to any value
 written to the port with @racket[write-special], and the result is
 inserted in its place. If a special value is a @racket[snip%]
 object, it is inserted into the editor. Otherwise, the special value
 is @racket[display]ed into the editor.

If line counting is enabled for the resulting output port, then the
 port will report the line, offset from the line's start, and position
 within the editor at which the port writes data.

If @racket[eventspace] is not @racket[#f], then when the output port
 is used in a thread other than @racket[eventspace]'s handler thread,
 content is delivered to @racket[text-editor] through a low-priority
 callback in @racket[eventspace].  Thus, if @racket[eventspace]
 corresponds to the eventspace for the editor's @tech{displays},
 writing to the output port is safe from any thread.

If @racket[eventspace] is @racket[#f], beware that the port is only
 weakly thread-safe. Content is delivered to @racket[text-editor] in
 an @tech{edit sequence}, but an edit sequence is not enough
 synchronization if, for example, the editor is displayed in an
 enabled @racket[editor-canvas%]. See @secref["editorthreads"] for
 more information.}


@defproc[(read-editor-global-footer [in (is-a?/c editor-stream-in%)])
         boolean?]{

See 
@racket[read-editor-global-header]. Call
@racket[read-editor-global-footer] even if
@racket[read-editor-global-header] returns @racket[#f].



}

@defproc[(read-editor-global-header [in (is-a?/c editor-stream-in%)])
         boolean?]{

Reads data from @racket[in] to initialize for reading editors from the
stream. The return value is @racket[#t] if the read succeeds, or @racket[#f]
otherwise.

One or more editors can be read from the stream by calling
the editor's 
@method[editor<%> read-from-file] method. (The number of editors to be
read must be known by the application beforehand.) When all editors
are read, call 
@racket[read-editor-global-footer]. Calls to 
@racket[read-editor-global-header] and
@racket[read-editor-global-footer] must bracket any call to 
@method[editor<%> read-from-file], and only one stream at a time
can be read using these methods or written using
@racket[write-editor-global-header] and
@racket[write-editor-global-footer].

When reading from streams that span Racket versions, use
@racket[read-editor-version] before this procedure.



}

@defproc[(read-editor-version [in (is-a?/c editor-stream-in%)]
                              [in-base (is-a?/c editor-stream-in-base%)]
                              [parse-format? any/c]
                              [raise-errors? any/c #t])
         boolean?]{

Reads version information from @racket[in-base], where @racket[in-base] is
 the base for @racket[in]. The version information parsed from
 @racket[in-base] is recorded in @racket[in] for later version-sensitive
 parsing. The procedure result is true if the version information was
 read successfully and if the version is supported.

If @racket[parse-format?] is true, then @racket[in-base] is checked for an
 initial @racket["WXME"] format indicator. Use @racket[#f] when
 @racket["WXME"] has been consumed already by format-dispatching code.

If @racket[raise-errors?] is true, then an error in reading triggers an
 exception, instead of a @racket[#f] result.



}

@defproc[(text-editor-load-handler [filename path string]
                                   [expected-module-name (or/c symbol? #f)])
         any/c]{

This procedure is a load handler for use with @racket[current-load].

The handler recognizes Racket editor-format files (see
 @secref["editorfileformat"]) and decodes them for loading. It is
 normally installed as GRacket starts (see @secref[#:doc reference-doc
 "running-sa"]).

The handler recognizes editor files by the first twelve characters of
 the file: @litchar{WXME01}@nonterm{digit}@nonterm{digit}@litchar{ ## }.
 Such a file is opened for loading by creating a @racket[text%]
 object, loading the file into the object with @method[editor<%>
 insert-file], and then converting the editor content into a port with
 @racket[open-input-text-editor]. After obtaining a port in this way,
 the content is read in essentially the same way as by the default
 Racket load handler. The difference is that the editor may contain
 instances of @racket[readable-snip<%>], which are ``read'' though the
 snips' @method[readable-snip<%> read-special] method; see
 @racket[open-input-text-editor] for details.


}

@defthing[the-editor-wordbreak-map (is-a?/c editor-wordbreak-map%)]{

See @racket[editor-wordbreak-map%].

}

@defthing[the-style-list (is-a?/c style-list%)]{

See @racket[style-list%].

}

@defproc[(write-editor-global-footer [out (is-a?/c editor-stream-out%)])
         boolean?]{

See @racket[write-editor-global-header]. Call
 @racket[write-editor-global-footer] even if
 @racket[write-editor-global-header] returns @racket[#f].



}

@defproc[(write-editor-global-header [out (is-a?/c editor-stream-out%)])
         boolean?]{

Writes data to @racket[out], initializing it for writing editors to
 the stream. The return value is @racket[#t] if the write succeeds, or
 @racket[#f] otherwise.

One or more editors can be written to the stream by calling the
 editor's @method[editor<%> write-to-file] method. When all editors
 are written, call @racket[write-editor-global-footer]. Calls to
 @racket[write-editor-global-header] and
 @racket[write-editor-global-footer] must bracket any call to
 @method[editor<%> write-to-file], and only one stream at a time can
 be written using these methods or read using
 @racket[read-editor-global-header] and
 @racket[read-editor-global-footer].

To support streams that span Racket versions, use
 @racket[write-editor-version] before this procedure.

See also @secref["editorfileformat"].

}

@defproc[(write-editor-version [out (is-a?/c editor-stream-out%)]
                               [out-base (is-a?/c editor-stream-out-base%)])
         boolean?]{

Writes version information to @racket[out-base] in preparation for
 writing editor information to the stream @racket[out].

The @racket[out] argument is currently not used, but @racket[out-base]
 should be the base for @racket[out]. In the future, @racket[out] may record
 information about the version for later version-sensitive output.

The result is @racket[#t] if the write succeeded, @racket[#f] otherwise.

}
