#lang scribble/doc
@(require "common.ss"
          scribble/bnf)

@title{Editor Functions}


@defproc[(add-editor-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @scheme[keymap%] object, the keymap is loaded with mappable
 functions that apply to all @scheme[editor<%>] objects:

@itemize{ 
@item{@scheme["copy-clipboard"]}
@item{@scheme["copy-append-clipboard"]}
@item{@scheme["cut-clipboard"]}
@item{@scheme["cut-append-clipboard"]}
@item{@scheme["paste-clipboard"]}
@item{@scheme["paste-x-selection"]}
@item{@scheme["delete-selection"]}
@item{@scheme["clear-selection"]}
@item{@scheme["undo"]}
@item{@scheme["redo"]}
@item{@scheme["select-all"]}
}

}

@defproc[(add-pasteboard-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @scheme[keymap%] object, the table is loaded with mappable
 functions that apply to @scheme[pasteboard%] objects. Currently,
 there are no such functions.

See also
@scheme[add-editor-keymap-functions].

}

@defproc[(add-text-keymap-functions [keymap (is-a?/c keymap%)])
         void?]{

Given a @scheme[keymap%] object, the table is loaded with functions
 that apply to all @scheme[text%] objects:
@itemize{ 
@item{@scheme["forward-character"]}
@item{@scheme["backward-character"]}
@item{@scheme["previous-line"]}
@item{@scheme["next-line"]}
@item{@scheme["previous-page"]}
@item{@scheme["next-page"]}
@item{@scheme["forward-word"]}
@item{@scheme["backward-word"]}
@item{@scheme["forward-select"]}
@item{@scheme["backward-select"]}
@item{@scheme["select-down"]}
@item{@scheme["select-up"]}
@item{@scheme["select-page-up"]}
@item{@scheme["select-page-down"]}
@item{@scheme["forward-select-word"]}
@item{@scheme["backward-select-word"]}
@item{@scheme["beginning-of-file"]}
@item{@scheme["end-of-file"]}
@item{@scheme["beginning-of-line"]}
@item{@scheme["end-of-line"]}
@item{@scheme["select-to-beginning-of-file"]}
@item{@scheme["select-to-end-of-file"]}
@item{@scheme["select-to-beginning-of-line"]}
@item{@scheme["select-to-end-of-line"]}
@item{@scheme["copy-clipboard"]}
@item{@scheme["copy-append-clipboard"]}
@item{@scheme["cut-clipboard"]}
@item{@scheme["cut-append-clipboard"]}
@item{@scheme["paste-clipboard"]}
@item{@scheme["paste-x-selection"]}
@item{@scheme["delete-selection"]}
@item{@scheme["delete-previous-character"]}
@item{@scheme["delete-next-character"]}
@item{@scheme["clear-selection"]}
@item{@scheme["delete-to-end-of-line"]}
@item{@scheme["delete-next-word"]}
@item{@scheme["delete-previous-word"]}
@item{@scheme["delete-line"]}
@item{@scheme["undo"]}
@item{@scheme["redo"]}
}

See also
@scheme[add-editor-keymap-functions].

}

@defproc[(append-editor-font-menu-items [menu (or/c @scheme[menu%] (is-a?/c popup-menu%))])
         void?]{
Appends menu items to a given menu (not a popup menu) to implement a
 standard set of font-manipulation operations, such as changing the
 font face or style. The callback for each menu item uses
@xmethod[top-level-window<%> get-edit-target-object] (finding the frame by following a chain of parents until a frame is
 reached); if the result is an @scheme[editor<%>] object,
@xmethod[editor<%> change-style] is called on the editor.

}

@defproc[(append-editor-operation-menu-items [menu (or/c @scheme[menu%] (is-a?/c popup-menu%))]
                                             [text-only? any/c #t])
         void?]{
Appends menu items to a given menu (not a popup menu) to implement the
 standard editor operations, such as cut and paste. The callback for
 each menu item uses
@xmethod[top-level-window<%> get-edit-target-object] (finding the frame by following a chain of parents until a frame is
 reached); if the result is an @scheme[editor<%>] object,
@xmethod[editor<%> do-edit-operation] is called on the editor.



If @scheme[text-only?] is @scheme[#f], then menu items that insert
 non-text snips (such as @onscreen{Insert Image...}) are appended to
 the menu.



}

@defparam[current-text-keymap-initializer proc ((is-a?/c keymap%) . -> . any/c)]{

Parameter that specifies a keymap-initialization procedure. This
 procedure is called to initialize the keymap of a
 @scheme[text-field%] object or a @scheme[text%] object created by
 @scheme[graphical-read-eval-print-loop].

The initializer takes a keymap object and returns nothing. The default
 initializer chains the given keymap to an internal keymap that
 implements standard text editor keyboard and mouse bindings for cut,
 copy, paste, undo, and select-all. The right mouse button is mapped
 to popup an edit menu when the button is released. Under X,
 start-of-line (Ctl-A) and end-of-line (Ctl-E) are also mapped.

}

@defproc[(editor-set-x-selection-mode [on any/c])
         void?]{

Under X Windows, editor selections conform to the X Windows selection
conventions instead of a clipboard-based convention. If @scheme[on] is
@scheme[#f], the behavior is switched to the clipboard-based convention
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
Determines the interpretation of @litchar{m:} for a @scheme[keymap%]
mapping under Mac OS X. See also
@xmethod[keymap% map-function].


First case:


If @scheme[on?] is @scheme[#t], @litchar{m:} corresponds to the Command key. If
@scheme[on?] is @scheme[#f], then @litchar{m:} corresponds to no key under Mac OS
X.



Second case:


Returns @scheme[#t] if @litchar{m:} corresponds to Command,
 @scheme[#f] otherwise.

}

@defproc[(open-input-graphical-file [filename string?])
         input-port]{

Opens @scheme[filename] (in @scheme['binary] mode) and checks whether it looks
 like a ``graphical'' file in editor format. If the file does not
 appear to be an editor file, the file port is returned with line
 counting enabled. Otherwise, the file is loaded into an editor, and
 the result port is created with
@scheme[open-input-text-editor].


}

@defproc[(open-input-text-editor [text-editor (is-a?/c text%)]
                                 [start-position exact-nonnegative-integer? 0]
                                 [end-position (or/c exact-nonnegative-integer? (one/of 'end)) 'end]
                                 [snip-filter ((is-a?/c snip%) . -> . any/c) (lambda (s) s)]
                                 [port-name any/c @scheme[text-editor]]
                                 [expect-to-read-all? any/c #f])
         input-port]{

Creates an input port that draws its content from @scheme[text-editor].
 The editor content between positions @scheme[start-position] and
 @scheme[end-position] is the content of the port. If @scheme[end-position]
 is @scheme['end], the content runs until the end of the editor. If a
 snip that is not a @scheme[string-snip%] object spans
 @scheme[start-position] or @scheme[end-position], the entire snip
 contributes to the port. If a @scheme[string-snip%] instance spans
 @scheme[start-position], only the part of the snip after
 @scheme[start-position] contributes, and if a @scheme[string-snip%]
 object spans @scheme[end-position], only the part before
 @scheme[end-position] contributes.

An instance of @scheme[string-snip%] in @scheme[text-editor] generates
 a character sequence in the resulting port. All other kinds of snips
 are passed to @scheme[snip-filter] to obtain a ``special'' value for
 the port.  If a snip is returned as the first result from
 @scheme[snip-filter], and if the snip is an instance of
 @scheme[readable-snip<%>], the snip generates a special value for the
 port through the @method[readable-snip<%> read-special] method. If
 @scheme[snip-filter] returns any other kind of snip, it is copied for
 the special result. Finally, a non-snip first result from
 @scheme[snip-filter] is used directly as the special result.

The @scheme[port-name] argument is used for the input port's name. The
 @scheme[expect-to-read-all?] argument is a performance hint; use
 @scheme[#t] if the entire port's stream will be read.

The result port must not be used if @scheme[text-editor] changes in any
 of the following ways: a snip is inserted (see
@method[text% after-insert]), a snip is deleted  (see
@method[text% after-delete]), a snip is split  (see
@method[text% after-split-snip]), snips are merged  (see
@method[text% after-merge-snips]), or a snip changes its count (which is rare; see
@method[snip-admin% recounted]). The
@method[text% get-revision-number] method can be used to detect any of these changes.



}

@defproc[(open-output-text-editor [text-editor (is-a?/c text%)]
                                  [start-position (or/c exact-nonnegative-integer? (one/of 'end)) 'end]
                                  [special-filter (any/c . -> . any/c) (lambda (x) x)]
                                  [port-name any/c @scheme[text-editor]])
         output-port]{

Creates an output port that delivers its content to @scheme[text-editor].
 The content is written to @scheme[text-editor] starting at the position
 @scheme[start-position], where @scheme['end] indicates that output should
 start at the text editor's current end position.

If @scheme[special-filter] is provided, it is applied to any value
 written to the port with @scheme[write-special], and the result is
 inserted in its place. If a special value is a @scheme[snip%]
 object, it is inserted into the editor. Otherwise, the special value
 is @scheme[display]ed into the editor.

If line counting is enabled for the resulting output port, then the
 port will report the line, offset from the line's start, and position
 within the editor at which the port writes data.



}

@defproc[(read-editor-global-footer [in (is-a?/c editor-stream-in%)])
         boolean?]{

See 
@scheme[read-editor-global-header]. Call
@scheme[read-editor-global-footer] even if
@scheme[read-editor-global-header] returns @scheme[#f].



}

@defproc[(read-editor-global-header [in (is-a?/c editor-stream-in%)])
         boolean?]{

Reads data from @scheme[in] to initialize for reading editors from the
stream. The return value is @scheme[#t] if the read succeeds, or @scheme[#f]
otherwise.

One or more editors can be read from the stream by calling
the editor's 
@method[editor<%> read-from-file] method. (The number of editors to be
read must be known by the application beforehand.) When all editors
are read, call 
@scheme[read-editor-global-footer]. Calls to 
@scheme[read-editor-global-header] and
@scheme[read-editor-global-footer] must bracket any call to 
@method[editor<%> read-from-file], and only one stream at a time
can be read using these methods or written using
@scheme[write-editor-global-header] and
@scheme[write-editor-global-footer].

When reading from streams that span PLT Scheme versions, use
@scheme[read-editor-version] before this procedure.



}

@defproc[(read-editor-version [in (is-a?/c editor-stream-in%)]
                              [in-base (is-a?/c editor-stream-in-base%)]
                              [parse-format? any/c]
                              [raise-errors? any/c #t])
         boolean?]{

Reads version information from @scheme[in-base], where @scheme[in-base] is
 the base for @scheme[in]. The version information parsed from
 @scheme[in-base] is recorded in @scheme[in] for later version-sensitive
 parsing. The procedure result is true if the version information was
 read successfully and if the version is supported.

If @scheme[parse-format?] is true, then @scheme[in-base] is checked for an
 initial @scheme["WXME"] format indicator. Use @scheme[#f] when
 @scheme["WXME"] has been consumed already by format-dispatching code.

If @scheme[raise-errors?] is true, then an error in reading triggers an
 exception, instead of a @scheme[#f] result.



}

@defproc[(text-editor-load-handler [filename path string]
                                   [expected-module-name (or/c symbol false/c)])
         any/c]{

This procedure is a load handler for use with @scheme[current-load].

The handler recognizes PLT Scheme editor-format files (see
 @secref["editorfileformat"]) and decodes them for loading. It is
 normally installed as MrEd starts (see @secref[#:doc reference-doc
 "running-sa"]).

The handler recognizes editor files by the first twelve characters of
 the file: @litchar{WXME01}@nonterm{digit}@nonterm{digit}@litchar{ ## }.
 Such a file is opened for loading by creating a @scheme[text%]
 object, loading the file into the object with @method[editor<%>
 insert-file], and then converting the editor content into a port with
 @scheme[open-input-text-editor]. After obtaining a port in this way,
 the content is read in essentially the same way as by the default
 MzScheme load handler. The difference is that the editor may contain
 instances of @scheme[readable-snip<%>], which are ``read'' though the
 snips' @method[readable-snip<%> read-special] method; see
 @scheme[open-input-text-editor] for details.


}

@defthing[the-editor-wordbreak-map (is-a?/c editor-wordbreak-map%)]{

See @scheme[editor-wordbreak-map%].

}

@defthing[the-style-list (is-a?/c style-list%)]{

See @scheme[style-list%].

}

@defproc[(write-editor-global-footer [out (is-a?/c editor-stream-out%)])
         boolean?]{

See @scheme[write-editor-global-header]. Call
 @scheme[write-editor-global-footer] even if
 @scheme[write-editor-global-header] returns @scheme[#f].



}

@defproc[(write-editor-global-header [out (is-a?/c editor-stream-out%)])
         boolean?]{

Writes data to @scheme[out], initializing it for writing editors to
 the stream. The return value is @scheme[#t] if the write succeeds, or
 @scheme[#f] otherwise.

One or more editors can be written to the stream by calling the
 editor's @method[editor<%> write-to-file] method. When all editors
 are written, call @scheme[write-editor-global-footer]. Calls to
 @scheme[write-editor-global-header] and
 @scheme[write-editor-global-footer] must bracket any call to
 @method[editor<%> write-to-file], and only one stream at a time can
 be written using these methods or read using
 @scheme[read-editor-global-header] and
 @scheme[read-editor-global-footer].

To support streams that span PLT Scheme versions, use
 @scheme[write-editor-version] before this procedure.

See also @secref["editorfileformat"].

}

@defproc[(write-editor-version [out (is-a?/c editor-stream-out%)]
                               [out-base (is-a?/c editor-stream-out-base%)])
         boolean?]{

Writes version information to @scheme[out-base] in preparation for
 writing editor information to the stream @scheme[out].

The @scheme[out] argument is currently not used, but @scheme[out-base]
 should be the base for @scheme[out]. In the future, @scheme[out] may record
 information about the version for later version-sensitive output.

The result is @scheme[#t] if the write succeeded, @scheme[#f] otherwise.

}
