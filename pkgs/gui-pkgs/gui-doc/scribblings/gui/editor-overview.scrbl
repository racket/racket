#lang scribble/doc
@(require scribble/bnf "common.rkt")

@title[#:tag "editor-overview"]{Editors}

The editor toolbox provides a foundation for two common kinds of
 applications:

@itemize[

 @item{@italic{Programs that need a sophisticated text editor} ---
 The simple text field control is inadequate for text-intensive
 applications.  Many programs need editors that can handle multiple
 fonts and non-text items.}

 @item{@italic{Programs that need a canvas with dragable objects} ---
 The drawing toolbox provides a generic drawing surface for plotting
 lines and boxes, but many applications need an interactive canvas,
 where the user can drag and resize individual objects.}

]

Both kinds of applications need an extensible editor that can handle
 text, images, programmer-defined items, and even embedded
 editors. The difference between them is the layout of items. The
 editor toolbox therefore provides two kinds of editors via two
 classes:

@itemize[

 @item{@racket[text%] --- in a @deftech{text editor}, items are
 automatically positioned in a paragraph flow.}

 @item{@racket[pasteboard%] --- in a @deftech{pasteboard editor},
 items are explicitly positioned and dragable.}

]

This editor architecture addresses the full range of real-world
 issues for an editor---including cut-and-paste, extensible file
 formats, and layered text styles---while supporting a high level of
 extensibility.  Unfortunately, the system is fairly complex as a
 result, and using the editor classes effectively requires a solid
 understanding of the structure and terminology of the editor
 toolbox. Nevertheless, enough applications fit one (or both) of the
 descriptions above to justify the depth and complexity of the toolbox
 and the learning investment required to use it.

A brief example illustrates how editors work. To start, an editor
 needs an @racket[editor-canvas%] to display its contents. Then, we
 can create a text editor and install it into the canvas:

@racketblock[
(define f (new frame% [label "Simple Edit"]
                      [width 200]
                      [height 200]))
(define c (new editor-canvas% [parent f]))
(define t (new text%))
(send c #,(:: editor-canvas% set-editor) t)
(send f #,(:: top-level-window<%> show) #t)
]

At this point, the editor is fully functional: the user can type text
 into the editor, but no cut-and-paste operations are available. We
 can support all of the standard operations on an editor via the
 menu bar:

@racketblock[
(define mb (new menu-bar% [parent f]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
]

Now, the standard cut and paste operations work, and the user can even
 set font styles. The user can also insert an embedded editor by
 selecting @onscreen{Insert Text} from the @onscreen{Edit} menu; after
 selecting the menu item, a box appears in the editor with the caret
 inside. Typing with the caret in the box stretches the box as text is
 added, and font operations apply wherever the caret is active. Text
 on the outside of the box is rearranged as the box changes
 sizes. Note that the box itself can be copied and pasted.

The content of an editor is made up of @defterm{@tech{snips}}. An
 embedded editor is a single snip from the embedding editor's
 point-of-view. To encode immediate text, a snip can be a single
 character, but more often a snip is a sequence of adjacent characters
 on the same line. The @method[text% find-snip] method extracts a snip
 from a text editor:

@racketblock[
(send t #,(:: text% find-snip) 0 'after)
]

The above expression returns the first snip in the editor, which may
 be a string snip (for immediate text) or an editor snip (for an
 embedded editor).

An editor is not permanently attached to any display. We can take the
 text editor out of our canvas and put a pasteboard editor in the
 canvas, instead:

@racketblock[
(define pb (new pasteboard%))
(send c #,(:: editor-canvas% set-editor) pb)
]

With the pasteboard editor installed, the user can no longer type
 characters directly into the editor (because a pasteboard does not
 support directly entered text). However, the user can cut text from
 elsewhere and paste it into pasteboard, or select one of the
 @onscreen{Insert} menu items in the @onscreen{Edit} menu. Snips are
 clearly identifiable in a pasteboard editor (unlike a text editor)
 because each snip is separately dragable.

We can insert the old text editor (which we recently removed from the
 canvas) as an embedded editor in the pasteboard by explicitly
 creating an editor snip:

@racketblock[
(define s (make-object editor-snip% t)) (code:comment @#,t{@racket[t] is the old text editor})
(send pb #,(:: editor<%> insert) s)
]

An individual snip cannot be inserted into different editors at the
 same time, or inserted multiple times in the same editor:

@racketblock[
(send pb #,(:: editor<%> insert) s) (code:comment @#,t{no effect})
]

However, we can make a deep copy of the snip and insert the copy into
 the pasteboard:

@racketblock[
(send pb #,(:: editor<%> insert) (send s #,(:: snip% copy)))
]

Applications that use the editor classes typically derive new versions
 of the @racket[text%] and @racket[pasteboard%] classes. For
 example, to implement an append-only editor (which allows insertions
 only at the end and never allows deletions), derive a new class from
 @racket[text%] and override the
 @method[text% can-insert?] and
 @method[text% can-delete?] methods:

@racketblock[
(define append-only-text% 
  (class text%
    (inherit #,(:: text% last-position))
    (define/augment (#,(:: text% can-insert?) s l) (= s (#,(:: text% last-position))))
    (define/augment (#,(:: text% can-delete?) s l) #f)
    (super-new)))
]

@section[#:tag "tb:miaoverview"]{Editor Structure and Terminology}

The editor toolbox supports extensible and nestable editors by
 decomposing an editor assembly into three functional parts:

@itemize[

 @item{The @deftech{editor} itself stores the state of the text or
 pasteboard and handles most events and editing operations. The
 @racket[editor<%>] interface defines the core editor functionality,
 but editors are created as instances of @racket[text%] or
 @racket[pasteboard%].}

 @item{A @deftech{snip} is a segment of information within the
 editor.  Each snip can contain a sequence of characters, a picture,
 or an interactive object (such as an embedded editor). In a text
 editor, snips are constrained to fit on a single line and generally
 contain data of a single type. The @racket[snip%] class implements a
 basic snip. Other snip classes include @racket[string-snip%] for
 managing text, @racket[image-snip%] for managing pictures, and
 @racket[editor-snip%] for managing embedded editors.}

 @item{A @deftech{display} presents the editor on the screen. The
 display lets the user scroll around an editor or change editors. Most
 displays are instances of the @racket[editor-canvas%] class, but the
 @racket[editor-snip%] class also acts as a display for embedded
 editors.}

]

These three parts are illustrated by a simple word processor. The
 editor corresponds to the text document. The editor object receives
 keyboard and mouse commands for editing the text. The text itself is
 distributed among snips. Each character could be a separate snip, or
 multiple characters on a single line could be grouped together into a
 snip. The display roughly corresponds to the window in which the
 text is displayed.  While the editor manages the arrangement of the
 text as it is displayed into a window, the display determines which
 window to draw into and which part of the editor to display.

Each selectable entity in an editor is an @deftech{item}. In a
 pasteboard, all selection and dragging operations work on snips, so
 there is a one-to-one correspondence between snips and items.  In an
 editor, one snip contains one or more consecutive items, and every
 item belongs to some snip. For example, in a simple text editor, each
 character is an item, but multiple adjacent characters may be grouped
 into a single snip. The number of items in a snip is the snip's
 @deftech{count}.

Each place where the insertion point can appear in a text editor is a
 @deftech{position}. A text editor with @math{n} items contains
 @math{n+1} positions: one position before each item, and one position
 after the last item.

The order of snips within a pasteboard determines each snip's drawing
 plane. When two snips overlap within the pasteboard, the snip that is
 earlier in the order is in front of the other snip (i.e., the former
 is drawn after the latter, such that the former snip may cover part
 of the latter snip).

When an editor is drawn into a display, each snip and position has a
 @deftech{location}. The location of a position or snip is specified
 in coordinates relative to the top-left corner of the
 editor. Locations in an editor are only meaningful when the editor is
 displayed.


@subsection[#:tag "editoradministrators"]{Administrators}

Two extra layers of administration manage the @techlink{display}-editor and
 editor-snip connections. An editor never communicates directly with
 a @techlink{display}; instead, it always communicates with an @deftech{editor
 administrator}, an instance of the @racket[editor-admin%] class,
 which relays information to the @techlink{display}. Similarly, a snip
 communicates with a @deftech{snip administrator}, an instance of the
 @racket[snip-admin%] class.

The administrative layers make the editor hierarchy flexible without
 forcing every part of an editor assembly to contain the functionality
 of several parts. For example, a text editor can be a single
 @techlink{item} within another editor; without administrators, the
 @racket[text%] class would also have to contain all the functionality
 of a @techlink{display} (for the containing editor) and a snip (for
 the embedded editor). Using administrators, an editor class can serve
 as both a containing and an embedded editor without directly
 implementing the @techlink{display} and snip functionality.

A snip belongs to at most one editor via a single administrator. An
 editor also has only one administrator at a time. However, the
 administrator that connects the an editor to the standard
 @techlink{display} (i.e., an editor canvas) can work with other such
 administrators. In particular, the administrator of an
 @racket[editor-canvas%] (each one has its own administrator) can work
 with other @racket[editor-canvas%] administrators, allowing an editor
 to be displayed in multiple @racket[editor-canvas%] windows at the
 same time.

When an editor is displayed by multiple canvases, one of the canvases'
 administrators is used as the editor's primary administrator. To
 handle user and update events for other canvases, the editor's
 administrator is temporarily changed and then restored through the
 editor's @method[editor<%> set-admin] method. The return value of the
 editor's @method[editor<%> get-admin] method thus depends on the
 context of the call.

@subsection[#:tag "editorstyles"]{Styles}

A @deftech{style}, an instance of the @racket[style<%>] interface,
 parameterizes high-level display information that is common to all
 snip classes. This includes the font, color, and alignment for
 drawing the item. A single style is attached to each snip.

Styles are hierarchical: each style is defined in terms of another
 style. @index*['("Basic style") (list @elem{@racket["Basic"]
 style})]{There} is a single @deftech{root style}, named
 @racket["Basic"], from which all other styles in an editor are
 derived. The difference between a base style and each of its derived
 style is encoded in a @deftech{style delta} (or simply
 @deftech{delta}). A delta encodes changes such as

@itemize[

 @item{change the font family to @italic{X};}

 @item{enlarge the font by adding @italic{Y} to the point size;}

 @item{toggle the boldness of the font; or}
 
 @item{change everything to match the style description @italic{Z}.}

]

Style objects are never created separately; rather, they are always
 created through a @deftech{style list}, an instance of the
 @racket[style-list%] class. A style list manages the styles,
 servicing external requests to find a particular style, and it
 manages the hierarchical relationship between styles.  A global style
 list is available, @indexed-racket[the-style-list], but new style
 lists can be created for managing separate style hierarchies. For
 example, each editor will typically have its own style list.

Each new style is defined in one of two ways:

@itemize[

 @item{A @deftech{derived style} is defined in terms of a base style
 and a delta. Every style (except for the root style) has a base
 style, even if it does not depend on the base style in any way (i.e.,
 the delta describes a fixed style rather than extensions to an
 existing style). (This is the usual kind of style inheritance, as
 found in word processors such as Microsoft Word.)}

 @item{A @deftech{join style} is defined in terms of two other styles:
 a base style and a @deftech{shift style}. The meaning of a join style
 is determined by reinterpreting the shift style; in the
 reinterpretation, the base style is used as the @italic{root} style
 for the shift style. (This is analogous to multi-level
 styles, like the paragraph and character styles in FrameMaker. In
 this analogy, the paragraph style is the base style, and the
 character style is the shift style.  However, FrameMaker allows only
 those two levels; with join styles support any number of levels.)}

]

@index*['("Standard style") (list @elem{@racket["Standard"]
 style})]{Usually}, when text is inserted into a text editor, it
 inherits the style of the preceding snip. If text is inserted into an
 empty editor, the text is usually assigned a style called
 @racket["Standard"]. By default, the @racket["Standard"] style is
 unmodified from the root style. The default style name can be changed
 by overriding @method[editor<%> default-style-name].

The exception to the above is when @xmethod[text% change-style] is
 called with the current selection @techlink{position} (when the
 selection is a @techlink{position} and not a range). In that case,
 the style is remembered, and if the next editor-modifying action is a
 text insertion, the inserted text gets the remembered style.

See @xmethod[text% get-styles-sticky] for more information about the
 style of inserted text.


@section[#:tag "editorfileformat"]{File Format}

To allow editor content to be saved to a file, the editor classes
 implement a special file format called @deftech{WXME}. (The format is
 used when cutting and pasting between applications or eventspaces,
 too). The file format is not documented, except that it begins
 @litchar{WXME01}@nonterm{digit}@nonterm{digit}@litchar{ ## }. Otherwise, the
 @method[editor<%> load-file] and @method[editor<%> save-file] methods
 define the format internally. The file format is the same for text
 and pasteboard editors. When a pasteboard saves its content to a
 file, it saves the snips from front to back, and also includes extra
 location information. The @racketmodname[wxme] library provides
 utilities for manipulating WXME files.

Editor data is read and written using @racket[editor-stream-in%] and
@racket[editor-stream-out%] objects.  Editor information can only be
 read from or written to one stream at a time. To write one or more
 editors to a stream, first call the function
 @racket[write-editor-global-header] to write initialization data into
 an output stream. When all editors are written to the stream, call
 @racket[write-editor-global-footer]. Similarly, reading editors from
 a stream is initialized with @racket[read-editor-global-header] and
 finalized with @racket[read-editor-global-footer]. Optionally, to
 support streams that span versions of Racket, use
 @racket[write-editor-version] and @racket[read-editor-version] before
 the header operations.

The editor file data format can be embedded within another file, and
 it can be extended with new kinds of data. The editor file format can
 be extended in two ways: with snip- or content-specific data, and
 with editor-specific global data. These are described in the
 remainder of this section.

@subsection{Encoding Snips}

@index['("snips" "saving")]{@index['("snips" "cut and paste")]{The}}
 generalized notion of a snip allows new snip types to be defined and
 immediately used in any editor class. Also, when two applications
 support the same kinds of snips, snip data can easily be cut and
 pasted between them, and the same data files will be readable by each
 program. This interoperability is due to a consistent encoding
 mechanism that is built into the snip system.

Graceful and extensible encoding of snips requires that 
 two issues are addressed:

@itemize[

 @item{The encoding function for a snip can be associated with the snip
 itself. To convert a snip from an encoded representation (e.g., as
 bytes in a file) to a memory object, a decoding function must be
 provided for each type of snip. Furthermore, a list of such decoders
 must be available to the high-level decoding process. This decoding
 mapping is defined by associating a @deftech{snip class} object to
 every snip. A snip class is an instance of the @racket[snip-class%]
 class.}

 @item{Some editors may require additional information to be stored
 about a snip; this information is orthogonal to the type-specific
 information stored by the snip itself. For example, a pasteboard
 needs to remember a snip's @techlink{location}, while a text editor
 does not need this information.  If data is being cut and pasted from
 one pasteboard to another, then information about relative
 @techlink{location}s needs to be maintained, but this information
 should not inhibit pasting into an editor. Extra data is associated
 with a snip through @deftech{editor data} objects, which are
 instances of the @racket[editor-data%] class; decoding requires that
 each editor data object has an @deftech{editor data class}, which is
 an instance of the @racket[editor-data-class%] class.}

]

Snip classes, snip data, and snip data classes solve problems related
 to encoding and decoding snips. In an application that has no need
 for saving files or cut-and-paste, these issues can be safely
 ignored.

@subsubsection[#:tag "editorsnipclasses"]{Snip Classes}

Each snip can be associated to a @tech{snip class}. This ``class''
 is not a class description in the programmer's language; it is an
 object which provides a way to create new snips of the appropriate
 type from an encoded snip specification.

Snip class objects can be added to the eventspace-specific
 @deftech{snip class list}, which is returned by
 @racket[get-the-snip-class-list]. When a snip is encoded, the snip's
 class name is associated with the encoding; when the snip needs to be
 decoded, then the snip class list is searched by name to find the
 snip's class. The snip class will then provide a decoding function
 that can create a new snip from the encoding.

If a snip class's name is of the form
@;-
@racket["((lib ...) (lib ...))"], 
@;-
 then the snip class implementation can be loaded on
 demand. The name is parsed using @racket[read]; if the result has the
 form @racket[((lib _string ...) (lib _string ...))], then the first
 element used with @racket[dynamic-require] along with
 @racket['snip-class]. If the @racket[dynamic-require] result is a
 @racket[snip-class%] object, then it is inserted into the current
 eventspace's snip class list, and loading or saving continues using
 the new class.

The second @racket[lib] form in @racket["((lib ...) (lib ...))"]
 supplies a reader for a text-only version of the snip. See
 @secref["snipclassmapping"] for more information on how 
 such snipclasses work (and generally see the
 @racketmodname[wxme] library).

A snip class's name can also be just @racket["(lib ...)"], which is
 used like the first part of the two-@racket[lib] form. However, this
 form provides no information for the text-only @racketmodname[wxme]
 reader.

@subsubsection[#:tag "editordata"]{Editor Data}

While a snip belongs to an editor, the editor may store extra
 information about a snip in some specialized way. When the snip is to
 be encoded, this extra information needs to be put into an
 @tech{editor data} object so that the extra information can be
 encoded as well.  In a text editor, extra information can be
 associated with ranges of @techlink{item}s, as well as snips.

Just as a snip must be associated with a snip class to be decoded (see
 @|snipclassdiscuss|), an editor data object needs an @tech{editor
 data class} for decoding. Every editor data class object can be added
 to the eventspace-specific @deftech{editor data class list}, returned
 by @racket[get-the-editor-data-class-list]. Alternatively, like snip
 classes (see @secref["editorsnipclasses"]), editor data class names
 can use the form @racket["((lib ...)  (lib ...))"]  to enable
 on-demand loading. The corresponding module should export an
 @racket[editor-data-class%] object named @racket['editor-data-class].

To store and load information about a snip or region in an editor:

@itemize[

 @item{derive new classes from @racket[editor-data%] and
 @racket[editor-data-class%].}

@item{derive a new class from the @racket[text%] or
  @racket[pasteboard%] class, and override the @method[editor<%>
  get-snip-data] and @method[editor<%> set-snip-data] methods and/or the
  @method[text% get-region-data] and @method[text% set-region-data]
  methods.

  Note: the @method[text% get-region-data] and @method[text%
  set-region-data] methods are called for cut-and-paste encoding, but
  not for file-saving encoding; see @|globaleditordatadiscuss| for
  information on extending the file format.}

]


@subsection[#:tag "globaleditordata"]{Global Data: Headers and Footers}

The editor file format provides for adding extra global data in
 special header and footer sections. To save and load special header
 and/or footer records:

@itemize[

 @item{Pick a name for each header/footer record. This name should not
 conflict with any other header/footer record name in use, and no one
 else should use these names. All names beginning with ``wx'' are
 reserved for internal use. By tagging extra header and footer records
 with a unique name, the file can be safely loaded in an installation that
 does not support the records.}

 @item{Derive a new class from the @racket[text%] or
 @racket[pasteboard%] class, and override the @method[editor<%>
 write-headers-to-file], @method[editor<%> write-footers-to-file],
 @method[editor<%> read-header-from-file] and/or @method[editor<%>
 read-footer-from-file] methods.}

]

When an editor is saved, the methods @method[editor<%>
 write-headers-to-file] and @method[editor<%> write-footers-to-file]
 are invoked; at this time, the derived @racket[text%] or
 @racket[pasteboard%] object has a chance to save records.  To write a
 header/footer record, first invoke the @method[editor<%>
 begin-write-header-footer-to-file] method, at which point the record
 name is provided. Once the record is written, call @method[editor<%>
 end-write-header-footer-to-file].

When an editor is loaded and a header/footer record is encountered,
 the @method[editor<%> read-header-from-file] or @method[editor<%>
 read-footer-from-file] method is invoked, with the record name as the
 argument.  If the name matches a known record type, then the data can
 be loaded.

See also @method[editor<%> write-headers-to-file] and
 @method[editor<%> read-header-from-file].


@section[#:tag "editoreol"]{End of Line Ambiguity}

Because an editor can force a line break even when there is no
 newline item, a @techlink{position} alone does not always
 specify a @techlink{location} for the caret. Consider the last
 @techlink{position} of a line that is soft-broken (i.e., no newline
 is present): there is no @techlink{item} between the last
 @techlink{item} of the line and the first @techlink{item} of the next
 line, so two @techlink{location}s (one end-of-line and one
 start-of-line) map to the same @techlink{position}.

For this reason, @techlink{position}-setting and
 @techlink{position}-getting methods often have an extra argument. In
 the case of a @techlink{position}-setting method, the argument
 specifies whether the caret should be drawn at the left or right side
 of the page (in the event that the @techlink{location} is doubly
 defined); @racket[#t] means that the caret should be drawn on the
 right side. Similarly, methods which calculate a @techlink{position}
 from a @techlink{location} will take an extra boxed boolean; the box
 is filled with @racket[#t] if the position is ambiguous and it came
 from a right-side location, or @racket[#f] otherwise.

@section[#:tag "editorflattened"]{Flattened Text}

In plain text editors, there is a simple correlation between
 @techlink{position}s and characters. In an @racket[editor<%>] object,
 this is not true much of the time, but it is still sometimes useful
 to just ``get the text'' of an editor.

Text can be extracted from an editor in either of two forms:

@itemize[

 @item{@deftech{Simple text}, where there is one character per
 @techlink{item}. @techlink{Item}s that are characters are mapped to
 themselves, and all other @techlink{item}s are mapped to a
 period. Line breaks are represented by newline characters
 (ASCII 10).}

 @item{@deftech{Flattened text}, where each @techlink{item} can map to
 an arbitrary string.  @techlink{Item}s that are characters are still
 mapped to themselves, but more complicated @techlink{item}s can be
 represented with a useful string determined by the @techlink{item}'s
 snip. Newlines are mapped to platform-specific character sequences
 (linefeed on Unix and Mac OS X, and
 linefeed--carriage return on Windows). This form is called
 ``flattened'' because the editor's @techlink{item}s have been reduced
 to a linear sequence of characters.}

]

@section[#:tag "drawcaretinfo"]{Caret Ownership}

Within a frame, only one object can contain the keyboard focus. This
 property must be maintained when a frame contains multiple editors in
 multiple @techlink{display}s, and when a single editor contains other
 editors as @techlink{item}s.

When an editor has the keyboard focus, it will usually display the
 current selection or a line indicating the insertion point; the line
 is called the @deftech{caret}.

When an editor contains other editors, it keeps track of caret
 ownership among the contained sub-editors. When the caret is taken
 away from the main editor, it will revoke caret ownership from the
 appropriate sub-editor.

When an editor or snip is drawn, an argument to the drawing method
 specifies whether the caret should be drawn with the data or whether
 a selection spans the data. This argument can be any of:

@itemize[

 @item{@indexed-racket['no-caret] --- The caret should not be drawn at
 all.}

 @item{@indexed-racket['show-inactive-caret] --- The caret should be drawn
 as inactive; items may be identified as the local current selection,
 but the keyboard focus is elsewhere.}

 @item{@indexed-racket['show-caret] --- The caret should be drawn to show
 keyboard focus ownership.}

 @item{@racket[(cons _start _end)] --- The caret is owned by an
 enclosing region, and its selection spans the current editor or snip;
 in the case of the snip, the selection spans elements @racket[_start]
 through @racket[_end] positions within the snip.}

]

The @racket['show-inactive-caret] display mode is useful for showing
 selection ranges in text editors that do not have the focus. This
 @racket['show-inactive-caret] mode is distinct from @racket['no-caret]
 mode; when editors are embedded, only the locally active editor shows
 its selection.


@section[#:tag "editorcutandpastetime"]{Cut and Paste Time Stamps}

Methods of @racket[editor<%>] that use the clipboard --- including
 @method[editor<%> copy], @method[editor<%> cut], @method[editor<%>
 paste], and @method[editor<%> do-edit-operation] --- consume a time
 stamp argument. This time stamp is generally extracted from the
 @racket[mouse-event%] or @racket[key-event%] object that triggered
 the clipboard action. Unix uses the time stamp to synchronize clipboard
 operations among the clipboard clients.

All instances of @racket[event%] include a time stamp, which can be
 obtained using @method[event% get-time-stamp].

If the time stamp is 0, it defaults to the current time. Using 0 as the
 time stamp almost always works fine, but it is considered bad manners
 on Unix.


@section[#:tag "editorclickback"]{Clickbacks}

@deftech{Clickbacks} in a @racket[text%] editor facilitate the
 creation of simple interactive objects, such as hypertext. A
 clickback is defined by associating a callback function with a range
 of @techlink{item}s in the editor. When a user clicks on the
 @techlink{item}s in that range, the callback function is invoked. For
 example, a hypertext clickback would associate a range to a callback
 function that changes the selection range in the editor.

By default, the callback function is invoked when the user releases
 the mouse button. The @method[text% set-clickback] method accepts
 an optional argument that causes the callback function to be invoked
 on the button press, instead. This behavior is useful, for example,
 for a clickback that creates a popup menu.

Note that there is no attempt to save clickback information when a
 file is saved, since a clickback will have an arbitrary procedure
 associated with it.

@section[#:tag "lockinfo"]{Internal Editor Locks}

Instances of @racket[editor<%>] have three levels of internal
 locking:

@itemize[

 @item{write locking --- When an editor is internally locked for
 writing, the abstract content of the editor cannot be changed (e.g.,
 insertion attempts fail silently). However, snips in a text editor
 can still be split and merged, and the text editor can be changed in
 ways that affect the flow of lines. The
 @method[editor<%> locked-for-write?] method reports whether an
 editor is currently locked for writing.}

 @item{flow locking --- When a text editor is internally locked for
 reflowing, it is locked for writing, the snip content of the editor
 cannot change, the @techlink{location} of a snip cannot be computed if it
 is not already known (see
 @xmethod[editor<%> locations-computed?]), and the editor cannot
 be drawn to a @techlink{display}. A request for uncomputed location
 information during a flow lock produces undefined results. The
 @method[editor<%> locked-for-flow?] method reports whether an
 editor is currently locked for flowing.}

 @item{read locking --- When an editor is internally locked for
 reading, no operations can be performed on the editor (e.g., a
 request for the current selection position returns an undefined
 value). This extreme state is used only during callbacks to its snips
 for setting the snip's administrator, splitting the snip, or merging
 snips.  The @method[editor<%> locked-for-read?]  method reports
 whether an editor is currently locked for reading.}

]

The internal lock for an editor is @italic{not} affected by calls to
 @method[editor<%> lock].

Methods that report @techlink{location}-independent information about an
 editor never trigger a lock. A method that reports @techlink{location}
 information may trigger a flow lock or write lock if the relevant
 information has not been computed since the last modification to the
 editor (see @xmethod[editor<%> locations-computed?]). A method
 that modifies the editor in any way, even setting the selection
 position, can trigger a read lock, flow lock, or write lock.

@section[#:tag "editorthreads"]{Editors and Threads}

An editor is not tied to any particular thread or eventspace, except
 to the degree that it is displayed in a canvas (which has an
 eventspace). Concurrent access of an editor is always safe in the
 weak sense that the editor will not become corrupted. However, because
 editor access can trigger locks, concurrent access can produce 
 contract failures or unexpected results.

An editor supports certain concurrent patterns
 reliably. One relevant pattern is updating an editor in one thread
 while the editor is displayed in a canvas that is managed by a
 different (handler) thread. To ensure that canvas refreshes are not
 performed while the editor is locked for flowing, and to ensure that
 refreshes do not prevent editor modifications, the following are
 guaranteed:

@itemize[

 @item{When an editor's @method[editor<%> refresh] method is
 called during an @deftech{edit sequence} (which is started by
 @method[editor<%> begin-edit-sequence] and ended with
 @method[editor<%> end-edit-sequence]), the requested refresh
 region is recorded, but the refresh is not performed. Instead, the
 refresh is delayed until the end of the edit sequence.}

 @item{Attempting to start an edit sequence while a refresh is in
 progress blocks until the refresh is complete.}

 @item{The @method[editor<%> on-display-size-when-ready] method
 calls @method[editor<%> on-display-size] only when the editor
 is not being refreshed and only when an edit sequence is not in
 progress. In the first case, the
 @method[editor<%> on-display-size] call is delegated to the
 refreshing thread to be called after the refresh completes. In the
 second case, the @method[editor<%> on-display-size] call is
 delegated to the edit-sequence thread, to be called when the edit
 sequence is complete.}

]

Thus, disabling an @racket[editor-canvas%] object (using
 @method[window<%> enable]) is sufficient to ensure that a
 background thread can modify an editor displayed by the canvas, as
 long as all modifications are in edit sequences. The background
 modifications will impair canvas refreshes minimally and temporarily,
 and refreshes will not impair modifications in the background thread.

A second supported pattern is reading an editor in a background thread
 while the editor may be manipulated in other threads. Since no
 @techlink{location}-independent reads introduce locks, the such reads in
 the background thread will not impair other threads. However, other
 threads may interfere with the background thread, causing it to
 receive erroneous or out-of-date content information. This one-sided
 guarantee is useful if the background thread's work can be discarded
 when the editor is modified.
