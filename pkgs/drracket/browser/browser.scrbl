#lang scribble/doc

@(require scribble/manual
        (for-label browser
                   browser/browser-unit
                   browser/browser-sig
                   browser/htmltext
                   browser/external
                   browser/tool
                   racket/base
                   racket/contract
                   racket/class
                   racket/gui/base
                   net/url
                   framework/framework))

@(define-syntax-rule (def-ext id)
  (begin
    (require (for-label net/sendurl))
    (define id (racket send-url))))
@(def-ext net-send-url)


@title{Browser: Simple HTML Rendering}

The @racketmodname[browser] library provides the following procedures
and classes for parsing and viewing HTML files.  The
@racketmodname[browser/htmltext] library provides a simplified interface
for rendering to a subclass of the GRacket @racket[text%] class.  The
@racketmodname[browser/external] library provides utilities for
launching an external browser (such as Firefox).

@section[#:tag "browser"]{Browser}

@defmodule*/no-declare[(browser)]
@declare-exporting[browser/browser browser]

The browser supports basic HTML commands, plus special Racket hyperlinks
of the form @litchar{<A MZSCHEME=sexpr>...</A>}.  When the user clicks
on such a link, the string @racket[sexpr] is parsed as a Racket program
and evaluated.  Since @racket[sexpr] is likely to contain Racket
strings, and since escape characters are difficult for people to read, a
@litchar{|} character in @racket[sexpr] is converted to a @litchar{"}
character before it is parsed.  Thus,

@verbatim[#:indent 2]{
  <A MZSCHEME="|This goes nowhere.|">Nowhere</A>
}

creates a ``Nowhere'' hyperlink, which executes the Racket program

@racketblock[
  "This goes nowhere."
]

The value of that program is a string.  When a Racket hyperlink returns
a string, it is parsed as a new HTML document.  Thus, where the use
clicks on ``Nowhere,'' the result is a new page that says ``This goes
nowhere.''

The browser also treats comment forms containing
@litchar{MZSCHEME=sexpr} specially.   Whereas the
@litchar{<A MZSCHEME=sexpr>...</A>} form executes the expression when
the user clicks, the @litchar{MZSCHEME} expression in a comment is
executed immediately during HTML rendering.  If the result is a string,
the comment is replaced in the input HTML stream with the content of the
string.  Thus,

@verbatim[#:indent 2]{
  <!-- MZSCHEME="(format |<B>Here</B>: ~a| (current-directory))" -->
}

inserts the path of the current working directory into the containing
document (and ``Here'' is boldfaced).  If the result is a snip instead
of a string, it replaces the comment in the document.  Other types of
return values are ignored.

If the html file is being accessed as a @litchar{file:} url, the
@racket[current-load-relative-directory] parameter is set to the
directory during the evaluation of the mzscheme code (in both
examples).  The Racket code is executed through @racket[eval].

The @litchar{MZSCHEME} forms are disabled unless the web page is a
@litchar{file:} url that points into the @racket[doc] collection.

@defproc[(open-url [url (or/c url? string? input-port?)]) (is-a?/c hyper-frame%)]{
  Opens the given url in a vanilla browser frame and returns the
  frame.  The frame is an instance of @racket[hyper-frame%].
}

@defboolparam[html-img-ok ok?]{
  A parameter that determines whether the browser attempts to download
  and render images.
}

@defboolparam[html-eval-ok ok?]{
  A parameter that determines whether @litchar{MZSCHEME=} tags are
  evaluated.
}

@; ----------------------------------------------------------------------

@definterface[hyper-frame<%> ()]{
  @defmethod[(get-hyper-panel%) (subclass?/c panel%)]{
    Returns the class that is instantiated when the frame is created.
    Must be a panel with hyper-panel-mixin mixed in.  Defaults to just
    returning @racket[hyper-panel%].
  }

  @defmethod[(get-hyper-panel) (is-a?/c panel%)]{
    Returns the hyper panel in this frame.
  }
}

@; ----------------------------------------------------------------------

@defmixin[hyper-frame-mixin (frame%) (hyper-frame<%>)]{

  @defconstructor/auto-super[([url (or/c url? string? input-port?)])]{
    Shows the frame and visits @racket[url].
  }

}

@; ----------------------------------------------------------------------

@defclass[hyper-no-show-frame%
          (hyper-frame-mixin (frame:status-line-mixin frame:basic%))
          ()]

@; ----------------------------------------------------------------------

@defmixin[hyper-no-show-frame-mixin (frame%) ()]{
  The same as the @racket[hyper-frame-mixin], except that it doesn't
  show the frame and the initialization arguments are unchanged.
}

@; ----------------------------------------------------------------------

@defclass[hyper-frame%
          (hyper-no-show-frame-mixin (frame:status-line-mixin frame:basic%))
          ()]

@; ----------------------------------------------------------------------

@definterface[hyper-text<%> ()]{
  @defmethod[(url-allows-evalling? [url (or/c port? url?)]) boolean?]{
    Determines if @litchar{MZSCHEME} annotations are actually evaluated,
    for a given url.
  }
}
@; ----------------------------------------------------------------------

@defmixin[hyper-text-mixin (text%) (hyper-text<%>)]{

  An instance of a @racket[hyper-text-mixin]-extended class should be
  displayed only in an instance of a class created with
  @racket[hyper-canvas-mixin].

  @defconstructor/auto-super[([url (or/c url? string? input-port?)]
                              [status-frame
                               (or/c (is-a?/c top-level-window<%>) false/c)]
                              [post-data (or/c false/c bytes?)])]{
    The @racket[url] is loaded into the @racket[text%] object (using the
    @method[hyper-text-mixin reload] method), a top-level window for
    status messages and dialogs, a progress procedure used as for
    @racket[get-url], and either @racket[#f] or a post string to be sent
    to a web server (technically changing the GET to a POST).

    Sets the autowrap-bitmap to @racket[#f].
  }

  @defmethod[(map-shift-style [start exact-nonnegative-integer?]
                              [end exact-nonnegative-integer?]
                              [shift-style style<%>])
             void?]{
    Maps the given style over the given range.
  }

  @defmethod[(make-link-style [start exact-nonnegative-integer?]
                              [end exact-nonnegative-integer?])
             void?]{
    Changes the style for the given range to the link style.
  }

  @defmethod[(get-url) (or/c url? string? input-port? false/c)]{
    Returns the URL displayed by the editor, or @racket[#f] if there is
    none.
  }

  @defmethod[(get-title) string?]{
    Gets the page's title.
  }

  @defmethod[(set-title [str string?]) void?]{
    Sets the page's title.
  }

  @defmethod[(hyper-delta) style-delta%]{
    Override this method to set the link style.
  }

  @defmethod[(add-tag [name string?] [pos exact-nonnegative-integer?]) void?]{
    Installs a tag.
  }

  @defmethod[(find-tag [name/number (or/c string? exact-nonnegative-integer?)])
             (or/c exact-nonnegative-integer? false/c)]{
    Finds the location of a tag in the buffer (where tags are installed
    in HTML with @litchar{<A NAME="name">}) and returns its position.
    If @racket[name] is a number, the number is returned (assumed to be
    an offset rather than a tag).  Otherwise, if the tag is not found,
    @racket[#f] is returned.
  }

  @defmethod[(remove-tag [name string?]) void?]{
    Removes a tag.
  }

  @defmethod[(post-url [url (or/c string? url?)]
                       [post-data-bytes (or/c bytes? false/c) #f]) void?]{
    Follows the link, optionally with the given post data.
  }

  @defmethod[(add-link [start exact-nonnegative-integer?]
                       [end exact-nonnegative-integer?]
                       [url (or/c url? string?)])
             void?]{
    Installs a hyperlink.
  }

  @defmethod[(add-racket-callback [start exact-nonnegative-integer?]
                                  [end exact-nonnegative-integer?]
                                  [racket-expr string?])
             void?]{
    Installs a Racket evaluation hyperlink.
  }

  @defmethod[(add-thunk-callback [start exact-nonnegative-integer?]
                                 [end exact-nonnegative-integer?]
                                 [thunk (-> any)])
             void?]{
    Installs a thunk-based hyperlink.
  }

  @defmethod[(eval-racket-string [str string?]) any]{
    Called to handle the @litchar{<A MZSCHEME="expr">...</A>} tag and
    @litchar{<! MZSCHEME="expr">} comments (see above).  Evaluates the
    string; if the result is a string, it is opened as an HTML page.
  }

  @defmethod[(reload) void?]{
    Reloads the current page.

    By default, the text uses the basic style named
    @racket["Html Standard"] in the editor (if it exists).
  }

  @defmethod[(remap-url [url (or/c url? string?)]) (or/c url? string?)]{
    When visiting a new page, this method is called to remap the url.
    The remapped url is used in place of the original url.  If this
    method returns @racket[#f], the page doesn't go anywhere.

    This method may be killed (if the user clicks the ``stop'' button).
  }

  @defmethod[(get-hyper-keymap) (is-a?/c keymap%)]{
    Returns a keymap suitable for frame-level handling of events to
    redirect page-up, @|etc| to the browser canvas.
  }

}

@defclass[hyper-canvas% (hyper-canvas-mixin canvas:basic%) ()]{}


@; ----------------------------------------------------------------------

@defclass[hyper-text% (hyper-text-mixin text:keymap%) ()]{

  Extends the @racket[text:keymap%] class to support standard key
  bindings in the browser window.

}

@; ----------------------------------------------------------------------

@defmixin[hyper-canvas-mixin (editor-canvas%) ()]{

  A @racket[hyper-can-mixin]-extended canvas's parent should be an
  instance of a class derived with @racket[hyper-panel-mixin].

  @defconstructor/auto-super[()]{
  }

  @defmethod[(get-editor%) (subclass?/c text%)]{

    Returns the class used to implement the editor in the browser
    window.  It should be derived from @racket[hyper-text%] and should
    pass on the initialization arguments to @racket[hyper-text%].

    The dynamic extent of the initialization of this editor is called on
    a thread that may be killed (via a custodian shutdown).  In that
    case, the editor in the browser's editor-canvas may not be an
    instance of this class.
  }

  @defmethod[(current-page) any/c]{
    Returns a representation of the currently displayed page, which
    includes a particular editor and a visible range within the editor.
  }

  @defmethod[(goto-url [url (or/c url? string?)]
                       [relative-to-url (or/c url? string? false/c)]
                       [progress-proc (boolean? . -> . any) void]
                       [post-data (or/c bytes? false/c) #f])
             void?]{
    Changes to the given url, loading it by calling the
    @racket[make-editor] method.  If @racket[relative-to-url] is not
    @racket[#f], it must be a URL for resolving @racket[url] as a
    relative URL.  @racket[url] may also be a port, in which case,
    @racket[relative-to-url] must be @racket[#f].

    The @racket[progress-proc] procedure is called with a boolean at the
    point where the URL has been resolved and enough progress has been
    made to dismiss any message that the URL is being resolved.  The
    procedure is called with @racket[#t] if the URL will be loaded into
    a browser window, @racket[#f] otherwise (e.g., the user will save
    the URL content to a file).

    If @racket[post-data-bytes] is a byte string instead of false, the
    URL GET is changed to a POST with the given data.
  }

  @defmethod[(set-page [page any/c] [notify? any/c]) void?]{
    Changes to the given page.  If @racket[notify?] is not @racket[#f],
    the canvas's parent is notified about the change by calling its
    @racket[leaving-page] method.
  }

  @defmethod[(after-set-page) void?]{
    Called during @racket[set-page].  Does nothing by default.
  }
}

@; ----------------------------------------------------------------------

@definterface[hyper-panel<%> ()]{
}

@; ----------------------------------------------------------------------

@defmixin[hyper-panel-mixin (area-container<%>) (hyper-panel<%>)]{

  @defconstructor/auto-super[([info-line? any/c])]{
    Creates controls and a hyper text canvas.  The controls permit a
    user to move back and forth in the hypertext history.

    The @racket[info-line?] argument indicates whether the browser
    should contain a line to display special @litchar{DOCNOTE} tags in a
    page.  Such tags are used primarily by the PLT documentation.
  }

  @defmethod[(make-canvas [container (is-a?/c area-container<%>)]) void?]{
    Creates the panel's hypertext canvas, an instance of a class derived
    using @racket[hyper-canvas-mixin].  This method is called during
    initialization.
  }

  @defmethod[(get-canvas%) (subclass?/c editor-canvas%)]{
    Returns the class instantiated by make-canvas.  It must be derived
    from @racket[hyper-canvas-mixin].
  }

  @defmethod[(make-control-bar-panel [container (is-a?/c area-container<%>)])
             any/c]{
    Creates the panel's sub-container for the control bar containing the
    navigation buttons.  If @racket[#f] is returned, the panel will have
    no control bar.  The default method instantiates
    @racket[horizontal-panel%].
  }

  @defmethod[(rewind) void?]{
    Goes back one page, if possible.
  }

  @defmethod[(forward) void?]{
    Goes forward one page, if possible.
  }

  @defmethod[(get-canvas) (is-a?/c editor-canvas%)]{
    Gets the hypertext canvas.
  }

  @defmethod[(on-navigate) void?]{
    Callback that is invoked any time the displayed hypertext page
    changes (either by clicking on a link in the canvas or by
    @racket[rewind] or @racket[forward] calls).
  }

  @defmethod[(leaving-page [page any/c] [new-page any/c])
             any]{
    This method is called by the hypertext canvas to notify the panel
    that the hypertext page changed.  The @racket[page] is @racket[#f]
    if @racket[new-page] is the first page for the canvas.  See also
    @racket[page->editor].
  }

  @defmethod[(filter-notes [notes (listof string?)])
             (listof string?)]{
    Given the notes from a page as a list of strings (where each string
    is a note), returns a single string to print above the page.
  }

  @defmethod[(reload) void?]{
    Reloads the currently visible page by calling the @racket[reload]
    method of the currently displayed hyper-text.
  }
}

@; ----------------------------------------------------------------------

@defclass[hyper-panel% (hyper-panel-mixin vertical-panel%) ()]

@; ----------------------------------------------------------------------

@defproc[(editor->page [editor (is-a?/c text%)]) any/c]{
  Creates a page record for the given editor, suitable for use with the
  @racket[set-page] method of @racket[hyper-canvas-mixin].
}

@defproc[(page->editor [page any/c]) (is-a?/c text%)]{
  Extracts the editor from a page record.
}

@defparam[bullet-size n exact-nonnegative-integer?]{
  Parameter controlling the point size of a bullet.
}

@defclass[image-map-snip% snip% ()]{
  Instances of this class behave like @racket[image-snip%] objects,
  except they have a @litchar{<map> ... </map>} associated with them and
  when clicking on them (in the map) they will cause their init arg text
  to follow the corresponding link.

  @defconstructor[([html-text (is-a?/c html-text<%>)])]{
  }

  @defmethod[(set-key [key string?]) void?]{
    Sets the key for the image map (eg, @racket["#key"]).
  }

  @defmethod[(get-key) string?]{
    Returns the current key.
  }

  @defmethod[(add-area [shape string?]
                       [region (listof number?)]
                       [href string?])
             void?]{
    Registers the shape named by @racket[shape] whose coordinates are
    specified by @racket[region] to go to @racket[href] when that region
    of the image is clicked on.
  }
}

@defstruct[(exn:cancelled exn) ()]{
  This exception may be raised by the
  @method[hyper-text-mixin reload] method.
}

@defstruct[(exn:file-saved-instead exn) ([pathname path-string?])]{
  This exception may be raised by the
  @method[hyper-text-mixin reload] method.
}

@; ----------------------------------------------------------------------

@section[#:tag "browser-unit"]{Browser Unit}

@defmodule[browser/browser-unit]

@defthing[browser@ unit?]{
  Imports @racket[mred^], @racket[tcp^], and @racket[url^], and exports
  @racket[browser^].
}

@; ----------------------------------------------------------------------

@section[#:tag "browser-sig"]{Browser Signature}

@defmodule[browser/browser-sig]

@defsignature[browser^ ()]{
  Includes all of the bindings of the @racketmodname[browser] library.
}

@; ----------------------------------------------------------------------

@section[#:tag "html-text"]{HTML As Text Editor}

@defmodule[browser/htmltext]

@definterface[html-text<%> (text%)]{

  @defmethod[(get-url) (or/c url? string? false/c)]{
    Returns a base URL used for building relative URLs, or @racket[#f]
    if no base is available.
  }

  @defmethod[(set-title [str string?]) void?]{
    Registers the title @racket[str] for the rendered page.
  }

  @defmethod[(add-link [start exact-nonnegative-integer?]
                       [end exact-nonnegative-integer?]
                       [url (or/c url? string?)])
             void?]{
    Registers a hyperlink for the given region in rendered page.
  }

  @defmethod[(add-tag [name string?] [pos exact-nonnegative-integer?]) void?]{
    Installs a tag.
  }

  @defmethod[(make-link-style [start exact-nonnegative-integer?]
                              [end exact-nonnegative-integer?])
             void?]{
    Changes the style for the given range to the link style.
  }

  @defmethod[(add-racket-callback [start exact-nonnegative-integer?]
                                  [end exact-nonnegative-integer?]
                                  [racket-expr string?])
             void?]{
    Installs a Racket evaluation hyperlink.
  }

  @defmethod[(add-thunk-callback [start exact-nonnegative-integer?]
                                 [end exact-nonnegative-integer?]
                                 [thunk (-> any)])
             void?]{
    Installs a thunk-based hyperlink.
  }

  @defmethod[(post-url [url (or/c string? url?)]
                       [post-data-bytes (or/c bytes? false/c) #f]) void?]{
    Follows the link, optionally with the given post data.
  }
}


@defmixin[html-text-mixin (text%) ()]{
  Extends the given @racket[text%] class with implementations of the
  @racket[html-text<%>] methods.  Hyperlinks are attached to clickbacks
  that use @net-send-url from @racketmodname[net/sendurl].
}

@defproc[(render-html-to-text [in input-port?]
                              [dest (is-a? html-text<%>)]
                              [load-img? any/c]
                              [eval-mz? any/c])
         void?]{
  Reads HTML from @racket[in] and renders it to @racket[dest].  If
  @racket[load-img?] is @racket[#f], then images are rendered as Xed-out
  boxes.  If @racket[eval-mz?] is @racket[#f], then @litchar{MZSCHEME}
  hyperlink expressions and comments are not evaluated.

  Uses the style named @racket["Html Standard"] in the editor's
  style-list (if it exists) for all of the inserted text's default
  style.
}

@; ----------------------------------------------------------------------

@section[#:tag "external"]{Launching an External Browser}

@defmodule[browser/external]

@defproc[(send-url [str null] [separate-window? void #t]) null]{
  Like @net-send-url from @racket[net/sendurl], but on Unix, the user
  is prompted for a browser to use if none is recorded in the
  preferences file.
}

@defproc[(browser-preference? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a valid browser preference.
}

@defproc[(update-browser-preference [url (or/c string? false/c)]) void?]{
  On Unix, prompts the user for a browser preference and records the
  user choice as a framework preference (even if one is already
  recorded).  If @racket[url] is not @racket[#f], it is used in the
  dialog to explain which URL is to be opened; if it is @racket[#f], the
  @racket['internal] will be one of the options for the user.
}

@defproc[(install-help-browser-preference-panel) void?]{
  Installs a framework preference panel for ``Browser'' options.
}

@defproc[(add-to-browser-prefs-panel [proc ((is-a?/c panel%) . -> . any)])
         void?]{
  The @racket[proc] is called when the ``Browser'' panel is constructed
  for preferences.  The supplied argument is the panel, so @racket[proc]
  can add additional option controls.  If the panel is already created,
  @racket[proc] is called immediately.
}

@; ----------------------------------------------------------------------

@section[#:tag "tool"]{DrRacket Browser Preference Panel}

@defmodule[browser/tool]

@defthing[tool@ unit?]{
  A unit that implements a DrRacket tool to add the ``Browser''
  preference panel.
}
