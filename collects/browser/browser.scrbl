#lang scribble/doc

@(require scribble/manual
        (for-label browser
                   browser/browser-unit
                   browser/browser-sig
                   browser/htmltext
                   browser/external
                   browser/tool
                   scheme/base
                   scheme/contract
                   scheme/class
                   scheme/gui/base
                   net/url
                   framework/framework))

@(define-syntax-rule (def-ext id)
  (begin
   (require (for-label net/sendurl))
   (define id (scheme send-url))))
@(def-ext net-send-url)


@title{@bold{Browser}: Simple HTML Rendering}

The @schememodname[browser] library provides the following procedures
and classes for parsing and viewing HTML files.  The
@schememodname[browser/htmltext] library provides a simplified interface
for rendering to a subclass of the MrEd @scheme[text%] class.  The
@schememodname[browser/external] library provides utilities for
launching an external browser (such as Firefox).

@section[#:tag "browser"]{Browser}

@defmodule[browser]

The browser supports basic HTML commands, plus special Scheme hyperlinks
of the form @litchar{<A MZSCHEME=sexpr>...</A>}.  When the user clicks
on such a link, the string @scheme[sexpr] is parsed as a Scheme program
and evaluated.  Since @scheme[sexpr] is likely to contain Scheme
strings, and since escape characters are difficult for people to read, a
@litchar{|} character in @scheme[sexpr] is converted to a @litchar{"}
character before it is parsed.  Thus,

@verbatim[#:indent 2]{
  <A MZSCHEME="|This goes nowhere.|">Nowhere</A>
}

creates a ``Nowhere'' hyperlink, which executes the Scheme program

@schemeblock[
  "This goes nowhere."
]

The value of that program is a string.  When a Scheme hyperlink returns
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
@scheme[current-load-relative-directory] parameter is set to the
directory during the evaluation of the mzscheme code (in both
examples).  The Scheme code is executed through @scheme[eval].

The @litchar{MZSCHEME} forms are disabled unless the web page is a
@litchar{file:} url that points into the @scheme[doc] collection.

@defproc[(open-url [url (or/c url? string? input-port?)]) (is-a?/c hyper-frame%)]{
  Opens the given url in a vanilla browser frame and returns the
  frame.  The frame is an instance of @scheme[hyper-frame%].
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

@defmixin[hyper-frame-mixin (frame%) ()]{

  @defconstructor/auto-super[([url (or/c url? string? input-port?)])]{
    Shows the frame and visits @scheme[url].
  }

  @defmethod[(get-hyper-panel%) (subclass?/c panel%)]{
    Returns the class that is instantiated when the frame is created.
    Must be a panel with hyper-panel-mixin mixed in.  Defaults to just
    returning @scheme[hyper-panel%].
  }

  @defmethod[(get-hyper-panel) (is-a?/c panel%)]{
    Returns the hyper panel in this frame.
  }
}

@; ----------------------------------------------------------------------

@defclass[hyper-no-show-frame%
          (hyper-frame-mixin (frame:status-line-mixin frame:basic%))
          ()]

@; ----------------------------------------------------------------------

@defmixin[hyper-no-show-frame-mixin (frame%) ()]{
  The same as the @scheme[hyper-frame-mixin], except that it doesn't
  show the frame and the initialization arguments are unchanged.
}

@; ----------------------------------------------------------------------

@defclass[hyper-frame%
          (hyper-no-show-frame-mixin (frame:status-line-mixin frame:basic%))
          ()]

@; ----------------------------------------------------------------------

@defmixin[hyper-text-mixin (text%) ()]{

  An instance of a @scheme[hyper-text-mixin]-extended class should be
  displayed only in an instance of a class created with
  @scheme[hyper-canvas-mixin].

  @defconstructor/auto-super[([url (or/c url? string? input-port?)]
                              [status-frame
                               (or/c (is-a?/c top-level-window<%>) false/c)]
                              [post-data (or/c false/c bytes?)])]{
    The @scheme[url] is loaded into the @scheme[text%] object (using the
    @method[hyper-text-mixin reload] method), a top-level window for
    status messages and dialogs, a progress procedure used as for
    @scheme[get-url], and either @scheme[#f] or a post string to be sent
    to a web server (technically changing the GET to a POST).

    Sets the autowrap-bitmap to @scheme[#f].
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
    Returns the URL displayed by the editor, or @scheme[#f] if there is
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
    If @scheme[name] is a number, the number is returned (assumed to be
    an offset rather than a tag).  Otherwise, if the tag is not found,
    @scheme[#f] is returned.
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

  @defmethod[(add-scheme-callback [start exact-nonnegative-integer?]
                                  [end exact-nonnegative-integer?]
                                  [scheme-expr string?])
             void?]{
    Installs a Scheme evaluation hyperlink.
  }

  @defmethod[(add-thunk-callback [start exact-nonnegative-integer?]
                                 [end exact-nonnegative-integer?]
                                 [thunk (-> any)])
             void?]{
    Installs a thunk-based hyperlink.
  }

  @defmethod[(eval-scheme-string [str string?]) any]{
    Called to handle the @litchar{<A MZSCHEME="expr">...</A>} tag and
    @litchar{<! MZSCHEME="expr">} comments (see above).  Evaluates the
    string; if the result is a string, it is opened as an HTML page.
  }

  @defmethod[(reload) void?]{
    Reloads the current page.

    The text defaultly uses the basic style named
    @scheme["Html Standard"] in the editor (if it exists).
  }

  @defmethod[(remap-url [url (or/c url? string?)]) (or/c url? string?)]{
    When visiting a new page, this method is called to remap the url.
    The remapped url is used in place of the original url.  If this
    method returns @scheme[#f], the page doesn't go anywhere.

    This method may be killed (if the user clicks the ``stop'' button).
  }

  @defmethod[(get-hyper-keymap) (is-a?/c keymap%)]{
    Returns a keymap suitable for frame-level handling of events to
    redirect page-up, @|etc| to the browser canvas.
  }

}

@; ----------------------------------------------------------------------

@defclass[hyper-text% (hyper-text-mixin text:keymap%) ()]{

  Extends the @scheme[text:keymap%] class to support standard key
  bindings in the browser window.

}

@; ----------------------------------------------------------------------

@defmixin[hyper-canvas-mixin (editor-canvas%) ()]{

  A @scheme[hyper-can-mixin]-extended canvas's parent should be an
  instance of a class derived with @scheme[hyper-panel-mixin].

  @defconstructor/auto-super[()]{
  }

  @defmethod[(get-editor%) (subclass?/c text%)]{

    Returns the class used to implement the editor in the browser
    window.  It should be derived from @scheme[hyper-text%] and should
    pass on the initialization arguments to @scheme[hyper-text%].

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
    @scheme[make-editor] method.  If @scheme[relative-to-url] is not
    @scheme[#f], it must be a URL for resolving @scheme[url] as a
    relative URL.  @scheme[url] may also be a port, in which case,
    @scheme[relative-to-url] must be @scheme[#f].

    The @scheme[progress-proc] procedure is called with a boolean at the
    point where the URL has been resolved and enough progress has been
    made to dismiss any message that the URL is being resolved.  The
    procedure is called with @scheme[#t] if the URL will be loaded into
    a browser window, @scheme[#f] otherwise (e.g., the user will save
    the URL content to a file).

    If @scheme[post-data-bytes] is a byte string instead of false, the
    URL GET is changed to a POST with the given data.
  }

  @defmethod[(set-page [page any/c] [notify? any/c]) void?]{
    Changes to the given page.  If @scheme[notify?] is not @scheme[#f],
    the canvas's parent is notified about the change by calling its
    @scheme[leaving-page] method.
  }

  @defmethod[(after-set-page) void?]{
    Called during @scheme[set-page].  Defaultly does nothing.
  }
}

@; ----------------------------------------------------------------------

@defmixin[hyper-panel-mixin (area-container<%>) ()]{

  @defconstructor/auto-super[([info-line? any/c])]{
    Creates controls and a hyper text canvas.  The controls permit a
    user to move back and forth in the hypertext history.

    The @scheme[info-line?] argument indicates whether the browser
    should contain a line to display special @litchar{DOCNOTE} tags in a
    page.  Such tags are used primarily by the PLT documentation.
  }

  @defmethod[(make-canvas [container (is-a?/c area-container<%>)]) void?]{
    Creates the panel's hypertext canvas, an instance of a class derived
    using @scheme[hyper-canvas-mixin].  This method is called during
    initialization.
  }

  @defmethod[(get-canvas%) (subclass?/c editor-canvas%)]{
    Returns the class instantiated by make-canvas.  It must be derived
    from @scheme[hyper-canvas-mixin].
  }

  @defmethod[(make-control-bar-panel [container (is-a?/c area-container<%>)])
             any/c]{
    Creates the panel's sub-container for the control bar containing the
    navigation buttons.  If @scheme[#f] is returned, the panel will have
    no control bar.  The default method instantiates
    @scheme[horizontal-panel%].
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
    @scheme[rewind] or @scheme[forward] calls).
  }

  @defmethod[(leaving-page [page any/c] [new-page any/c])
             any]{
    This method is called by the hypertext canvas to notify the panel
    that the hypertext page changed.  The @scheme[page] is @scheme[#f]
    if @scheme[new-page] is the first page for the canvas.  See also
    @scheme[page->editor].
  }

  @defmethod[(filter-notes [notes (listof string?)])
             (listof string?)]{
    Given the notes from a page as a list of strings (where each string
    is a note), returns a single string to print above the page.
  }

  @defmethod[(reload) void?]{
    Reloads the currently visible page by calling the @scheme[reload]
    method of the currently displayed hyper-text.
  }
}

@; ----------------------------------------------------------------------

@defclass[hyper-panel% (hyper-panel-mixin vertical-panel%) ()]

@; ----------------------------------------------------------------------

@defproc[(editor->page [editor (is-a?/c text%)]) any/c]{
  Creates a page record for the given editor, suitable for use with the
  @scheme[set-page] method of @scheme[hyper-canvas-mixin].
}

@defproc[(page->editor [page any/c]) (is-a?/c text%)]{
  Extracts the editor from a page record.
}

@defparam[bullet-size n exact-nonnegative-integer?]{
  Parameter controlling the point size of a bullet.
}

@defclass[image-map-snip% snip% ()]{
  Instances of this class behave like @scheme[image-snip%] objects,
  except they have a @litchar{<map> ... </map>} associated with them and
  when clicking on them (in the map) they will cause their init arg text
  to follow the corresponding link.

  @defconstructor[([html-text (is-a?/c html-text<%>)])]{
  }

  @defmethod[(set-key [key string?]) void?]{
    Sets the key for the image map (eg, @scheme["#key"]).
  }

  @defmethod[(get-key) string?]{
    Returns the current key.
  }

  @defmethod[(add-area [shape string?]
                       [region (listof number?)]
                       [href string?])
             void?]{
    Registers the shape named by @scheme[shape] whose coordinates are
    specified by @scheme[region] to go to @scheme[href] when that region
    of the image is clicked on.
  }
}

@; ----------------------------------------------------------------------

@section[#:tag "browser-unit"]{Browser Unit}

@defmodule[browser/browser-unit]

@defthing[browser@ unit?]{
  Imports @scheme[mred^], @scheme[tcp^], and @scheme[url^], and exports
  @scheme[browser^].
}

@; ----------------------------------------------------------------------

@section[#:tag "browser-sig"]{Browser Signature}

@defmodule[browser/browser-sig]

@defsignature[browser^ ()]{
  Includes all of the bindings of the @schememodname[browser] library.
}

@; ----------------------------------------------------------------------

@section[#:tag "html-text"]{HTML As Text Editor}

@defmodule[browser/htmltext]

@definterface[html-text<%> (text%)]{

  @defmethod[(get-url) (or/c url? string? false/c)]{
    Returns a base URL used for building relative URLs, or @scheme[#f]
    if no base is available.
  }

  @defmethod[(set-title [str string?]) void?]{
    Registers the title @scheme[str] for the rendered page.
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

  @defmethod[(add-scheme-callback [start exact-nonnegative-integer?]
                                  [end exact-nonnegative-integer?]
                                  [scheme-expr string?])
             void?]{
    Installs a Scheme evaluation hyperlink.
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
  Extends the given @scheme[text%] class with implementations of the
  @scheme[html-text<%>] methods.  Hyperlinks are attached to clickbacks
  that use @net-send-url from @schememodname[net/sendurl].
}

@defproc[(render-html-to-text [in input-port?]
                              [dest (is-a? html-text<%>)]
                              [load-img? any/c]
                              [eval-mz? any/c])
         void?]{
  Reads HTML from @scheme[in] and renders it to @scheme[dest].  If
  @scheme[load-img?] is @scheme[#f], then images are rendered as Xed-out
  boxes.  If @scheme[eval-mz?] is @scheme[#f], then @litchar{MZSCHEME}
  hyperlink expressions and comments are not evaluated.

  Uses the style named @scheme["Html Standard"] in the editor's
  style-list (if it exists) for all of the inserted text's default
  style.
}

@; ----------------------------------------------------------------------

@section[#:tag "external"]{Launching an External Browser}

@defmodule[browser/external]

@defproc[(send-url [str null] [separate-window? void #t]) null]{
  Like @net-send-url from @scheme[net/sendurl], but under Unix, the user
  is prompted for a browser to use if none is recorded in the
  preferences file.
}

@defproc[(browser-preference? [v any/c]) boolean?]{
  Returns @scheme[#t] if @scheme[v] is a valid browser preference.
}

@defproc[(update-browser-preference [url (or/c string? false/c)]) void?]{
  Under Unix, prompts the user for a browser preference and records the
  user choice as a framework preference (even if one is already
  recorded).  If @scheme[url] is not @scheme[#f], it is used in the
  dialog to explain which URL is to be opened; if it is @scheme[#f], the
  @scheme['internal] will be one of the options for the user.
}

@defproc[(install-help-browser-preference-panel) void?]{
  Installs a framework preference panel for ``Browser'' options.
}

@defproc[(add-to-browser-prefs-panel [proc ((is-a?/c panel%) . -> . any)])
         void?]{
  The @scheme[proc] is called when the ``Browser'' panel is constructed
  for preferences.  The supplied argument is the panel, so @scheme[proc]
  can add additional option controls.  If the panel is already created,
  @scheme[proc] is called immediately.
}

@; ----------------------------------------------------------------------

@section[#:tag "tool"]{DrScheme Browser Preference Panel}

@defmodule[browser/tool]

@defthing[tool@ unit?]{
  A unit that implements a DrScheme tool to add the ``Browser''
  preference panel.
}
