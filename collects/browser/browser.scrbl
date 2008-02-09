#lang scribble/doc

@(require scribble/manual
	(for-label browser/browser
                   browser/htmltext
                   framework/framework))

@title{@bold{Browser}: Simple HTML Rendering}

The @schememodname[browser/browser] library provides the following
procedures and classes for parsing and viewing HTML files.  The
@schememodname[browser/htmltext] library provides a simplified
interface for rendering to a subclass of the MrEd @scheme[text%]
class. The [browser/external] library provides utilities for launching
an external browser (such as Firefox).

@section[#:tag "browser"]{Browser}

@defmodule[browser/browser]

The browser supports basic HTML commands, plus special Scheme
hyperlinks of the form @(litchar "<A MZSCHEME=sexpr>...</A>"). When
the user clicks on such a link, the string @scheme[sexpr] is parsed as
a Scheme program and evaluated. Since @scheme[sexpr] is likely to
contain Scheme strings, and since escape characters are difficult for
people to read, a @litchar{|} character in @scheme[sexpr] is
converted to a @litchar{"} character before it is parsed. Thus,

@verbatim[#<<EOS
  <A MZSCHEME="|This goes nowhere.|">Nowhere</A>
EOS
]

creates a ``Nowhere'' hyperlink, which executes the Scheme program

@schemeblock[
"This goes nowhere."
]

The value of that program is a string. When a Scheme hyperlink returns
a string, it is parsed as a new HTML document. Thus, where the use
clicks on ``Nowhere,'' the result is a new page that says ``This goes
nowhere.''

The browser also treats comment forms containing @(litchar "MZSCHEME=sexpr")
specially.  Whereas the @(litchar "<A MZSCHEME=sexpr>...</A>") form executes the
expression when the user clicks, the @(litchar "MZSCHEME") expression in a comment
is executed immediately during HTML rendering. If the result is a
string, the comment is replaced in the input HTML stream with the
content of the string. Thus,

@verbatim[#<<EOS
  <!-- MZSCHEME="(format |<B>Here</B>: ~a| (current-directory))" -->
EOS
]

inserts the path of the current working directory into the containing
document (and ``Here'' is boldfaced). If the result is a snip instead
of a string, it replaces the comment in the document. Other types of
return values are ignored.

If the html file is being accessed as a @(litchar "file:") url, the
@scheme[current-load-relative-directory] parameter is set to the
directory during the evaluation of the mzscheme code (in both
examples). The Scheme code is executed through @scheme[eval].

The @(litchar "MZSCHEME") forms are disabled unless the web page is a
@(litchar "file:") url that points into the @scheme[doc] collection.

@defproc[(open-url [url null]) null]{
	Opens the given url
 	(either a string, url record, or port) 
  	in a vanilla browser frame and returns
  	the frame. The frame is an instance of
  	@scheme[hyper-frame%].
}

@defproc[(html-img-ok [on? null undefined]) null]{
	@scheme[html-img-ok] controls image rendering for the browser.

	If @scheme[on?] is provided, sets the value of the parameter to 
	@scheme[on?], otherwise returns the current value of the parameter
}

@defproc[(html-eval-ok [on? null undefined]) null]{
	@scheme[html-eval-ok] controls the evaluation of @(litchar "MZSCHEME=")
	tags.

	If @scheme[on?] is provided, sets the value of the parameter to 
	@scheme[on?], otherwise returns the current value of the parameter
}

@defmixin[hyper-frame-mixin () ()]{
	The result of this mixin takes one argument, a url
   	string. During the initialization of objects created from
   	this mixin, the code shows the frame and visits the url.

	@defconstructor[([frame% null])]{
		Extends the given @scheme[frame%] class.
	}

	@defmethod[(get-hyper-panel%) null]{
	Returns the class that is instantiated when the frame is created.
	Must be a panel with hyper-panel-mixin mixed in. Defaults to
  	just returning @scheme[hyper-panel%].
	}

	@defmethod[(get-hyper-panel) null]{
	Returns the hyper panel in this frame.
	}
}

@defmixin[hyper-no-show-frame-mixin () ()]{
	This is the same as the @scheme[hyper-frame-mixin], except that it
  	doesn't show the frame and the initialization arguments
  	are unchanged.

	@defconstructor[([frame% null])]{
		Extends the given @scheme[frame%] class.
	}
}

@defmixin[hyper-text-mixin () ()]{
	The initialization arguments are extended with
   	four new first arguments: a url or a port to be loaded
   	into the @scheme[text%] object (using the @scheme[reload] method,
   	described below), a top-level-window or @scheme[#f] to use as a
   	parent for status dialogs, a progress procedure used as
   	for @scheme[get-url], and either @scheme[#f] or a post string to be sent
   	to a web server (technically changing the GET to a POST).

   	Sets the autowrap-bitmap to @scheme[#f].
   
   	An instance of a @scheme[(hyper-text-mixin text%)] class should be displayed
   	only in an instance of a class created with @scheme[hyper-canvas-mixin]
   	(described below).

	@defconstructor[([text% null])]{
		Extends the given @scheme[text%] class.
	}

   	The mixin adds the following methods:

	@defmethod[(map-shift-style [start null] [end null] [shift-style null]) null]{
		Maps the given style over the given range.
	}

	@defmethod[(make-link-style [start null] [end null]) null]{
		Changes the style for the given range to the link style.
	}

	@defmethod[(get-url) null]{
		Returns the URL displayed by the editor, or @scheme[#f] if there
	    is none.
	}

	@defmethod[(get-title) null]{
		Gets the page's title.
	}

	@defmethod[(set-title [string null]) null]{
		Sets the page's title.
	}

	@defmethod[(hyper-delta) null]{
		A @scheme[style-delta%] object; override it to set the link style.
	}

	@defmethod[(add-tag [name-string null] [pos null]) null]{
		Installs a tag.
	}

	@defmethod[(find-tag [name-string/number null]) null]{
		Finds the location of a tag in the buffer (where tags are
     	installed in HTML with @(litchar "<A NAME=\"name\">")) and returns its
     	position. If @scheme[name] is a number, the number is returned 
		(assumed to be an offset rather than a tag). Otherwise, 
		if the tag is not found, @scheme[#f] is returned.
	}

	@defmethod[(remove-tag [name null]) null]{
		Removes a tag.
	}

	@defmethod[(post-url [url string?] [post-data-bytes null #f]) null]{
		Follows the link in the string. 
		@scheme[post-data-bytes] defaults to @scheme[#f]
	}

	@defmethod[(add-link [start null] [end null] [url-string null]) null]{
		Installs a hyperlink.
	}

	@defmethod[(add-scheme-callback [start null] [end null] [scheme-string null]) null]{
		Installs a Scheme evaluation hyperlink.
	}

	@defmethod[(add-thunk-callback [start null] [end null] [thunk null]) null]{
		Installs a thunk-based hyperlink.
	}

	@defmethod[(eval-scheme-string [string null]) null]{
		Called to handle the @(litchar "<A MZSCHEME=\"expr\">...</A>") 
		tag and @(litchar "<! MZSCHEME=\"expr\">") comments (see above). 
		Evaluates the string; if the result is a string, 
		it is opened as an HTML page.
	}

	@defmethod[(reload) null]{
		Reloads the current page.
     
    	The text defaultly uses the basic style named "Html
	    Standard" in the editor (if it exists).	}

	@defmethod[(remap-url [url null]) null]{
		When visiting a new page, this method is called to remap
	   	the url. The remapped url is used in place of the
   		original url. If this method returns @scheme[#f], the page doesn't
	   	go anywhere.

   		This method may be killed (if the user clicks the
   		``stop'' button)
	}
	
	@defmethod[(get-hyper-keymap) null]{
		Returns a keymap suitable for frame-level handling of events to
	    redirect page-up, etc. to the browser canvas.
	}

}

@defclass[hyper-text% hyper-text-mixin #;(hyper-text-mixin text:keymap%) ()]{

	Extends the @scheme[text:keymap%] class to support standard
	key bindings in the browser window.

}

@defmixin[hyper-canvas-mixin () ()]{
	@defconstructor[([editor-canvas% null])]{
		Extends the given
 		@scheme[editor-canvas%] class. 
		The initialization arguments are unchanged.
	}

 	The canvas's parent should be an instance of a class derived with
  	@scheme[hyper-panel-mixin] (described below).

	@defmethod[(get-editor%) null]{
	Returns the class used to implement the editor in the browser
    window. It should be derived from @scheme[hyper-text%] 
	and should pass on the
   	initialization arguments to @scheme[hyper-text%].

   	The dynamic extent of the initialization of this
   	editor is called on a thread that may be killed (via a
   	custodian shutdown)

   	In that case, the editor in the browser's
   	editor-canvas may not be an instance of this class.
	}

	@defmethod[(current-page) null]{
	Returns a representation of the currently displayed page, which
   	includes a particular editor and a visible range within the
   	editor.
	}

	@defmethod[(goto-url [url null] [relative-to-url null] [progress-proc null undefined] [post-data-bytes null @scheme[#f]]) null]{
	Changes to the given url, loading it by calling the @scheme[make-editor]
   	method. If @scheme[relative-to-url] is not @scheme[#f], it must be 
	a URL for resolving @scheme[url] as a relative URL. 
	@scheme[url] may also be a port, in which case, 
	@scheme[relative-to-url] must be @scheme[#f].

   	The @scheme[progress-proc] procedure is called with a boolean at the
   	point where the URL has been resolved and enough progress has
   	been made to dismiss any message that the URL is being
   	resolved. The procedure is called with @scheme[#t] if the URL will be
   	loaded into a browser window, @scheme[#f] otherwise (e.g., the user will
   	save the URL content to a file).

   	If @scheme[post-data-bytes] is a byte string instead of false, the URL
   	GET is changed to a POST with the given data.
	}

	@defmethod[(set-page [page null] [notify? null]) null]{
	Changes to the given page. If @scheme[notify?] is not @scheme[#f], 
	the canvas's parent is notified about the change by calling its 
	@scheme[leaving-page] method.		
	}

	@defmethod[(after-set-page) null]{
	Called during @scheme[set-page]. Defaultly does nothing.
	}
}

@defmixin[hyper-panel-mixin () ()]{
	@defconstructor[([area-container% null])]{
	Extends the given area
	container class. The initialization arguments are unchanged, but
  	initialization creates controls and a hyper text canvas.  The
  	controls permit a user to move back and forth in the hypertext
  	history.
	}
	
  	The mixin adds a initialization argument, @scheme[info-line?], 
	which is a boolean indicating whether the browser should contain a 
	line to display special @(litchar "DOCNOTE") tags in a page. 
	Such tags are used primarily by the PLT documentation.

  	The mixin adds the following instance variables:
	
	@defmethod[(make-canvas [container null]) null]{
	Creates the panel's hypertext canvas, an instance of a class
   	derived using @scheme[hyper-canvas-mixin] (described above). This
   	method is called during initialization.
	}

	@defmethod[(get-canvas%) null]{
	Returns the class instantiated by make-canvas. It must be derived from
   	@scheme[hyper-canvas%].
	}

	@defmethod[(make-control-bar-panel [container null]) null]{
	Creates the panel's sub-container for the control bar containing
   	the navigation buttons. If @scheme[#f] is returned, the panel will 
	have no control bar. The default method instantiates
	@scheme[horizontal-panel%].
	}

	@defmethod[(rewind) null]{
	Goes back one page, if possible.
	}

	@defmethod[(forward) null]{
	Goes forward one page, if possible.
	}

	@defmethod[(get-canvas) null]{
	Gets the hypertext canvas.
	}

	@defmethod[(on-navigate) null]{
	Callback that is invoked any time the displayed hypertext page
   	changes (either by clicking on a link in the canvas or by
   	@scheme[rewind] or @scheme[forward] calls).
	}

	@defmethod[(leaving-page [page null] [new-page null]) null]{
	This method is called by the hypertext canvas to notify the
   	panel that the hypertext page changed. The @scheme[page] is @scheme[#f]
   	if @scheme[new-page] is the first page for the canvas. See also
   	@scheme[page->editor] (described below).
	}

	@defmethod[(filter-notes [list-of-strings null]) null]{
	Given the notes from a page as a list of strings (where
   	each string is a note), returns a single string to print
   	above the page.
	}

	@defmethod[(reload) null]{
	Reloads the currently visible page by calling the @scheme[reload] 
	method of the currently displayed hyper-text.
	}
}

@defproc[(editor->page [editor null]) null]{
	Creates a page record for the given editor,
  	suitable for use with the @scheme[set-page] method of 
	@scheme[hyper-canvas-mixin].
}

@defproc[(page->editor [page null]) null]{
	Extracts the editor from a page record.
}

@defproc[(on-installer-run [proc null undefined]) null]{
	Parameter for a procedure to be invoked
  	after the installer is run on a .plt file.
}

@defproc[(bullet-size [n null undefined]) null]{
	Parameter controlling the point size of a
  	bullet.	
}

@defmixin[image-map-snip% () ()]{
	Instances of this class behave like @scheme[image-snip%] objects,
	except they have a @(litchar "<map> ... </map>") associated with them and
	when clicking on them (in the map) they will cause their
	init arg text to follow the corresponding link.

	@defconstructor[([html-text (is-a?/c html-text<%>)])]{
	}

	@defmethod[(set-key [key-string null]) null]{
		Sets the key for the image map (eg, "#key").
	}

	@defmethod[(get-key) null]{
		Returns the current key.
	}

	@defmethod[(add-area [shape-string null] [list-of-numbers null]  [href-string null]) null]{
		Registers the shape named by the shape-string whose
  		coordinates are specified by the list-of-numbers to go to
  		the href named href-string when that region of the image
  		is clicked on.
	}
}

@section[#:tag "browser-unit"]{Browser Unit}

@defmodule[browser/browser-unit]

The _browser-unit.ss_ library in the "browser" collection is a
unitized version of the code documented above.  It imports unit
matching the following signatures:
@(itemize    
	(item @scheme[setup:plt-installer^])
    (item @scheme[mred^])
    (item @scheme[tcp^] " (see \"tcp-sig.ss\" in the \"net\" collection)")
    (item @scheme[url^] " (see \"url-sig.ss\" in the \"url\" collection)"))

It exports the @scheme[browser^] signature.

The _browser-sig.ss_ library in the ``browser'' collection defines
the @scheme[browser^] signature with all of the names listed above.


@section[#:tag "html-text"]{HTML As Text}

@defmodule[browser/htmltext]

@definterface[html-text<%> ()]{
	An interface that extends @scheme[text%] with the following methods:

	@defmethod[(get-url) null]{
	Returns a base URL used for building
	relative URLs, or @scheme[#f] if no base is available.
	}

	@defmethod[(set-title [str null]) null]{
	Registers the title @scheme[str]
  	for the rendered page.
	}

	@defmethod[(add-link [start-pos null] [end-pos null] [url-string null]) null]{
	Registers a hyperlink for the given region in rendered page.
	}

	@defmethod[(label [pos null]) null]{
	Registers a tag at the given position in the rendered page.
	}

	@defmethod[(make-link-style [start-pos null] [end-pos null]) null]{
	Modifies the style of the rendered page from @scheme[start-pos] to 
	@scheme[end-pos] to look like a hyperlink.
	}

	@defmethod*[([(add-scheme-callback [pos null] [endpos null] [code-string null]) null]
                     [(add-scheme-callback [pos null] [endpos null] [thunk null]) null])]{
	Registers a code-evaluating or thunk-invoking hyperlink for the given region.
	}

	@defmethod[(post-url [url null] [post-data-bytes null]) null]{
	Performs a post to the given @scheme[url] with the given post data.
	}
}

@defmixin[html-text-mixin () ()]{
	@defconstructor[([text%-subclassr% null])]{
	Extends the given @scheme[text%] class with implementations of the 
	@scheme[html-text<%>] methods. Hyperlinks are attached to clickbacks 
	that use @scheme[send-url]
	(from the ``sendurl.ss'' library of the ``net'' collection).
	}
}

@defproc[(render-html-to-text [input-port null] [html-text<%>-obj null]  [load-img? null]  [eval-mz? null]) null]{
	Reads HTML from @scheme[input-port] and renders it to 
	@scheme[html-text<%>-obj].  If @scheme[load-img?] is false, then images 
	are rendered as Xed-out boxes. If @scheme[eval-mz?] is false, then 
	MZSCHEME hyperlink expressions and comments are not evaluated.

	Uses the style named ``Html Standard'' in the editor's
	style-list (if it exists) for all of the inserted text's
	default style. 
}

@section[#:tag "external"]{Launching an External Browser}

@defmodule[browser/external]

@defproc[(send-url [str null] [separate-window? void #t]) null]{
	Like @scheme[send-url] in @scheme[(lib "sendurl.ss" "net")], but under Unix,
  	the user is prompted for a browser to use if none is recorded
  	in the preferences file.
}

@defproc[(browser-preference? [v null]) null]{
	Returns @scheme[#t] if @scheme[v] is a valid browser preference.
}

@defproc[(update-browser-preference [url-or-#f null]) null]{
	Under Unix, prompts the user for a browser preference and records
  	the user choice as a framework preference (even if one is already
  	recorded). If @scheme[url-or-#f] is not @scheme[#f], it is used in the 
	dialog to explain which URL is to be opened; if it is @scheme[#f], 
	the @scheme['internal] will be one of the options for the user.
}

@defproc[(install-help-browser-preference-panel) null]{
	Installs a framework preference panel for ``Browser'' options.
}

@defproc[(add-to-browser-prefs-panel [proc null]) null]{
	The @scheme[proc] must be a procedure that takes a @scheme[panel%] argument.
	It will be called when the ``Browser'' panel is constructed for
  	preferences. The supplied argument is the panel, so @scheme[proc] can add
  	additional option controls. If the panel is already created, @scheme[proc]
  	is called immediately.
}

@scheme[tool@]

  A unit that implements a DrScheme tool to add the ``Browser''
  preference panel.
