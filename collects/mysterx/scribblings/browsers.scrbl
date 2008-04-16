#lang scribble/doc
@(require "common.ss")

@title[#:tag "browsers"]{Browsers}

@defclass[mx-browser% object% ()]{

@defconstructor[([label string? "MysterX"] 
                 [width (or/c exact-nonnegative-integer? (one-of/c 'default)) 'default]
                 [height (or/c exact-nonnegative-integer? (one-of/c 'default)) 'default]
                 [x (or/c exact-integer? (one-of/c 'default)) 'default]
                 [y (or/c exact-integer? (one-of/c 'default)) 'default]
                 [style (listof (any-of/c 'iconize 'maximize 'no-system-menu
                                          'no-thick-border 'scrollbars))])]{

  Creates an instance of a MysterX browser. The @scheme[label]
  argument is a string for the document caption, with default .  The
  @scheme[width], @scheme[height], @scheme[x], and @scheme[y] give the
  size and placement of the browser window on the desktop, with
  defaults provided by Windows.  When @scheme[style-list] includes
  @scheme['scrollbars], the vertical scrollbar is disabled if
  scrolling is unnecessary, and the horizontal scrollbar disappears if
  scrolling is unnecessary.

  Although the browser window cannot be hidden initially, it can be
  iconized.  The @method[mx-browser% restore] method can be used to
  restore an iconized browser to an ordinary window.}

@defmethod[(current-document) (is-a?/c mx-document<%>)]{

 Returns the current document in the browser.}

@defmethod[(print-document) void?]{

 Prints the document displayed by the browser to the default
 printer.  As an unintentional side-effect, the browser 
 window is minimized.}

@defmethod[(show [show? any/c]) void?]{

  If @scheme[show?] is @scheme[#f], the browser window is hidden.
  Otherwise, the window is shown.}

@defmethod[(navigate [url string?]) string?]{

  Navigates the browser to the URL given by @scheme[url].
  Any DHTML changes to the page associated with the URL 
  are not shown.  Returns a string that is the actual URL 
  navigated to.}

@defmethod[(navigate/status [url string?]) 
           (list/c string? (or/c false/c integer? (one-of/c 'no-status)))]{

  Navigates the browser to the URL given by @scheme[url].
  Any DHTML changes to the page associated with the URL 
  are not shown.  Returns a list, whose first element string that 
  is the actual URL navigated to, and whose second element is
  a status code, one of: @scheme[#f], indicating no status could be 
  obtained; a number, such as @scheme[200] or @scheme[404], indicating the 
  http status; or @scheme['no-status], indicating that @scheme[url] does not 
  denote a URL with the ``http'' scheme.}

@defmethod[(go-back) string?]{

  Navigates the browser back to a URL within its history list.
  Any DHTML changes to the page associated with the URL 
  are not shown.  Returns a string that is the actual URL 
  navigated to.}

@defmethod[(go-forward) string?]{

  Navigates the browser forward to a URL within its history list.  
  Any DHTML changes to the page associated with the URL are 
  not shown.  Returns a string that is the actual URL 
  navigated to.}

@defmethod[(refresh) boolean?]{

  Refreshes the document in the browser.  Returns @scheme[#t]
  if the refresh is successful, @scheme[#f] otherwise.}

@defmethod[(iconize) void?]{

  Iconizes the browser window.}

@defmethod[(restore) void?]{

  Restores the browser window, if it has been iconized.}

@defmethod[(current-url) string?]{

  Returns a string indicating the currently displayed URL. }

@defmethod[(register-event-handler [elem (is-a?/c mx-element%)] 
                                   [f ((is-a?/c mx-event<%>) . -> . any)])
           void?]{

  Registers an event handler for the HTML element @scheme[elem].
  The result of @scheme[f] is discarded.}

@defmethod[(unregister-event-handler [elem (is-a?/c mx-element%)]) void?]{

  Unregisters an event handler for an HTML element
  in the browser.}

@defmethod[(handle-events) void?]{

  Creates a thread to handle events using the registered event
  handlers.}

@defmethod[(stop-handling-events) void?]{

  Kills the thread currently handling events for
  the browser.}

}

@; ----------------------------------------

@defproc[(block-while-browsers) void?]{
 
 Blocks until all browser windows have been closed or hidden, using
 the show method of @scheme[mx-browser%].  This is useful when a
 MysterX program file is run as a script, to prevent @exec{mzscheme}
 or @exec{mred} from closing prematurely.}


