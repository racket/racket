#lang scribble/doc
@(require scribble/manual
          (for-label racket
                     net/sendurl))

@title{Documentation Utilities}

This section describes a bunch of utilities designed for use by DrRacket
and its tools to connect to the scribble documentation. See also
@racketmodname[scribble/xref].

@section{Searching in the Docs}

@defmodule[help/search]

@defproc[(send-main-page [#:sub sub path-string? "index.html"]
                         [#:notify notify (-> path? void)]
                         [#:fragment fragment (or/c #f ??) #f]
                         [#:query query (or/c #f ??) #f])
         any]{
  Visits the documentation file @racket[sub] in your browser. 
  
  This function builds a url that points into the main collection documentation
  or into the user-specific documentation, depending on the @racket[sub] argument.
  Once it finds the path, @racket[send-main-path] passes the path to
  @racket[notify]. The @racket[fragment] and @racket[query] arguments are passed
  to @racket[send-url/file], along with the url.
}

@defproc[(perform-search [str string?]
                         [context (or/c #f
                                        string?
                                        (list/c string? string))
                                  #f])
         void?]{
  Searches for @racket[str] in the documentation. The @racket[context] argument
  supplies a context for the search or, if it is two strings, a context for
  the search and a label for that context.
}

@section{Bug Reporting}
@defmodule[help/bug-report]

@defproc[(help-desk:report-bug [this-bug-id #f (or/c #f exact-positive-integer?)] 
                               [#:frame-mixin 
                                frame-mixin
                                (make-mixin-contract frame%)
                                values])
         void?]{
  Opens a bug report window to edit the but report identified by @racket[this-bug-id].
  If @racket[this-bug-id] is @racket[#f], then creates a new bug id and uses that one.
  
  The @racket[frame-mixin] argument is passed the frame class before creating the window.
}

@defproc[(saved-bug-report-titles/ids) (listof brinfo?)]{
  Returns a list of the saved bug reports.
}

@defproc[(discard-all-saved-bug-reports) void?]{
  Deletes all of the saved bug reports, except those currently
  open in frames.
}

@defstruct[brinfo ([title label-string?]
                   [id number?]) #:transparent]{
  A record representing a saved bug report. The @racket[id] field is suitable
  for use with @racket[help-desk:report-bug] and the @racket[label] field
  is suitable for use in a GUI control.
}
