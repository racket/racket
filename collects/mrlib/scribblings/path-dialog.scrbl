#lang scribble/doc
@(require "common.rkt"
          (for-label mrlib/path-dialog))

@title{Path Dialog}

@defmodule[mrlib/path-dialog]

@defclass[path-dialog% dialog% ()]{

The @racket[path-dialog%] class implements a platform-independent
file/directory dialog.  The dialog is similar in functionality to the
@racket[get-file], @racket[put-file], @racket[get-directory], and
@racket[get-file-list] procedures, but considerable extra functionality
is available through the @racket[path-dialog%] class.


@defconstructor[([label (or/c label-string? false/c) #f]
                 [message (or/c label-string? false/c) #f]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                 [directory (or/c path-string? false/c) #f]
                 [filename (or/c path-string? false/c) #f]
                 [put? any/c #f]
                 [dir? any/c #f]
                 [existing? any/c (not put?)]
                 [new? any/c #f]
                 [multi? any/c #f]
                 [can-mkdir? any/c put?]
                 [filters (or/c (listof (list string? string?)) 
                                (one-of/c #f #t))
                          #t]
                 [show-file? (or/c (path? . -> . any) false/c) #f]
                 [show-dir? (or/c (path? . -> . any) false/c) #f]
                 [ok? (or/c (path? . -> . any) false/c) #f]
                 [guard (or/c (path? . -> . any) false/c) #f])]{

The @racket[label] argument is the dialog's title string. If
@racket[label] is @racket[#f], the default is based on other field
values.

The @racket[message] argument is a prompt message to show at the top
of the dialog.  If it is @racket[#f], no prompt line.

The @racket[parent] argument is the parent frame or dialog, if any,
for this dialog.

The @racket[directory] argument specifies the dialog's initial
directory.  If it is @racket[#f], the initial directory is the last
directory that was used by the user (or the current directory on first
use).

The @racket[filename] argument provides an initial filename text, if
any.

If @racket[put?] is true, the dialog operates in choose-file-to-write
mode (and warn the user if choosing an existing name).

If @racket[dir?] is true, the dialog operates in directory-choice
mode.

If @racket[existing?] is true, the use must choose an existing file.

If @racket[new?] is true, the user must choose a non-existant
path. Providing both @racket[new?] and @racket[existing?] as true
triggers an exception.

If @racket[multi?] is true, the dialog allows selection of multiple
paths.

If @racket[can-mkdir?] is true, the dialog includes a button for the
user to create a new directory.

The @racket[filters] argument is one of:

@itemize[

   @item{@racket[(list (list _filter-name _filter-glob) ...)] --- a
     list of pattern names (e.g., @racket["Scheme Files"]) and glob
     patterns (e.g., @racket["*.rkt;*.scrbl"]).  Any list, including an
     empty list, enables a filter box for the user to enter glob
     patterns, and the given list of choices is available in a
     combo-box drop-down menu.  Glob patterns are the usual Unix ones
     (see @racket[glob->regexp]), and a semicolon can be used to allow
     multiple patterns.}

   @item{@racket[#f] --- no patterns and no filter input box.}

   @item{@racket[#t] --- use a generic @racket["All"] filter, which is
     @racket["*.*"] on Windows and @racket["*"] on other
     platforms.}

]

The @racket[show-file?] predicate is used to filter file paths that
are shown in the dialog.  The predicate is applied to the file name as
a string while the current-directory parameter is set. This predicate
is intended to be a lightweight filter for choosing which names to
display.

The @racket[show-dir?] predicate is similar, but for directories
instead of files.

The @racket[ok?] predicate is used in a similar fashion to the
@racket[show-file?] and @racket[show-dir?] predicate, but it is used
to determine whether the @onscreen{OK} button should be enabled when a
file or directory is selected (so it need not be as lightweight as the
other predicates).

The @racket[guard] procedure is a generic verifier for the dialog's
final result, as produced by the @method[path-dialog% run] method.  It
receives the result that is about to be returned (which can be a list
in a multi-selection dialog), and can return a different value (any
value) instead. If it throws an exception, an error dialog is shown,
and the dialog interaction continues (so it can be used to verify
results without dismissing the dialog). This procedure can also raise
@|void-const|, in which case the dialog remains without an error
message.}


@defmethod[(run) any/c]{

Shows the dialog and returns the selected result. If a @racket[guard]
procedure is not supplied when the dialog is created, then the result
is either a path or a list of paths (and the latter only when
@racket[_multi?] is true when the dialog is created). If a
@racket[_guard] procedure is supplied, its result determines the result
of this method.}}
