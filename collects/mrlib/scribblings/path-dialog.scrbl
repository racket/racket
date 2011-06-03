#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/path-dialog))

@title{Path Dialog}

@defmodule[mrlib/path-dialog]

@defclass[path-dialog% dialog% ()]{

The @scheme[path-dialog%] class implements a platform-independent
file/directory dialog.  The dialog is similar in functionality to the
@scheme[get-file], @scheme[put-file], @scheme[get-directory], and
@scheme[get-file-list] procedures, but considerable extra functionality
is available through the @scheme[path-dialog%] class.


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

The @scheme[label] argument is the dialog's title string. If
@scheme[label] is @scheme[#f], the default is based on other field
values.

The @scheme[message] argument is a prompt message to show at the top
of the dialog.  If it is @scheme[#f], no prompt line.

The @scheme[parent] argument is the parent frame or dialog, if any,
for this dialog.

The @scheme[directory] argument specifies the dialog's initial
directory.  If it is @scheme[#f], the initial directory is the last
directory that was used by the user (or the current directory on first
use).

The @scheme[filename] argument provides an initial filename text, if
any.

If @scheme[put?] is true, the dialog operates in choose-file-to-write
mode (and warn the user if choosing an existing name).

If @scheme[dir?] is true, the dialog operates in directory-choice
mode.

If @scheme[existing?] is true, the use must choose an existing file.

If @scheme[new?] is true, the user must choose a non-existant
path. Providing both @scheme[new?] and @scheme[existing?] as true
triggers an exception.

If @scheme[multi?] is true, the dialog allows selection of multiple
paths.

If @scheme[can-mkdir?] is true, the dialog includes a button for the
user to create a new directory.

The @scheme[filters] argument is one of:

@itemize[

   @item{@scheme[(list (list _filter-name _filter-glob) ...)] --- a
     list of pattern names (e.g., @scheme["Scheme Files"]) and glob
     patterns (e.g., @scheme["*.scm;*.ss"]).  Any list, including an
     empty list, enables a filter box for the user to enter glob
     patterns, and the given list of choices is available in a
     combo-box drop-down menu.  Glob patterns are the usual Unix ones
     (see @scheme[glob->regexp]), and a semicolon can be used to allow
     multiple patterns.}

   @item{@scheme[#f] --- no patterns and no filter input box.}

   @item{@scheme[#t] --- use a generic @scheme["All"] filter, which is
     @scheme["*.*"] under Windows and @scheme["*"] on other
     platforms.}

]

The @scheme[show-file?] predicate is used to filter file paths that
are shown in the dialog.  The predicate is applied to the file name as
a string while the current-directory parameter is set. This predicate
is intended to be a lightweight filter for choosing which names to
display.

The @scheme[show-dir?] predicate is similar, but for directories
instead of files.

The @scheme[ok?] predicate is used in a similar fashion to the
@scheme[show-file?] and @scheme[show-dir?] predicate, but it is used
to determine whether the @onscreen{OK} button should be enabled when a
file or directory is selected (so it need not be as lightweight as the
other predicates).

The @scheme[guard] procedure is a generic verifier for the dialog's
final result, as produced by the @method[path-dialog% run] method.  It
receives the result that is about to be returned (which can be a list
in a multi-selection dialog), and can return a different value (any
value) instead. If it throws an exception, an error dialog is shown,
and the dialog interaction continues (so it can be used to verify
results without dismissing the dialog). This procedure can also raise
@|void-const|, in which case the dialog remains without an error
message.}


@defmethod[(run) any/c]{

Shows the dialog and returns the selected result. If a @scheme[guard]
procedure is not supplied when the dialog is created, then the result
is either a path or a list of paths (and the latter only when
@scheme[_multi?] is true when the dialog is created). If a
@scheme[_guard] procedure is supplied, its result determines the result
of this method.}}
