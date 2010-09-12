#lang scribble/doc
@(require "common.rkt")

@title[#:tag "multi-file"]{Multiple-File Submissions}

By default, the system is set up for submissions of single a single
file, straight fom DrRacket using the handin-client.  There is some
limited support for multi-file submissions in
@racketmodname[handin-server/checker] and in the handin-client. It is
possible to submit multiple files, and have the system generate a
single file that is the concatenation of all submission files (used
only with text files).  To set up multi-file submissions, do the
following:

@itemize[
@item{Add a @racket[:multi-file] keyword in @racket[check:], and as a
  value, use the suffix that should be used for the single
  concatenated output file.}

@item{You can also add a @racket[:names-checker] keyword--the value
  can be a regexp that all submitted files must follow (e.g.,
  @racket[".*[.]rkt$"]), or a list of expected file names.
  Alternatively, it can be a 1-argument procedure that will receive
  the (sorted) list of submitted files and can throw an error if some
  files are missing or some files are forbidden.}

@item{Make sure that @racket[:create-text?] is on if you want the
  contents of a multi-file submission to be unpacked and filenames
  checked.}

@item{In the @filepath{info.rkt} file of the handin-client you need to
  set @racket[enable-multifile-handin] to @racket[#t], and adjust
  @racket[selection-default] to patterns that are common to your
  course.  (It can be a single pattern, or a list of them.)}]

On the server side, each submission is saved in a file called
@filepath{raw}, which contains all submitted files.  In the
@filepath{grading} directory, you will get a @filepath{text.<sfx>}
file (@filepath{<sfx>} is the suffix that is used as a value for
@racket[:multi-file]) that contains all submitted files with clear
separators.  A possible confusion is that every submission is a
complete set of files that overwrites any existing submission, whereas
students may think that the server accumulates incoming files.  To
avoid such confusion, when a submission arrives an there is already an
existing previous submission, the contents is compared, and if there
are files that existed in the old submission but not in the new ones,
the student will see a warning pop-up that allows aborting the
submission.

On the client side, students will have an additional file-menu entry
for submitting multiple files, which pops up a dialog that can be used
to submit multiple files.  In this dialog, students choose their
working directory, and the @racket[selection-default] entry from the
@filepath{handin-client/info.rkt} file specifies a few patterns that
can be used to automatically select files.  The dialog provides all
handin-related functionality that is available in DrRacket.  For
further convenience, it can be used as a standalone application: in
the account management dialog, the @onscreen{Un/Install} tab has a
button that will ask for a directory where it will create an
executable for the multi-file submission utility---the resulting
executable can be used outside of DrRacket (but Racket is still
required, so it cannot be uninstalled).
