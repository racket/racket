#lang scribble/doc
@(require "common.rkt")

@title[#:tag "drracket-files"]{DrRacket Files}

@; ----------------------------------------

@section[#:tag "drracket-file-formats"]{Program Files}

The standard @as-index{file extension} for a Racket program file is
@indexed-file{.rkt}. The extensions @indexed-file{.ss},
@indexed-file{.scm}, and @indexed-file{.sch} are also historically
popular.

DrRacket's editor can save a program file in two different formats:

@itemize[

 @item{@deftech{Plain-text file format} --- All text editors can read
       this format. DrRacket saves a program in plain-text format by
       default, unless the program contains images or text boxes.
       (Plain-text format does not preserve images or text boxes.)

       Plain-text format is platform-specific because different
       platforms have different newline conventions. However, most
       tools for moving files across platforms support a ``text''
       transfer mode that adjusts newlines correctly.}

 @item{@deftech{Multimedia file format} --- This format is specific to
       DrRacket, and no other editor recognizes it. DrRacket saves a
       program in multimedia format by default when the program
       contains images, text boxes, or formatted text.

       Multimedia format is platform-independent, and it uses an ASCII
       encoding (so that different ways of transferring the file are
       unlikely to corrupt the file).}

]

@; ----------------------------------------

@section[#:tag "drracket-autosave-files"]{Backup and Autosave Files}

When you modify an existing file in DrRacket and save it, DrRacket
copies the old version of the file to a special backup file if no
backup file exists. The backup file is saved in the same directory as
the original file, and the backup file's name is generated from the
original file's name:

@itemize[

 @item{On Unix and Mac OS X, a @filepath{~} is added to the end of
       the file's name.}

 @item{On Windows, the file's extension is replaced with
       @filepath{.bak}.}

]

Every five minutes, DrRacket checks each open file. If any file is
modified and not saved, DrRacket saves the file to a special autosave
file (just in case there is a power failure or some other catastrophic
error). If the file is later saved, or if the user exists DrRacket
without saving the file, the autosave file is removed. The autosave
file is saved in the same directory as the original file, and the
autosave file's name is generated from the original file's name:

@itemize[

 @item{On Unix and Mac OS X, a @filepath{#} is added to the start
       and end of the file's name, then a number is added after the
       ending @filepath{#}, and then one more @filepath{#} is appended
       after the number. The number is selected to make the autosave
       filename unique.}

 @item{On Windows, the file's extension is replaced with a number
       to make the autosave filename unique.}

]

If the definitions window is modified and there is no current file,
then an autosave file is written to the user's ``documents''
directory. @margin-note{The ``documents'' directory is determined by
@racket[(find-system-path 'doc-dir)].}

@; ----------------------------------------

@section{Preference Files}

On start-up, DrRacket reads configuration information from a
preferences file. The name and location of the preferences file
depends on the platform and user:

@margin-note{The expression @racket[(find-system-path 'pref-file)]
 returns the platform- and user-specific preference file path.}

@itemize[

 @item{On Unix, preferences are stored in a
   @indexed-file{.racket} subdirectory in the user's home
   directory, in a file @indexed-file{racket-prefs.rktd}.}

 @item{On Windows, preferences are stored in a file
  @indexed-file{racket-prefs.rktd} in a sub-directory
  @indexed-file{Racket} in the user's application-data folder as
  specified by the Windows registry; the application-data folder is
  usually @indexed-file{Application Data} in the user's profile
  directory, and that directory is usually hidden in the Windows GUI.}

 @item{On Mac OS X, preferences are stored in
  @indexed-file{org.racket-lang.prefs.rktd} in the user's preferences
  folder.}

]

A lock file is used while modifying the preferences file, and it is
created in the same directory as the preferences file. On Windows,
the lock file is named @indexed-file{_LOCKracket-prefs.rktd}; on Unix,
it is @indexed-file{.LOCK.racket-prefs.rktd}; on Mac OS X, it is
@indexed-file{.LOCK.org.racket-lang.prefs.rktd}.

If the user-specific preferences file does not exist, and the file
@indexed-file{racket-prefs.rktd} in the @filepath{defaults} collection
does exist, then it is used for the initial preference settings.  (See
@secref[#:doc '(lib "scribblings/reference/reference.scrbl")
"collects"] for more information about collections.) This file thus
allows site-specific configuration for preference defaults. To set up
such a configuration, start DrRacket and configure the preferences to
your liking.  Then, exit DrRacket and copy your preferences file into
the @filepath{defaults} collection as
@filepath{racket-prefs.rktd}. Afterward, users who have no preference
settings already will get the preference settings that you chose.
