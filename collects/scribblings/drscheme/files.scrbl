#lang scribble/doc
@(require "common.ss")

@title[#:tag "drscheme-files"]{DrScheme Files}

@; ----------------------------------------

@section[#:tag "drscheme-file-formats"]{Program Files}

The standard @as-index{file extension} for a PLT Scheme program file
is @indexed-file{.ss}. The extensions @indexed-file{.scm} and
@indexed-file{.sch} are also popular.

DrScheme's editor can save a program file in two different formats:

@itemize[

 @item{@deftech{Plain-text file format} --- All text editors can read
       this format. DrScheme saves a program in plain-text format by
       default, unless the program contains images or text boxes.
       (Plain-text format does not preserve images or text boxes.)

       Plain-text format is platform-specific because different
       platforms have different newline conventions. However, most
       tools for moving files across platforms support a ``text''
       transfer mode that adjusts newlines correctly.}

 @item{@deftech{Multimedia file format} --- This format is specific to
       DrScheme, and no other editor recognizes it. DrScheme saves a
       program in multimedia format by default when the program
       contains images, text boxes, or formatted text.

       Multimedia format is platform-independent, and it uses an ASCII
       encoding (so that different ways of transferring the file are
       unlikely to corrupt the file).}

]

@; ----------------------------------------

@section[#:tag "drscheme-autosave-files"]{Backup and Autosave Files}

When you modify an existing file in DrScheme and save it, DrScheme
copies the old version of the file to a special backup file if no
backup file exists. The backup file is saved in the same directory as
the original file, and the backup file's name is generated from the
original file's name:

@itemize[

 @item{Under Unix and Mac OS X, a @filepath{~} is added to the end of
       the file's name.}

 @item{Under Windows, the file's extension is replaced with
       @filepath{.bak}.}

]

Every five minutes, DrScheme checks each open file. If any file is
modified and not saved, DrScheme saves the file to a special autosave
file (just in case there is a power failure or some other catastrophic
error). If the file is later saved, or if the user exists DrScheme
without saving the file, the autosave file is removed. The autosave
file is saved in the same directory as the original file, and the
autosave file's name is generated from the original file's name:

@itemize[

 @item{Under Unix and Mac OS X, a @filepath{#} is added to the start
       and end of the file's name, then a number is added after the
       ending @filepath{#}, and then one more @filepath{#} is appended
       after the number. The number is selected to make the autosave
       filename unique.}

 @item{Under Windows, the file's extension is replaced with a number
       to make the autosave filename unique.}

]

If the definitions window is modified and there is no current file,
then an autosave file is written to the user's ``documents''
directory. @margin-note{The ``documents'' directory is determined by
@scheme[(find-system-path 'doc-dir)].}

@; ----------------------------------------

@section{Preference Files}

On start-up, DrScheme reads configuration information from a
preferences file. The name and location of the preferences file
depends on the platform and user:

@margin-note{The expression @scheme[(find-system-path 'pref-file)]
 returns the platform- and user-specific preference file path.}

@itemize[

 @item{Under Unix, preferences are stored in a
   @indexed-file{.plt-scheme} subdirectory in the user's home
   directory, in a file @indexed-file{plt-prefs.ss}.}

 @item{Under Windows, preferences are stored in a file
  @indexed-file{plt-prefs.ss} in a sub-directory @indexed-file{PLT
  Scheme} in the user's application-data folder as specified by the
  Windows registry; the application-data folder is usually
  @indexed-file{Application Data} in the user's profile directory, and
  that directory is usually hidden in the Windows GUI.}

 @item{Under Mac OS X, preferences are stored in
  @indexed-file{org.plt-scheme.prefs.ss} in the user's preferences
  folder.}

]

A lock file is used while modifying the preferences file, and it is
created in the same directory as the preferences file. Under Windows,
the lock file is named @indexed-file{_LOCKplt-prefs.ss}; under Unix,
it is @indexed-file{.LOCK.plt-prefs.ss}; under Mac OS X, it is
@indexed-file{.LOCK.org.plt-scheme.prefs.ss}.

If the user-specific preferences file does not exist, and the file
@indexed-file{plt-prefs.ss} in the @filepath{defaults} collection does
exist, then it is used for the initial preference settings.  (See
@secref[#:doc '(lib "scribblings/reference/reference.scrbl")
"collects"] for more information about collections.) This file thus
allows site-specific configuration for preference defaults. To set up
such a configuration, start DrScheme and configure the preferences to
your liking.  Then, exit DrScheme and copy your preferences file into
the @filepath{defaults} collection as
@filepath{plt-prefs.ss}. Afterward, users who have no preference
settings already will get the preference settings that you chose.
