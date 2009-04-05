#lang scribble/doc
@(require scribble/manual
          "common.ss"
          (for-label scheme/base))

@title[#:tag "plt"]{Packaging Library Collections}

@margin-note{Before creating a @filepath{.plt} archive to distribute,
consider instead posting your package on
@link["http://planet.plt-scheme.org/"]{@|PLaneT|}.}

The command-line flags @DFlag{plt} and @DFlag{collection-plt} direct
@|mzc| to create an archive for distributing library files to PLT Scheme
users. A distribution archive usually has the suffix
@as-index{@filepath{.plt}}, which DrScheme recognizes as an archive to
provide automatic unpacking facilities. The @exec{setup-plt} program
also supports @filepath{.plt} unpacking.

An archive contains the following elements:

@itemize[

 @item{A set of files and directories to be unpacked, and flags
 indicating whether they are to be unpacked relative to the PLT Scheme
 add-ons directory (which is user-specific), the PLT Scheme installation
 directory, or a user-selected directory.

 The files and directories for an archive are provided on the command
 line to @|mzc|, either directly with @DFlag{plt} or in the form of
 collection names with @DFlag{collection-plt}.

 The @as-index{@DFlag{at-plt}} flag indicates that the files and
 directories should be unpacked relative to the user's add-ons
 directory, unless the user specifies the PLT Scheme installation
 directory when unpacking. The @as-index{@DFlag{collection-plt}} flag
 implies @DFlag{at-plt}.  The @as-index{@DFlag{all-users}} flag
 overrides @DFlag{at-plt}, and it indicates that the files and
 directories should be unpacked relative to the PLT Scheme
 installation directory, always.}

 @item{A flag for each file indicating whether it overwrites an
 existing file when the archive is unpacked; the default is to leave
 the old file in place, but @|mzc|'s @as-index{@DFlag{replace}} flag
 enables replacing for all files in the archive.}

 @item{A list of collections to be set-up (via Setup PLT) after the
 archive is unpacked; @|mzc|'s @as-index{@DPFlag{setup}} flag adds a
 collection name to the archive's list, but each collection for
 @DFlag{collection-plt} is added automatically.}

 @item{A name for the archive, which is reported to the user by the
 unpacking interface; @|mzc|'s @as-index{@DFlag{plt-name}} flag sets
 the archive's name, but a default name is determined automatically
 for @DFlag{collection-plt}.}

 @item{A list of required collections (with associated version
 numbers) and a list of conflicting collections; @|mzc| always names
 the @filepath{mzscheme} collection in the required list (using the
 collection's pack-time version), @|mzc| names each packed collection
 in the conflict list (so that a collection is not unpacked on top of
 a different version of the same collection), and @|mzc| extracts
 other requirements and conflicts from the @filepath{info.ss} files of
 collections for @DFlag{collection-plt}.}

]

Use the @DFlag{plt} flag to specify individual directories and files
for the archive. Each file and directory must be specified with a
relative path. By default, if the archive is unpacked with DrScheme,
the user will be prompted for a target directory, and if
@exec{setup-plt} is used to unpack the archive, the files and
directories will be unpacked relative to the current directory. If the
@DFlag{at-plt} flag is provided to @|mzc|, the files and directories
will be unpacked relative to the user's PLT Scheme add-ons directory,
instead. Finally, if the @DFlag{all-users} flag is provided to @|mzc|,
the files and directories will be unpacked relative to the PLT Scheme
installation directory, instead.

Use the @DFlag{collection-plt} flag to pack one or more collections;
sub-collections can be designated by using a @litchar{/} as a path
separator on all platforms. In this mode, @|mzc| automatically uses
paths relative to the PLT Scheme installation or add-ons directory for
the archived files, and the collections will be set-up after
unpacking. In addition, @|mzc| consults each collection's
@filepath{info.ss} file, as described below, to determine the set of
required and conflicting collections. Finally, @|mzc| consults the
first collection's @filepath{info.ss} file to obtain a default name
for the archive.  For example, the following command creates a
@filepath{sirmail.plt} archive for distributing a @filepath{sirmail}
collection:

@commandline{mzc --collection-plt sirmail.plt sirmail}

When packing collections, @|mzc| checks the following fields of each
collection's @filepath{info.ss} file (see @secref["info.ss" #:doc
'(lib "scribblings/setup-plt/setup-plt.scrbl")]):

@itemize[

 @item{@scheme[requires] --- A list of the form @scheme[(list (list
 _coll _vers) ...)] where each @scheme[_coll] is a non-empty list of
 relative-path strings, and each @scheme[_vers] is a (possibly empty)
 list of exact integers. The indicated collections must be installed
 at unpacking time, with version sequences that match as much of the
 version sequence specified in the corresponding @scheme[vers].

 A collection's version is indicated by a @scheme[version] field in
 it's @filepath{info.ss} file, and the default version is the empty list.
 The version sequence generalized major and minor version numbers. For
 example, version @scheme['(2 5 4 7)] of a collection can be used when
 any of @scheme['()], @scheme['(2)], @scheme['(2 5)], @scheme['(2 5
 4)], or @scheme['(2 5 4 7)] is required.}

 @item{@scheme[conflicts] --- A list of the form @scheme[(list _coll
 ...)] where each @scheme[_coll] is a non-empty list of relative-path
 strings. The indicated collections must @emph{not} be installed at
 unpacking time.}

]

For example, the @filepath{info.ss} file in the @filepath{sirmail} collection
might contain the following @scheme[info] declaration:

@schememod[
setup/infotab
(define name "SirMail")
(define mred-launcher-libraries (list "sirmail.ss"))
(define mred-launcher-names (list "SirMail"))
(define requires (list (list "mred")))
]

Then, the @filepath{sirmail.plt} file (created by the command-line
example above) will contain the name ``SirMail.'' When the archive is
unpacked, the unpacker will check that the MrEd collection is
installed (not just MzScheme), and that MrEd has the same version as
when @filepath{sirmail.plt} was created.

@; ----------------------------------------

@section{Scheme API for Packaging}

Although @|mzc|'s command-line interface is sufficient for most
purposes, see the @schememodname[setup/pack] library for a more
general interface for constructing archives.


