#lang scribble/doc
@(require scribble/manual 
          "common.rkt" 
          (for-label racket/base
                     setup/pack
                     racket/contract/base))

@title[#:tag "plt"]{@exec{raco pack}: Packing Library Collections}

The @exec{raco pack} command creates an archive of files and
directories. Formerly, such archives were used directly to distribute
library files to Racket users, but the package manager (see
@other-manual['(lib "pkg/scribblings/pkg.scrbl")]) is now the
preferred mechanism for distribution.

A packed archive usually has the suffix @as-index{@filepath{.plt}}.
The @exec{raco pkg} command recognizes a @filepath{.plt} archive for
installation as a package. The @exec{raco setup} command (see
@secref["setup"]) also supports @filepath{.plt} unpacking and
installation when using the @Flag{A} flag, but such installations do
not benefit from the more general management facilities of @exec{raco
pkg}, while the @exec{raco unpack} command (see @secref["unpack"])
unpacks an archive locally without attempting to install it. DrRacket
recognizes the @filepath{.plt} and currently treats such an archive in
the same way as @exec{raco setup -A}.

An archive contains the following elements:

@itemize[

 @item{A set of files and directories to be unpacked, and flags
 indicating whether they are to be unpacked relative to the Racket
 add-ons directory (which is user-specific), the Racket installation
 directory, or a user-selected directory.

 The files and directories for an archive are provided on the command
 line to @exec{raco pack}, either directly or in the form of
 collection names when the @DFlag{collect} flag is used.

 The @as-index{@DFlag{at-plt}} flag indicates that the files and
 directories should be unpacked relative to the user's add-ons
 directory, unless the user specifies the Racket installation
 directory when unpacking. The @as-index{@DFlag{collection-plt}} flag
 implies @DFlag{at-plt}.  The @as-index{@DFlag{all-users}} flag
 overrides @DFlag{at-plt}, and it indicates that the files and
 directories should be unpacked relative to the Racket
 installation directory, always.}

 @item{A flag for each file indicating whether it overwrites an
 existing file when the archive is unpacked; the default is to leave
 the old file in place, but the @as-index{@DFlag{replace}} flag
 enables replacing for all files in the archive.}

 @item{A list of collections to be set-up (via @exec{raco setup})
 after the archive is unpacked; the @as-index{@DPFlag{setup}} flag
 adds a collection name to the archive's list, but each collection for
 @DFlag{collection-plt} is added automatically.}

 @item{A name for the archive, which is reported to the user by the
 unpacking interface; the @as-index{@DFlag{plt-name}} flag sets the
 archive's name, but a default name is determined automatically when
 using @DFlag{collect}.}

 @item{A list of required collections (with associated version
 numbers) and a list of conflicting collections; the @exec{raco pack}
 command always names the @filepath{racket} collection in the required
 list (using the collection's pack-time version), @exec{raco pack}
 names each packed collection in the conflict list (so that a
 collection is not unpacked on top of a different version of the same
 collection), and @exec{raco pack} extracts other requirements and
 conflicts from the @filepath{info.rkt} files of collections when
 using @DFlag{collect}.}

]

Specify individual directories and files for the archive when not
using @DFlag{collect}. Each file and directory must be specified with
a relative path. By default, if the archive is unpacked with DrRacket,
the user will be prompted for a target directory, and if @exec{raco
setup} is used to unpack the archive, the files and directories will
be unpacked relative to the current directory. If the @DFlag{at-plt}
flag is provided, the files and directories will be unpacked relative
to the user's Racket add-ons directory, instead. Finally, if the
@DFlag{all-users} flag is provided, the files and directories will be
unpacked relative to the Racket installation directory, instead.

Use the @DFlag{collect} flag to pack one or more collections;
sub-collections can be designated by using a @litchar{/} as a path
separator on all platforms. In this mode, @exec{raco pack}
automatically uses paths relative to the Racket installation or
add-ons directory for the archived files, and the collections will be
set-up after unpacking. In addition, @exec{raco pack} consults each
collection's @filepath{info.rkt} file, as described below, to
determine the set of required and conflicting collections. Finally,
@exec{raco pack} consults the first collection's @filepath{info.rkt}
file to obtain a default name for the archive.  For example, the
following command creates a @filepath{sirmail.plt} archive for
distributing a @filepath{sirmail} collection:

@commandline{raco pack --collect sirmail.plt sirmail}

When packing collections, @exec{raco pack} checks the following fields
of each collection's @filepath{info.rkt} file (see @secref["info.rkt"]):

@itemize[

 @item{@racket[requires] --- A list of the form @racket[(list (list
 _coll _vers) ...)] where each @racket[_coll] is a non-empty list of
 relative-path strings, and each @racket[_vers] is a (possibly empty)
 list of exact integers. The indicated collections must be installed
 at unpacking time, with version sequences that match as much of the
 version sequence specified in the corresponding @racket[vers].

 A collection's version is indicated by a @racket[version] field in
 its @filepath{info.rkt} file, and the default version is the empty list.
 The version sequence generalized major and minor version numbers. For
 example, version @racket['(2 5 4 7)] of a collection can be used when
 any of @racket['()], @racket['(2)], @racket['(2 5)], @racket['(2 5
 4)], or @racket['(2 5 4 7)] is required.}

 @item{@racket[conflicts] --- A list of the form @racket[(list _coll
 ...)] where each @racket[_coll] is a non-empty list of relative-path
 strings. The indicated collections must @emph{not} be installed at
 unpacking time.}

]

For example, the @filepath{info.rkt} file in the @filepath{sirmail} collection
might contain the following @racket[info] declaration:

@racketmod[
info
(define name "SirMail")
(define mred-launcher-libraries (list "sirmail.rkt"))
(define mred-launcher-names (list "SirMail"))
(define requires (list (list "mred")))
]

Then, the @filepath{sirmail.plt} file (created by the command-line
example above) will contain the name ``SirMail.'' When the archive is
unpacked, the unpacker will check that the @filepath{mred} collection
is installed, and that @filepath{mred} has the same version as when
@filepath{sirmail.plt} was created.

@; ------------------------------------------------------------------------

@section[#:tag "format-of-.plt-archives"]{Format of @filepath{.plt} Archives}

The extension @filepath{.plt} is not required for a distribution
archive, but the @filepath{.plt}-extension convention helps users
identify the purpose of a distribution file.

The raw format of a distribution file is described below. This format
is uncompressed and sensitive to communication modes (text
vs. binary), so the distribution format is derived from the raw format
by first compressing the file using @exec{gzip}, then encoding the gzipped
file with the MIME base64 standard (which relies only the characters
@litchar{A}-@litchar{Z}, @litchar{a}-@litchar{z}, @litchar{0}-@litchar{9}, 
@litchar{+}, @litchar{/}, and @litchar{=}; all other characters are ignored
when a base64-encoded file is decoded).

The raw format is

@itemize[
  @item{
    @litchar{PLT} are the first three characters.}

  @item{
    An S-expression matching

    @racketblock[
                            (lambda (request failure)
                              (case request
                                [(name) _name]
                                [(unpacker) (@#,racket[quote] mzscheme)]
                                [(requires) (@#,racket[quote] _requires)]
                                [(conflicts) (@#,racket[quote] _conflicts)]
                                [(plt-relative?) _plt-relative?]
                                [(plt-home-relative?) _plt-home-relative?]
                                [(test-plt-dirs) _test-dirs]
                                [else (failure)]))
     ]

    where the @racket[_name], @racket[_requires], @|etc|, meta-variables
    stand for S-expressions as follows:
    
    @itemize[
      @item{
        @racket[_name] --- a human-readable string describing the archive's
        contents. This name is used only for printing messages to the
        user during unpacking.}

      @item{
        @racket[_requires] --- a list of collections required to be installed before
        unpacking the archive, which associated versions; see the
        documentation of @racket[pack] for details.}

     @item{
        @racket[_conflicts] --- a list of collections required @emph{not} to be installed
        before unpacking the archive.}

     @item{
        @racket[_plt-relative?] --- a boolean; if true, then the archive's
        content should be unpacked relative to the plt add-ons directory.}

     @item{
        @racket[_plt-home-relative?] --- a boolean; if true and if
        @racket['plt-relative?] is true, then the archive's content should be
        unpacked relative to the Racket installation.}

     @item{
        @racket[_test-plt-dirs] --- @racket[#f] or a @racket['_paths] where 
        @racket[_paths] is a list of path strings;
        in the latter case, a true value of @racket[_plt-home-relative?] is
        cancelled if any of the directories in the list (relative to the
        Racket installation) is unwritable by the user.}
   ]

    The S-expression is extracted from the archive
    using @racket[read] (and the result is @emph{not}
    @racket[eval]uated).}

 @item{
   An S-expression matching

                @racketblock[
                         (unit (import main-collects-parent-dir mzuntar) 
                               (export)
                               (mzuntar void)
                               (@#,racket[quote] _collections))
                 ]

    where @racket[_collections] is a list of collection paths
    (where each collection path is a list of strings); once the archive
    is unpacked, @exec{raco setup} will compile and setup the specified
    collections.

     The S-expression is extracted from the archive
     using @racket[read] (and the result is @emph{not}
     @racket[eval]uated).}

]

The archive continues with @tech{unpackables}. @tech{Unpackables} are
extracted until the end-of-file is found (as indicated by an
@litchar{=} in the base64-encoded input archive).

An @deftech{unpackable} is one of the following:

@itemize[
   @item{
     The symbol @racket['dir] followed by a list S-expression. The @racket[build-path]
     procedure will be applied to the list to obtain a relative path for
     the directory (and the relative path is combined with the target
     directory path to get a complete path).

     The @racket['dir] symbol and list are extracted from the archive
     using @racket[read] (and the result is @emph{not}
     @racket[eval]uated).}

   @item{
     The symbol @racket['file], a list, a number, an asterisk, and the file
     data. The list specifies the file's relative path, just as for
     directories. The number indicates the size of the file to be
     unpacked in bytes. The asterisk indicates the start of the file
     data; the next n bytes are written to the file, where n is the
     specified size of the file.

     The symbol, list, and number are all extracted from the archive
     using @racket[read] (and the result is @emph{not}
     @racket[eval]uated). After the number is read, input characters
     are discarded until an asterisk is found. The file data must
     follow this asterisk immediately.}
   
   @item{
     The symbol @racket['file-replace] is treated like @racket['file], 
     but if the file exists on disk already, the file in the archive replaces
     the file on disk.}
]

@; ----------------------------------------

@section{API for Packing}

@defmodule[setup/pack]{Although the @exec{raco pack} command can be
used to create most @filepath{.plt} files, the
@racketmodname[setup/pack] library provides a more general API for
making @filepath{.plt} archives.}

@defproc[(pack-collections-plt
          (dest path-string?)
          (name string?)
          (collections (listof (listof path-string?)))
          [#:replace? replace? boolean? #f]
          [#:at-plt-home? at-home? boolean? #f]
          [#:test-plt-collects? test? boolean? #t]
          [#:extra-setup-collections collection-list (listof path-string?) null] 
          [#:file-filter filter-proc (path-string? . -> . boolean?) std-filter]) void?]{

  Creates the @filepath{.plt} file specified by the pathname @racket[dest],
  using the @racket[name] as the name reported to @exec{raco setup}
  as the archive's description.

  The archive contains the collections listed in @racket[collections], which
  should be a list of collection paths; each collection path is, in
  turn, a list of relative-path strings.

  If the @racket[#:replace?] argument is @racket[#f], then attempting to
  unpack the archive will report an error when any of the collections exist
  already, otherwise unpacking the archive will overwrite an existing
  collection.

  If the @racket[#:at-plt-home?] argument is @racket[#t], then the archived
  collections will be installed into the Racket installation directory
  instead of the user's directory if the main @filepath{collects} directory
  is writable by the user. If the @racket[#:test-plt-collects?] argument is
  @racket[#f] (the default is @racket[#t]) and the @racket[#:at-plt-home?] argument
  is @racket[#t], then installation fails if the main @filepath{collects}
  directory is not writable.

  The optional @racket[#:extra-setup-collections] argument is a list of
  collection paths that are not included in the archive, but are
  set-up when the archive is unpacked.

  The optional @racket[#:file-filter] argument is the same as for
  @racket[pack-plt].}

@defproc[(pack-collections
          (dest path-string?)
          (name string?)
          (collections (listof (listof path-string?)))
          (replace? boolean?)
          (extra-setup-collections (listof path-string?))
          [filter (path-string? . -> . boolean?) std-filter]
          [at-plt-home? boolean? #f]) void?]{
  Old, keywordless variant of @racket[pack-collections-plt] for backward compatibility.}

@defproc[(pack-plt
            (dest path-string?)
            (name string?)
            (paths (listof path-string?))
            [#:as-paths as-paths (listof path-string?) paths]
            [#:file-filter filter-proc
                           (path-string? . -> . boolean?) std-filter]
            [#:encode? encode? boolean? #t]
            [#:file-mode file-mode-sym symbol? 'file]
            [#:unpack-unit unpack-spec any/c #f]
            [#:collections collection-list (listof path-string?) null]
            [#:plt-relative? plt-relative? any/c #f]
            [#:at-plt-home? at-plt-home? any/c #f]
            [#:test-plt-dirs dirs (or/c (listof path-string?) false/c) #f]
            [#:requires mod-and-version-list
                        (listof (listof path-string?)
                                (listof exact-integer?))
                        null]
            [#:conflicts mod-list
                         (listof (listof path-string?)) null])
         void?]{

  Creates the @filepath{.plt} file specified by the pathname @racket[dest],
  using the string @racket[name] as the name reported to @exec{raco setup} as
  the archive's description. The @racket[paths] argument must be a list of
  relative paths for directories and files; the contents of these files and
  directories will be packed into the archive. The optional @racket[as-paths]
  list provides the path to be recorded in the archive for each element of 
  @racket[paths] (so that the unpacked paths can be different from the packed
  paths).

  The @racket[#:file-filter] procedure is called with the relative path of each
  candidate for packing. If it returns @racket[#f] for some path, then that
  file or directory is omitted from the archive. If it returns @racket['file]
  or @racket['file-replace] for a file, the file is packed with that mode,
  rather than the default mode. The default is @racket[std-filter].
  
  If the @racket[#:encode?] argument is @racket[#f], then the output archive
  is in raw form, and still must be gzipped and mime-encoded (in that
  order). The default value is @racket[#t].

  The @racket[#:file-mode] argument must be @racket['file] or
  @racket['file-replace], indicating the default mode for a file in the
  archive. The default is @racket['file].

  The @racket[#:unpack-unit] argument is usually
  @racket[#f]. Otherwise, it must be an S-expression for the
  S-expression that describes unpacking; see
  @secref["format-of-.plt-archives"] more information about the
  unit. If the @racket[#:unpack-unit] argument is @racket[#f], an
  appropriate S-expression is generated.

  The @racket[#:collections] argument is a list of collection paths to be
  compiled after the archive is unpacked. The default is the @racket[null].

  If the @racket[#:plt-relative?] argument is true (the default is
  @racket[#f]), the archive's files and directories are to be unpacked
  relative to the user's add-ons directory or the Racket installation
  directories, depending on whether the @racket[#:at-plt-home?]
  argument is true and whether directories specified by
  @racket[#:test-plt-dirs] are writable by the user.

  If the @racket[#:at-plt-home?] argument is true (the default is
  @racket[#f]), then @racket[#:plt-relative?] must be true, and the
  archive is unpacked relative to the Racket installation directory. In
  that case, a relative path that starts with @filepath{collects} is
  mapped to the installation's main @filepath{collects} directory, and
  so on, for the following the initial directory names:

  @itemize[
     @item{@filepath{collects}}
     @item{@filepath{doc}}
     @item{@filepath{lib}}
     @item{@filepath{include}}
   ]

  If @racket[#:test-plt-dirs] is a @racket[list], then
  @racket[#:at-plt-home?] must be @racket[#t]. In that case, when the archive
  is unpacked, if any of the relative directories in the
  @racket[#:test-plt-dirs] list is unwritable by the current user, then the
  archive is unpacked in the user's add-ons directory after all.

  The @racket[#:requires] argument should have the shape @racket[(list
      (list _coll-path _version) _...)]  where each
      @racket[_coll-path] is a non-empty list of relative-path
      strings, and each @@racket[_version] is a (possibly empty) list
      of exact integers. The indicated collections must be installed
      at unpacking time, with version sequences that match as much of
      the version sequence specified in the corresponding
      @@racket[_version]. A collection's version is indicated by the
      @racketidfont{version} field of its @filepath{info.rkt} file.

  The @racket[#:conflicts] argument should have the shape
       @racket[(list _coll-path _...)]  where each @racket[_coll-path]
       is a non-empty list of relative-path strings. The indicated
       collections must @emph{not} be installed at unpacking time.}

@defproc[(pack
          (dest path-string?)
          (name string?)
          (paths (listof path-string?))
          (collections (listof path-string?))
          [filter (path-string? . -> . boolean?) std-filter]
          [encode? boolean? #t]
          [file-mode symbol? 'file]
          [unpack-unit any/c #f]
          [plt-relative? boolean? #t]
          [requires (listof (listof path-string?)
                            (listof exact-integer?)) null]
          [conflicts (listof (listof path-string?)) null]
          [at-plt-home? boolean? #f]) void?]{
  Old, keywordless variant of @racket[pack-plt] for backward compatibility.}

@defproc[(std-filter (p path-string?)) boolean?]{
  Returns @racket[#t] unless @racket[p], after stripping its directory
  path and converting to a byte string, matches one of the following
  regular expressions: @litchar{^[.]git}, @litchar{^[.]svn$},
  @litchar{^CVS$}, @litchar{^[.]cvsignore}, @litchar{^compiled$},
  @litchar{^doc}, @litchar{~$}, @litchar{^#.*#$}, @litchar{^[.]#}, or
  @litchar{[.]plt$}.}

@defproc[(mztar (path path-string?)
                [#:as-path as-path path-string? path]
                (output output-port?)
                (filter (path-string? . -> . boolean?))
                (file-mode (symbols 'file 'file-replace))) void?]{
   Called by @racket[pack] to write one directory/file @racket[path] to the
   output port @racket[output] using the filter procedure @racket[filter]
   (see @racket[pack] for a description of @racket[filter]). The @racket[path]
   is recorded in the output as @racket[as-path], in case the unpacked
   path should be different from the original path. The
   @racket[file-mode] argument specifies the default mode for packing a file,
   either @racket['file] or @racket['file-replace].}
