#lang scribble/manual
@(require (for-label ds-store
                     ds-store/alias
                     racket/base
                     racket/contract/base))

@title{Reading Writing @filepath{.DS_Store} Files}

A @filepath{.DS_Store} file is a metadata file on Mac OS X that holds
information about folder and icons as viewed and manipulated in
Finder. One common reason to manipulate @filepath{.DS_Store} files
is to create a nice-looking disk image for a Mac OS X installer.

@filepath{.DS_Store} reading nd writing is based on a
reverse-engineered description of the file format @cite["DS_Store"].

@section[#:tag "ds-store-api"]{@filepath{.DS_Store} Files and Entries}

@defmodule[ds-store]

@defproc[(read-ds-store [path path-string?]
                        [#:verbose verbose? any/c #f])
         (listof ds?)]{

Reads the @filepath{.DS_Store} file at @racket[path] returning a list
of store items.}

@defproc[(write-ds-store [path path-string?]
                         [dses (listof ds?)])
         void?]{

Writes @racket[dses] to the @filepath{.DS_Store} file at
@racket[path], replacing the file's current content.}

@defstruct*[ds ([path (or/c path-element? 'same)]
                [id symbol?]
                [type (or/c 'long 'shor 'bool 'type 'ustr 'blob)]
                [data (or/c exact-integer? boolean? symbol? string?
                            bytes? iloc? fwind?)])
            #:transparent]{

Represents a entry in a @filepath{.DS_Store} file. A
@filepath{.DS_Store} file typically has multiple entries for a single
file or directory in the same directory as the @filepath{.DS_Store}.

The @racket[path] should be @racket['same] only for a volume root
directory; information about a directory is otherwise recorded in its
parent directory's @filepath{.DS_Store} file.

The @racket[id] symbols should each have four ASCII characters. See
the @filepath{.DS_Store} format description @cite["DS_Store"] for more
information @racket[id] and @racket[type] values.

The @racket[data] field long should be an exact integer for
@racket['long] and @racket['shor] types, a boolean for the
@racket['bool] type, a 4-character ASCII symbol for the @racket['type]
type, a string for the @racket['ustr] type, and either a byte string,
@racket[iloc], or @racket[fwind] for the @racket['blob] type.}

@defstruct*[iloc ([x exact-integer?] [y exact-integer?]) #:transparent]{

Represents an icon location for an @racket['Iloc] entry.}

@defstruct*[fwind ([t exact-integer?]
                   [l exact-integer?]
                   [b exact-integer?]
                   [r exact-integer?]
                   [mode symbol?]
                   [sideview? any/c])
            #:transparent]{

Represent a window location for a @racket['fwi0] entry. The
@racket[mode] field should have four ASCII characters, and recognized
@racket[mode]s include @racket['icnv], @racket['clmv], and
@racket['Nlsv].}

@; ----------------------------------------

@section[#:tag "aliases"]{Finder Aliases}

A @racket['pict] entry in a @filepath{.DS_Store} file references a
file through a Finder alias.

@defmodule[ds-store/alias]

@defproc[(path->alias-bytes [path path-string?]
                            [#:wrt wrt-dir (or/c #f path-string?) #f])
         (or/c bytes? #f)]{

Constructs a byte string to represent a Finder alias but using the
@filepath{CoreFoundation} library on Mac OS X.}

@; ----------------------------------------

@bibliography[(bib-entry #:key "DS_Store"
                         #:title "DS_Store Format"
                         #:author "Wim Lewis and Mark Mentovai"
                         #:url "http://search.cpan.org/~wiml/Mac-Finder-DSStore/DSStoreFormat.pod")]
