#lang scribble/doc
@(require "common.rkt")

@title{Auto-Updater}

The handin-client has code that can be used for automatic updating of
clients.  This can be useful for courses where you distribute some
additional functionality (collections, teachpacks, language-levels
etc), and this functionality can change (or expected to change, for
example, distributing per-homework teachpacks).

To enable this, uncomment the relevant part of the @filepath{info.rkt}
file in the client code. It has the following three keys:
@indexed-racket[enable-auto-update] that turns this facility on, and
@indexed-racket[version-filename] and
@indexed-racket[package-filename] which are the expected file names of
the version file and the @filepath{.plt} file relative to the course
web address (the value of the @racket[web-address] key).  Also,
include in your client collection a @filepath{version} file that
contains a single number that is its version. Use a big integer that
holds the time of this collection in a @tt{YYYYMMDDHHMM} format.

When students install the client, every time DrRacket starts, it will
automatically check the version from the web page (as specified by the
@racket[web-address] and @racket[version-filename] keys), and if that
contains a bigger number, it will offer the students to download and
install the new version.  So, every time you want to distribute a new
version, you build a new @filepath{.plt} file that contains a new
version file, then copy these version and @filepath{.plt} files to
your web page, and students will be notified automatically.  Note: to
get this to work, you need to create your @filepath{.plt} file using
mzc's @DFlag{replace} flag, so it will be possible to overwrite
existing files.  (Also note that there is no way to delete files when
a new @filepath{.plt} is installed.)
