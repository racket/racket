#lang scribble/doc
@(require scribble/manual
          (for-label scheme
                     string-constants
                     setup/getinfo))

@title[#:tag "info.ss"]{@filepath{info.ss} File Format}

@defmodulelang[setup/infotab]

In each collection, a special module file @filepath{info.ss} provides
general information about a collection for use by various tools. For
example, an @filepath{info.ss} file specifies how to build the
documentation for a collection, and it lists plug-in tools for
DrScheme that the collection provides.

Although an @filepath{info.ss} file contains a module declaration, the
declaration has a highly constrained form. It must match the following
grammar of @scheme[_info-module]:

@schemegrammar*[
#:literals (info lib setup/infotab quote quasiquote
                 cons car cdr list list* reverse append
                 string-append path->string build-path
                 collection-path
                 system-library-subpath
                 string-constant)
[info-module (module info intotab-mod-path
               (define id info-expr)
               ...)]
[intotab-mod-path (lib "infotab.ss" "setup")
                  setup/infotab]
[info-expr (quote datum)
           (quasiquote datum)
           (info-primitive info-expr ...)
           id
           string
           number
           boolean
           (string-constant identifier)]
[info-primitive cons car cdr list
                list* reverse append
                string-append
                path->string build-path collection-path
                system-library-subpath]
]

For example, the following declaration could be the @filepath{info.ss}
library of the @filepath{help} collection. It contains definitions for
three info tags, @scheme[name], @scheme[mzscheme-launcher-libraries], and
@scheme[mzscheme-launcher-names].

@schememod[
setup/infotab
(define name "Help")
(define mzscheme-launcher-libraries '("help.ss"))
(define mzscheme-launcher-names     '("PLT Help"))
]

As illustrated in this example, an @filepath{info.ss} file can use
@hash-lang[] notation, but only with the @schememodname[setup/infotab]
language.

@;{
The @scheme[name] tag is required for @exec{setup-plt} to recognize
the collection and compile its files to bytecode. Similarly, an
@filepath{info.ss} file in a sub-directory of a collection causes the
sub-directory's files to be compiled.
;}

See also @scheme[get-info] from @schememodname[setup/getinfo].
