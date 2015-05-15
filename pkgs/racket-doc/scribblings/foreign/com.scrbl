#lang scribble/doc
@(require scribble/manual
          "com-common.rkt"
          (for-label racket/base
                     ffi/unsafe/com))

@title[#:style 'toc #:tag "com"]{COM (Common Object Model)}

The @racketmodname[ffi/com] and @racketmodname[ffi/unsafe/com]
libraries support COM interaction in two layers. The safe upper layer
provides functions for creating COM objects and dynamically
constructing method calls based on COM automation (i.e., reflective
information provided by the object). The unsafe lower layer provides a
syntactic form and functions for working more directly with COM
objects and interfaces.

A @deftech{COM object} instantiates a particular @deftech{COM
class}. A @tech{COM class} can be specified in either of two ways:

@itemlist[

 @item{A @deftech{CLSID} (class id), which is represented as a
 @tech{GUID}. A @deftech{GUID} (globally unique identifier) is a
 16-byte structure. GUIDs are typically written in string forms such
 as @racket["{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}"]. The
 @racket[string->guid] and @racket[guid->string] convert between
 string and @tech{GUID} forms. The @racket[string->clsid] function is
 the same as @racket[string->guid], but its use suggests that the
 resulting @tech{GUID} is to be used as a @tech{CLSID}.}

 @item{A @deftech{ProgID} is a human-readable name, such as
 @racket["MzCom.MzObj.5.2.0.7"], which includes a version number. The
 version number can be omitted in a @tech{ProgID}, in which case the
 most recent available version is used. The operating system provides
 a mapping between @tech{ProgIDs} and @tech{CLSIDs} that is available
 via @racket[progid->clsid] and @racket[clsid->progid].}

]

A @tech{COM object} can be instantiated either on the local machine or
on a remote machine. The latter relies on the operating system's
@deftech{DCOM} (distributed COM) support.

Each @tech{COM object} supports some number of @deftech{COM
interfaces}. A @tech{COM interface} has a programmatic name, such as
@cpp{IDispatch}, that corresponds to a C-layer protocol. Each
interface also has an @deftech{IID} (interface id) that is represented
as a @tech{GUID} such as
@racket["{00020400-0000-0000-C000-000000000046}"]. Direct calls to COM
methods require extracting a suitable interface pointer from an object
using @racket[QueryInterface] and the desired @tech{IID}; the result
is effectively cast it to a pointer to a dispatch-table pointer, where
the dispatch table has a statically known size and foreign-function
content. The @racket[define-com-interface] form simplifies description
and use of interface pointers. The COM automation layer uses a fixed
number of reflection interfaces internally, notably @cpp{IDispatch},
to call methods by name and with safe argument marshaling.

@local-table-of-contents[]

@include-section["com-auto.scrbl"]
@include-section["com-intf.scrbl"]
@include-section["active-x.scrbl"]
