#lang scribble/doc
@(require "common.rkt")

@title[#:tag "overview"]{Overview}

@section{Installation}

MysterX requires Internet Explorer (IE) 4 or later to be installed.
Distributed COM (DCOM) for your version of Windows is also required.
Recent versions of Windows come with DCOM; DCOM packages for Windows
95 and 98 are made available separately.

Two Windows DLLs support low-level operations in MysterX:
@filepath{myspage.dll} and @filepath{myssink.dll}.  Both are installed
in the registry (using @exec{regsvr32.exe}) when @exec{raco setup} runs the
MysterX post-installer. If you move the location of your Racket
installation, you may need to re-run @exec{raco setup} to make MysterX
work. Neither of these DLLs is specific to a Racket version, so
it's ok for one version of Racket to use the DLLs registered by
another.

@margin-note{Prior to version 369.4, @filepath{myssink.dll} was
version-specific. Its GUID was changed when it was made
version-independent.}

If you build a stand-alone executable that uses MysterX, you need to
specifically include @filepath{myspage.dll} and @filepath{myssink.dll}
with your distribution, and the DLLs will need to be registered on the
end user's machine. One way to do that is to include the following
little setup program (as an executable) in your distribution:

@racketblock[
  (module setup scheme/base
    (require mzlib/runtime-path
             mzlib/process)

    (code:comment @#,t{Ensure that DLLs are included with the distribution:})
    (define-runtime-path myspage-dll '(so "myspage"))
    (define-runtime-path myssink-dll '(so "myssink"))

    (code:comment @#,t{Register the DLLs:})
    (define regsvr32 
      (path->string (find-executable-path "regsvr32.exe" #f)))
    (system* regsvr32 (path->string myspage-dll))
    (system* regsvr32 (path->string myssink-dll)))
]

@; ----------------------------------------------------------------------

@section{Running a Demo}

Try 

@racketblock[
  (require mysterx/mxdemo)
]

The demo requires the MSCal Calendar control.  The calendar control is
normally installed with Microsoft Office, but it can also be
downloaded from elsewhere; look for @filepath{mscal.ocx}.

@; ----------------------------------------------------------------------

@section{Loading}

Load the MysterX module with 

@racketblock[
  (require mysterx)
]

Because some MysterX code relies on the @racketmodname[scheme/class]
class system, you may also need

@racketblock[
  (require mzlib/class)
]

Several MysterX procedures take HTML strings as input.  The
@racketmodname[xml] library provides procedures that convert Racket
syntax into XML strings.  You may find using these procedures useful
in creating HTML strings for use by MysterX.

@; ----------------------------------------------------------------------

@include-section["dcom.scrbl"]
