#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label scheme/base
                     mysterx))

@(define (com-index name what . proto)
   (index* (list (format "~a COM ~a" name what))
           (list @elem{@(tt name) COM @|what|})
           (apply tt proto)))

@title{MzCOM: Racket as a Windows COM Object}

@author["Paul Steckler"]

@exec{MzCOM.exe} is a Windows COM (i.e., Component Object Model) class
wrapper for Racket.

During normal installation of MzCOM, the executable is registered as a
COM object automatically.  If you move the Racket installation
folder, re-register @exec{MzCOM.exe} with

@commandline{mzcom.exe /RegServer}

The @exec{MzCOM.exe} executable find DLLs and Racket library
collections relative to its own path.

@; ----------------------------------------------------------------------

@section{Loading MzCOM}

To load a COM object, COM hosts require a COM class name or a ProgID.
MzCOM has the class name @tt{"MzObj Class"} and the ProgID
@tt{"MzCOM.MzObj.@nonterm{version}"}, where @nonterm{version} is
@(version).

In the Visual BASIC 6 environment, from the @menuitem["Project"
"References (VB6)"], check @onscreen{MzCOM 1.0 Type Library}.  In
Visual BASIC .NET, choose @menuitem["Project" "Add Reference"], and
from the @onscreen{COM} tab, select @onscreen{MzCOM 1.0 Type Library}.
In your code, declare a variable, then assign to it:

@verbatim[#:indent 2]{
  DIM schemeObject AS MzObj
  SET schemeObject = NEW MzObj
}

From Visual C++:

@verbatim[#:indent 2]{
 #include "mzcom.h" 
     
 CLSID clsid;
 IMzObj *pIMzObj;

 CoInitialize(NULL);
 CLSIDFromProgID(L"MzCOM.MzObj.<version>",&clsid);
 CoCreateInstance(clsid,NULL,CLSCTX_SERVER,IID_IMzObj, (void **)&pIMzObj);
}

where @tt{<version>} is the version number.  You'll need the
definition of @tt{IID_IMzObj} (see @secref["guids"]).  The header file
@filepath{mzcom.h} is generated as @filepath{src\worksp\mzcom\} when
building from the Racket source distribution. The above C/C++ code
is for illustration; your actual code should check return values, of
course.

Using @racketmodname[mysterx] to manipulate COM objects within Racket,
you can load MzCOM with either

@racketblock[
  (cci/coclass "MzObj Class")
]

or

@racketblock[
  (cci/progid "MzCOM.MzObj.<version>")
]

Consult your documentation for loading MzCOM into other COM
environments.  MzCOM is compiled as a ``dual-mode'' class, meaning its
methods may be called directly or by using OLE Automation.

@section[#:tag "guids"]{GUIDs}

When compiled from the Racket source distibrution, the directory
@filepath{src\worksp\mzcom\} contains the file @filepath{MzCOM_i.c}
that contains GUIDs for MzCOM.  Those GUIDs are as follows:

@verbatim[#:indent 2]{
  const IID IID_IMzObj = 
    {0xA604CBA8,0x2AB5,0x11D4,{0xB6,0xD3,0x00,0x60,0x08,0x90,0x02,0xFE}};

  const IID LIBID_MZCOMLib = 
    {0xA604CB9C,0x2AB5,0x11D4,{0xB6,0xD3,0x00,0x60,0x08,0x90,0x02,0xFE}};

  const IID DIID__IMzObjEvents = 
    {0xA604CBA9,0x2AB5,0x11D4,{0xB6,0xD3,0x00,0x60,0x08,0x90,0x02,0xFE}};

  const CLSID CLSID_MzObj = 
    {0xA3B0AF9E,0x2AB0,0x11D4,{0xB6,0xD2,0x00,0x60,0x08,0x90,0x02,0xFE}};
}

which represent the @tt{IMzObj} interface, the MzCOM type library, the
@tt{IMzObjEvents} interface, and the @tt{MzObj} class, respectively.


@section{Methods}

MzCOM support three COM methods:

@itemize[

 @item{@com-index["About" "method"]{void About(void)}

      Takes no arguments and displays an informational 
      dialog.}

 @item{@com-index["Eval" "method"]{BSTR Eval(BSTR input)}

      Takes and returns @tt{BSTR}s (BASIC strings).  The returned
       value is the result of evaluating the input expression,
       formatted as a string.  The input string may contain several
       S-expressions.  The embedded Racket updates its environment
       with each evaluation.  Therefore, it is possible to define
       procedures in a call to @tt{Eval}, and use the procedures in
       subsequent calls.}

 @item{@com-index["Reset" "method"]{Reset :: void Reset(void)}

       Resets the Racket environment to the initial environment.
       Also, the custodian for the primary Racket thread is invoked,
       shutting all its managed values.}
]

@section{Events}

MzCOM supports a single event.

@itemize[

 @item{@com-index["SchemeError" "event"]{SchemeError()}

        Passed a BSTR (BASIC string) that explains the error.}

]

@section{Errors}

When an error occurs in MzCOM, it creates a COM error object.  C and
C++ clients can use @tt{GetErrorInfo} to retrieve error information.
Clients implemented in other languages typically have some equivalent
means to obtain COM error information.

@section{Evaluation thread}

The Racket evaluator runs in a Win32 thread created when MzCOM is
loaded.  If an expression kills the primary Racket thread, as in

@racketblock[
  (kill-thread (current-thread))
]

then the evaluator Win32 thread is also killed.  
When that happens, subsequent calls to Eval() will fail.

@section{Acknowledgments}

MzCOM was developed in response to a query by 
Andre Van Meulebrouck.  Andre also did extensive 
testing with Visual BASIC.
