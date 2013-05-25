#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi)
(provide (protect-out (all-defined-out)))

;; Win32 type and structure declarations.

(define advapi-dll (and (eq? (system-type) 'windows)
                        (ffi-lib "Advapi32.dll")))
(define kernel-dll (and (eq? (system-type) 'windows)
                        (ffi-lib "kernel32.dll")))
(define ole-dll (and (eq? (system-type) 'windows)
                     (ffi-lib "ole32.dll")))
(define oleaut-dll (and (eq? (system-type) 'windows)
                        (ffi-lib "oleaut32.dll")))

(define-ffi-definer define-advapi advapi-dll
  #:default-make-fail make-not-available)
(define-ffi-definer define-kernel kernel-dll
  #:default-make-fail make-not-available)
(define-ffi-definer define-ole ole-dll
  #:default-make-fail make-not-available)
(define-ffi-definer define-oleaut oleaut-dll
  #:default-make-fail make-not-available)

;; for functions that use the Windows stdcall ABI:
(define-syntax-rule (_wfun type ...)
  (_fun #:abi winapi type ...))

;; for functions that return HRESULTs
(define-syntax _hfun
  (syntax-rules (->)
    [(_ type ... -> who res)
     (_wfun type ...
            -> (r : _HRESULT)
            -> (if (positive? r)
                   (windows-error (format "~a: failed" 'who) r)
                   res))]))

(define (bit-and? a b)(not (zero? (bitwise-and a b))))

(define _HRESULT _ulong)

(define _LONG _long)
(define _DWORD _int32)
(define _WORD _int16)
(define _REGSAM _DWORD)
(define _BOOL (make-ctype _int (lambda (v) (if v 1 0)) (lambda (v) (not (zero? v)))))
(define _UINT _uint)
(define _ULONG _ulong)
(define _INT _int)
(define _SHORT _short)
(define _USHORT _ushort)
(define _LCID _int32)
(define _DISPID _LONG)
(define _TYPEKIND _int)
(define _VARKIND _int)
(define _MEMBERID _DISPID)
(define _HREFTYPE _DWORD)
(define _VARTYPE _ushort)
(define _SCODE _LONG)
(define _FUNCKIND _int)
(define _INVOKEKIND _int)
(define _CALLCONV _int)
(define _DATE _double)
(define _CY _llong)
(define _SIZE_T _intptr)

(define-cstruct _GUID ([l _uint]
                       [s1 _ushort]
                       [s2 _ushort]
                       [c (_array/list _byte 8)]))

(define-cstruct _TYPEDESC ([u (_union
                               _pointer ; _TYPEDESC_pointer
                               _pointer ; _ARRAYDESC-pointer
                               _HREFTYPE)]
                           [vt _VARTYPE]))

(define-cstruct _SAFEARRAYBOUND ([cElements _ULONG]
                                 [lLbound _LONG]))

(define-cstruct _ARRAYDESC ([tdescElem _TYPEDESC]
                            [cDims _USHORT]
                            [rgbounds (_array _SAFEARRAYBOUND 1)]))

(define-cstruct _TYPEATTR ([guid _GUID]
                           [lcid _LCID]
                           [dwReserved _DWORD]
                           [memidConstructor _MEMBERID]
                           [memidDestructor _MEMBERID]
                           [lpstrSchema _string/utf-16]
                           [cbSizeInstance _ULONG]
                           [typekind _TYPEKIND]
                           [cFuncs _WORD]
                           [cVars _WORD]
                           [cImplTypes _WORD]
                           [cbSizeVft _WORD]
                           [cbAlignment _WORD]
                           [wTypeFlags _WORD]
                           [wMajorVerNum _WORD]
                           [wMinorVerNum _WORD]
                           ;;[tdescAlias _TYPEDESC]
                           ;;[idldescType _IDLDESC]
                           ))

(define _VVAL (_union _double
                      _intptr
                      ;; etc.
                      (_array _pointer 2)
                      ))

(define-cstruct _VARIANT ([vt _VARTYPE]
                          [wReserved1 _WORD]
                          [wReserved2 _WORD]
                          [wReserved3 _WORD]
                          [u _VVAL]))
(define _VARIANTARG _VARIANT)
(define _VARIANTARG-pointer _VARIANT-pointer)

(define-cstruct _IDLDESC ([dwReserved _intptr]
                          [wIDLFlags _USHORT]))

(define-cstruct _PARAMDESCEX ([cBytes _ULONG]
                              [varDefaultValue _VARIANTARG]))

(define-cstruct _PARAMDESC ([pparamdescex _PARAMDESCEX-pointer]
                            [wParamFlags _USHORT]))

(define-cstruct _ELEMDESC ([tdesc _TYPEDESC]
                           [u (_union _IDLDESC
                                      _PARAMDESC)]))


(define-cstruct _FUNCDESC ([memid _MEMBERID]
                           [lprgscode _pointer]
                           [lprgelemdescParam _ELEMDESC-pointer] ; an array
                           [funckind _FUNCKIND]
                           [invkind _INVOKEKIND]
                           [callconv _CALLCONV]
                           [cParams _SHORT]
                           [cParamsOpt _SHORT]
                           [oVft _SHORT]
                           [cScodes _SHORT]
                           [elemdescFunc _ELEMDESC]
                           [wFuncFlags _WORD]))

(define-cstruct _VARDESC ([memid _MEMBERID]
                          [lpstrSchema _string/utf-16]
                          [u (_union _ULONG _VARIANT-pointer)]
                          [elemdescVar _ELEMDESC]
                          [wVarFlags _WORD]
                          [varkind _VARKIND]))

(define-cstruct _DISPPARAMS ([rgvarg _pointer] ; to _VARIANTARGs
                             [rgdispidNamedArgs _pointer] ; to _DISPIDs
                             [cArgs _UINT]
                             [cNamedArgs _UINT]))

(define-cstruct _EXCEPINFO ([wCode _WORD]
                            [wReserved _WORD]
                            [bstrSource _string/utf-16]
                            [bstrDescription _string/utf-16]
                            [bstrHelpFile _string/utf-16]
                            [dwHelpContext _DWORD]
                            [pvReserved _intptr]
                            [pfnDeferredFillIn _intptr]
                            [scode _SCODE]))

(define (windows-error str raw-scode)
  (if (zero? raw-scode)
      (error str)
      (let ()
        (define size 1024)
        (define buf (make-bytes size))
        (define scode (if (negative? raw-scode)
                          (bitwise-and #xFFFFFFFF raw-scode)
                          raw-scode))
        (define len (FormatMessageW FORMAT_MESSAGE_FROM_SYSTEM #f scode 0 buf (quotient size 2)))
        (if (positive? len)
            (error (format "~a (~x; ~a)" str scode (regexp-replace #rx"[\r\n]+$"
                                                                   (cast buf _pointer _string/utf-16)
                                                                   "")))
            (error (format "~a (~x)" str scode))))))

(define E_NOINTERFACE #x80004002)

(define-kernel FormatMessageW (_wfun _DWORD _pointer
                                     _HRESULT _DWORD
                                     _pointer _DWORD
                                     (_pointer = #f)
                                     -> _DWORD))
(define FORMAT_MESSAGE_FROM_SYSTEM #x00001000)

(define CLSCTX_INPROC_SERVER #x1)
(define CLSCTX_LOCAL_SERVER #x4)
(define CLSCTX_REMOTE_SERVER #x10)

(define LOCALE_SYSTEM_DEFAULT #x0800)

(define IMPLTYPEFLAG_FDEFAULT #x1)
(define IMPLTYPEFLAG_FSOURCE #x2)
(define IMPLTYPEFLAG_FRESTRICTED #x4)
(define IMPLTYPEFLAG_FDEFAULTVTABLE #x8)

(define TKIND_ENUM 0)
(define TKIND_RECORD 1)
(define TKIND_MODULE 2)
(define TKIND_INTERFACE 3)
(define TKIND_DISPATCH 4)
(define TKIND_COCLASS 5)
(define TKIND_ALIAS 6)
(define TKIND_UNION 7)
(define TKIND_MAX 8)

(define INVOKE_FUNC 1)
(define INVOKE_PROPERTYGET 2)
(define INVOKE_PROPERTYPUT 4)
(define INVOKE_PROPERTYPUTREF 8)
(define INVOKE_EVENT 16)

(define FUNC_VIRTUAL 0)
(define FUNC_PUREVIRTUAL 1)
(define FUNC_NONVIRTUAL 2)
(define FUNC_STATIC 3)
(define FUNC_DISPATCH 4)

(define PARAMFLAG_NONE 0)
(define PARAMFLAG_FIN #x1)
(define PARAMFLAG_FOUT #x2)
(define PARAMFLAG_FLCID #x4)
(define PARAMFLAG_FRETVAL #x8)
(define PARAMFLAG_FOPT #x10)
(define PARAMFLAG_FHASDEFAULT #x20)
(define PARAMFLAG_FHASCUSTDATA #x40)

(define VT_EMPTY 0)
(define VT_NULL 1)
(define VT_I2 2)
(define VT_I4 3)
(define VT_R4 4)
(define VT_R8 5)
(define VT_CY 6)
(define VT_DATE 7)
(define VT_BSTR 8)
(define VT_DISPATCH 9)
(define VT_ERROR 10)
(define VT_BOOL 11)
(define VT_VARIANT 12)
(define VT_UNKNOWN 13)
(define VT_DECIMAL 14)
(define VT_I1 16)
(define VT_UI1 17)
(define VT_UI2 18)
(define VT_UI4 19)
(define VT_I8 20)
(define VT_UI8 21)
(define VT_INT 22)
(define VT_UINT 23)
(define VT_VOID 24)
(define VT_HRESULT 25)
(define VT_PTR 26)
(define VT_SAFEARRAY 27)
(define VT_CARRAY 28)
(define VT_USERDEFINED 29)
(define VT_LPSTR 30)
(define VT_LPWSTR 31)
(define VT_RECORD 36)
(define VT_INT_PTR 37)
(define VT_UINT_PTR 38)
(define VT_FILETIME 64)
(define VT_BLOB 65)
(define VT_STREAM 66)
(define VT_STORAGE 67)
(define VT_STREAMED_OBJECT 68)
(define VT_STORED_OBJECT 69)
(define VT_BLOB_OBJECT 70)
(define VT_CF 71)
(define VT_CLSID 72)
(define VT_VERSIONED_STREAM 73)
(define VT_BSTR_BLOB #xfff)
(define VT_VECTOR #x1000)
(define VT_ARRAY #x2000)
(define VT_BYREF #x4000)
(define VT_RESERVED #x8000)
(define VT_ILLEGAL #xffff)
(define VT_ILLEGALMASKED #xfff)
(define VT_TYPEMASK #xfff)

(define DISPID_PROPERTYPUT -3)

(define DISP_E_PARAMNOTFOUND #x80020004)
(define DISP_E_EXCEPTION #x80020009)
(define DISP_E_UNKNOWNNAME #x80020006)
(define REGDB_E_CLASSNOTREG #x80040154)

(define-ole IIDFromString (_hfun _string/utf-16 _GUID-pointer
                                 -> IIDFromString (void))
  #:fail (lambda ()
           (lambda (s guid)
             ;; Implement the conversion manually, so that it works
             ;; on all platforms (which module-startup issues)
             (define n (string->number (regexp-replace* #rx"[-{}]" s "") 16))
             (set-GUID-l! guid (arithmetic-shift n (* -12 8)))
             (set-GUID-s1! guid (bitwise-and #xFFFF (arithmetic-shift n (* -10 8))))
             (set-GUID-s2! guid (bitwise-and #xFFFF (arithmetic-shift n (* -8 8))))
             (set-GUID-c! guid (for/list ([i (in-range 8)])
                                 (bitwise-and #xFF (arithmetic-shift n (* (- -7 i)))))))))

(define-ole StringFromIID(_hfun _GUID-pointer (p : (_ptr o _pointer))
                                -> StringFromIID p))


(define (string->guid s [stay-put? #f])
  (define guid
    (if stay-put?
        (cast (malloc _GUID 'atomic-interior) _pointer (_gcable _GUID-pointer))
        (make-GUID 0 0 0 (list 0 0 0 0 0 0 0 0))))
  (IIDFromString s guid)
  guid)

(define (guid->string guid)
  (define p (StringFromIID guid))
  (begin0
   (cast p _pointer _string/utf-16)
   (CoTaskMemFree p)))

(define (guid=? guid guid2)
  (and (= (GUID-l guid) (GUID-l guid2))
       (= (GUID-s1 guid) (GUID-s1 guid2))
       (= (GUID-s2 guid) (GUID-s2 guid2))
       (andmap = (GUID-c guid) (GUID-c guid2))))

(define-ole CoTaskMemFree (_wfun _pointer -> _void))
(define-ole CoTaskMemAlloc (_wfun _SIZE_T -> _pointer))

(define-oleaut SysFreeString (_wfun _pointer -> _void))
(define-oleaut SysAllocStringLen (_wfun _pointer _uint -> _pointer))

(define (utf-16-length s)
  (for/fold ([len 0]) ([c (in-string s)])
    (+ len
       (if ((char->integer c) . > . #xFFFF)
           2
           1))))

(define (string->pointer s)
  (let ([v (malloc _gcpointer)])
    (ptr-set! v _string/utf-16 s)
    (let ([p (ptr-ref v _gcpointer)])
      (let ([len (utf-16-length s)])
        (SysAllocStringLen p len)))))

(define _SAFEARRAY-pointer (_cpointer 'SAFEARRAY))

(define-oleaut SafeArrayCreate (_wfun _VARTYPE
                                      _UINT
                                      (dims : (_list i _SAFEARRAYBOUND))
                                      -> _SAFEARRAY-pointer))
(define-oleaut SafeArrayDestroy (_hfun _SAFEARRAY-pointer
                                       -> SafeArrayDestroy (void)))
(define-oleaut SafeArrayGetVartype (_hfun _SAFEARRAY-pointer
                                          (vt : (_ptr o _VARTYPE))
                                          -> SafeArrayGetVartype vt))
(define-oleaut SafeArrayGetLBound (_hfun _SAFEARRAY-pointer
                                         _UINT
                                         (v : (_ptr o _LONG))
                                         -> SafeArrayGetLBound v))
(define-oleaut SafeArrayGetUBound (_hfun _SAFEARRAY-pointer
                                         _UINT
                                         (v : (_ptr o _LONG))
                                         -> SafeArrayGetUBound v))
(define-oleaut SafeArrayPutElement (_hfun _SAFEARRAY-pointer
                                          (_list i _LONG)
                                          _pointer
                                          -> SafeArrayPutElement (void)))
(define-oleaut SafeArrayGetElement (_hfun _SAFEARRAY-pointer
                                          (_list i _LONG)
                                          _pointer
                                          -> SafeArrayGetElement (void)))
(define-oleaut SafeArrayGetDim (_wfun _SAFEARRAY-pointer
                                      -> _UINT))
