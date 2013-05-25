#lang racket/base
(require ffi/unsafe
         ffi/unsafe/com)

;; --------------------------------------------------
;; Example from the documentation.
;; This test file is designed to load on all platforms, but interesting
;; tests run ony under Windows.

; The function that uses COM interfaces defined further below:
 
(define (show-all-classes)
  (define ccm
    (com-create-instance CLSID_StdComponentCategoriesMgr))
  (define icat (QueryInterface (com-object-get-iunknown ccm)
                               IID_ICatInformation
                               _ICatInformation-pointer))
  (define eci (EnumCategories icat LOCALE_SYSTEM_DEFAULT))
  (for ([catinfo (in-producer (lambda () (Next/ci eci)) #f)])
    (printf "~a:\n"
            (cast (array-ptr (CATEGORYINFO-szDescription catinfo))
                  _pointer
                  _string/utf-16))
    (define eg
      (EnumClassesOfCategories icat (CATEGORYINFO-catid catinfo)))
    (for ([guid (in-producer (lambda () (Next/g eg)) #f)])
      (printf " ~a\n" (or (clsid->progid guid)
                          (guid->string guid))))
    (Release eg))
  (Release eci)
  (Release icat))
 
; The class to instantiate:
 
(define CLSID_StdComponentCategoriesMgr
  (string->clsid "{0002E005-0000-0000-C000-000000000046}"))
 
; Some types and variants to match the specification:
 
(define _ULONG _ulong)
(define _CATID _GUID)
(define _REFCATID _GUID-pointer)
(define-cstruct _CATEGORYINFO ([catid _CATID]
                               [lcid _LCID]
                               [szDescription (_array _short 128)]))
 
; —— IEnumGUID ——-
 
(define IID_IEnumGUID
  (string->iid "{0002E000-0000-0000-C000-000000000046}"))
 
(define-com-interface (_IEnumGUID _IUnknown)
  ([Next/g (_mfun (_ULONG = 1) ; simplifed to just one
                  (guid : (_ptr o _GUID))
                  (got : (_ptr o _ULONG))
                  -> (r : _HRESULT)
                  -> (cond
                       [(zero? r) guid]
                       [(= r 1) #f]
                       [else (windows-error "Next/g failed" r)]))]
   [Skip _fpointer]
   [Reset _fpointer]
   [Clone _fpointer]))
 
; —— IEnumCATEGORYINFO ——-
 
(define IID_IEnumCATEGORYINFO
  (string->iid "{0002E011-0000-0000-C000-000000000046}"))
 
(define-com-interface (_IEnumCATEGORYINFO _IUnknown)
  ([Next/ci (_mfun (_ULONG = 1) ; simplifed to just one
                   (catinfo : (_ptr o _CATEGORYINFO))
                   (got : (_ptr o _ULONG))
                   -> (r : _HRESULT)
                   -> (cond
                       [(zero? r) catinfo]
                       [(= r 1) #f]
                       [else (windows-error "Next/ci failed" r)]))]
   [Skip _fpointer]
   [Reset _fpointer]
   [Clone _fpointer]))
 
; —— ICatInformation ——-
 
(define IID_ICatInformation
  (string->iid "{0002E013-0000-0000-C000-000000000046}"))
 
(define-com-interface (_ICatInformation _IUnknown)
  ([EnumCategories (_hmfun _LCID
                           (p : (_ptr o _IEnumCATEGORYINFO-pointer))
                           -> EnumCategories p)]
   [GetCategoryDesc (_hmfun _REFCATID _LCID
                            (p : (_ptr o _pointer))
                            -> GetCategoryDesc
                            (begin0
                             (cast p _pointer _string/utf-16)
                             (SysFreeString p)))]
   [EnumClassesOfCategories (_hmfun (_ULONG = 1) ; simplifed
                                    _REFCATID
                                    (_ULONG = 0) ; simplifed
                                    (_pointer = #f)
                                    (p : (_ptr o
                                               _IEnumGUID-pointer))
                                    -> EnumClassesOfCategories p)
                            #:release-with-function Release]
   [IsClassOfCategories _fpointer]
   [EnumImplCategoriesOfClass _fpointer]
   [EnumReqCategoriesOfClass _fpointer]))

; --------------------------------------------------

(when (eq? (system-type) 'windows)
  (show-all-classes))
