#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/winapi
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         racket/date
         racket/runtime-path
         racket/list
         (for-syntax racket/base)
         "private/win32.rkt")

;; Based on Paul Steckler's MysterX (especially the COM automation part)

(provide (protect-out
          ;; Unsafe:

          make-com-object

          define-com-interface
          QueryInterface AddRef Release)

         ;; Unsafe, but protected by original export:

         SysFreeString SysAllocStringLen

         ;; Not useful in safe mode, but harmless:

         _wfun _hfun _mfun _hmfun _GUID _GUID-pointer
         _HRESULT _LCID

         windows-error

         IID_NULL IID_IUnknown
         _IUnknown _IUnknown-pointer _IUnknown_vt

         LOCALE_SYSTEM_DEFAULT

         ;; Safe:
         
         guid? iid? clsid?
         string->guid string->iid string->clsid
         guid->string
         guid=?

         progid->clsid clsid->progid

         com-create-instance com-get-active-object
         com-object? com-object-eq?
         com-object-clsid com-object-set-clsid!
         com-release
         com-object-type com-type? com-type=?

         com-methods com-method-type com-invoke com-omit
         com-get-properties com-get-property-type com-get-property
         com-get-property*
         com-set-properties com-set-property-type com-set-property!

         com-events com-event-type
         com-make-event-executor com-event-executor?
         com-register-event-callback
         com-unregister-event-callback

         com-object-get-iunknown com-iunknown?
         com-object-get-idispatch com-idispatch?

         type-description? 
         type-describe type-described?
         type-described-value type-described-description)

;; FIXME:
;;   call args via var-desc (instead of func-dec)

;; ----------------------------------------
;; GUIDs

(define _REFIID _GUID-pointer)
(define _REFGUID _GUID-pointer)
(define _REFCLSID _GUID-pointer)

(define (copy-guid guid)
  (make-GUID (GUID-l guid)
             (GUID-s1 guid)
             (GUID-s2 guid)
             (GUID-c guid)))

(define (guid? v) (and (GUID? v) #t))
(define (iid? v) (guid? v))
(define (string->iid s [stay-put? #f])
  (string->guid s stay-put?))

(define IID_NULL (make-GUID 0 0 0 '(0 0 0 0 0 0 0 0)))
(define IID_IUnknown (string->iid "{00000000-0000-0000-C000-000000000046}" 
                                  ;; permanent for use in a MULTI_QI:
                                  #t))


(define-ole CLSIDFromProgID (_hfun _string/utf-16 _pointer 
                                   -> CLSIDFromProgID (void)))

(define-ole ProgIDFromCLSID (_fun _GUID-pointer (p : (_ptr o _pointer))
                                  -> (r : _HRESULT)
                                  -> (cond
                                      [(zero? r)
                                       (begin0
                                        (cast p _pointer _string/utf-16)
                                        (CoTaskMemFree p))]
                                      [(= REGDB_E_CLASSNOTREG r) #f]
                                      [else (windows-error "ProgIDFromCLSID: failed" r)])))

(define (progid->clsid progid)
  (unless (string? progid) (raise-type-error 'progid->clsid "string" progid))
  (define clsid (make-GUID 0 0 0 (list 0 0 0 0 0 0 0 0)))
  (CLSIDFromProgID progid clsid)
  clsid)

(define (clsid->progid clsid)
  (unless (clsid? clsid) (raise-type-error 'clsid->progid "clsid" clsid))
  (ProgIDFromCLSID clsid))

(define (clsid? v) (guid? v))
(define (string->clsid s) 
  (string->guid s))

;; ----------------------------------------
;; Manual memory management and strings

(define (_system-string/utf-16 mode)
  (make-ctype _pointer
              (lambda (s)
                (and s
                     (let ([c (string->pointer s)])
                       (register-cleanup! (lambda () (SysFreeString c)))
                       c)))
              (lambda (p)
                (begin0
                 (cast p _pointer _string/utf-16)
                 (when (memq 'out mode) (SysFreeString p))))))

(define current-cleanup (make-parameter #f))
(define current-commit (make-parameter #f))

(define (register-cleanup! proc)
  (let ([c (current-cleanup)])
    (when c
      (set-box! c (cons proc (unbox c))))))

(define (register-commit! proc)
  (let ([c (current-commit)])
    (when c
      (set-box! c (cons proc (unbox c))))))

;; ----------------------------------------
;; Describing COM interfaces for direct calls

(define-syntax-rule (_mfun type ...) (_wfun _pointer type ...))
(define-syntax-rule (_hmfun type ...) (_hfun _pointer type ...))

(define-for-syntax (format-id fmt id)
  (datum->syntax id
                 (string->symbol (format fmt (syntax-e id)))
                 id))

(define-syntax (define-com-interface stx)
  (syntax-case stx ()
    [(_ (_id _super-id) ([method-name type . alloc-spec] ...))
     (let ([strip-underscore (lambda (id)
                               (unless (identifier? id)
                                 (raise-syntax-error 
                                  #f
                                  "expected an identifier"
                                  stx
                                  id))
                               (let ([s (symbol->string (syntax-e id))])
                                 (unless (regexp-match #rx"^_" s)
                                   (raise-syntax-error
                                    #f
                                    "expected an identifier that starts with an underscore"
                                    stx
                                    id))
                                 (datum->syntax id
                                                (string->symbol (substring s 1))
                                                id)))])
       (with-syntax ([id (strip-underscore #'_id)]
                     [super-id (strip-underscore #'_super-id)])
         (for ([m (in-list (syntax->list #'(method-name ...)))]
               [a (in-list (syntax->list #'(alloc-spec ...)))])
           (unless (identifier? m)
             (raise-syntax-error #f "expected a method-name identifier" stx m))
           (syntax-case a ()
             [() (void)]
             [(#:release-with-function id) 
              (unless (identifier? #'id)
                (raise-syntax-error #f "expected an identifier" stx #'id))]
             [(#:release-with-function . _)
              (raise-syntax-error #f "expected an identifier after keyword" stx (syntax-case a () [(x . _) #'x]))]
             [(#:release-with-method id) 
              (unless (identifier? #'id)
                (raise-syntax-error #f "expected an identifier" stx #'id))]
             [(#:release-with-method . _)
              (raise-syntax-error #f "expected an identifier after keyword" stx (syntax-case a () [(x . _) #'x]))]
             [(#:releases) (void)]
             [(#:release . _)
              (raise-syntax-error #f "expected nothing after keyword" stx (syntax-case a () [(x . _) #'x]))]
             [(x . _) (raise-syntax-error #f "bad allocation specification" stx #'x)]
             [x (raise-syntax-error #f "bad allocation specification" stx #'x)]))
         (with-syntax ([id_vt (format-id "~a_vt" #'id)]
                       [_id_vt (format-id "_~a_vt" #'id)]
                       [_id_vt-pointer (format-id "_~a_vt-pointer" #'id)]
                       [_super-id_vt (format-id "_~a_vt" #'super-id)]
                       [id? (format-id "~a?" #'id)])
           #'(begin
               (define-cstruct (_id _super-id) ())
               (define-cstruct (_id_vt _super-id_vt) ([method-name type] ...))
               (define-syntax/maybe type method-name 
                 (make-method-syntax #'id_vt #'id #'id? #'_id_vt-pointer #'method-name #'alloc-spec))
               ...))))]))

(define-syntax define-syntax/maybe
  (syntax-rules (_fpointer)
    [(_ _fpointer . _) (begin)]
    [(_ _ . rest) (define-syntax . rest)]))

(define-for-syntax (make-method-syntax id_vt id id? _id_vt-pointer method-name alloc-spec)
  (lambda (stx)
    (if (identifier? stx)
        (raise-syntax-error #f "method name must be used only in an application position" stx)
        (syntax-case stx ()
          [(_ self arg ...)
           (with-syntax ([id_vt id_vt]
                         [id id]
                         [id? id?]
                         [_id_vt-pointer _id_vt-pointer]
                         [method-name method-name]
                         [alloc-spec alloc-spec]
                         [id_vt-method-name (format-id (format "~~a-~a" (syntax-e method-name))
                                                       id_vt)])
             #'(let ([obj self])
                 (check-com-type 'method-name 'id id? obj)
                 (wrap-alloc-spec
                  alloc-spec
                  (id_vt-method-name (cast (IUnknown-vt obj) _pointer _id_vt-pointer))
                  obj 
                  arg ...)))]))))

(define-syntax wrap-alloc-spec
  (syntax-rules ()
    [(_ () expr arg ...) (expr arg ...)]
    [(_ (#:release-with-function name) expr arg ...)
     (((allocator (lambda (v) (name v)))
       expr)
      arg ...)]
    [(_ (#:release-with-method name) expr obj arg ...)
     (let ([self obj])
       (((allocator (lambda (v) (name self v)))
         (let ([f expr])
           (lambda args
             (AddRef self)
             (apply f args))))
        self
        arg ...))]
    [(_ (#:releases) expr obj arg ...)
     (let ([self obj])
       (((deallocator cadr) 
         (let ([f expr])
           (lambda args
             (apply f args)
             (Release self))))
        self arg ...))]))

(define (check-com-type who id id? obj)
  (unless (id? obj)
    (raise-type-error who (symbol->string id) obj)))

;; --------------------------------------------------
;; IUnknown

(define-cstruct _IUnknown ([vt _pointer]))

(define-cstruct _IUnknown_vt
  ([QueryInterface (_mfun _REFIID (p : (_ptr o _pointer))
                          -> (r : _HRESULT)
                          -> (cond
                              [(= r E_NOINTERFACE) #f]
                              [(positive? r) (windows-error "QueryInterface: failed" r)]
                              [else p]))]
   [AddRef (_mfun -> _ULONG)]
   [Release (_mfun -> _ULONG)]))

(define Release
  ((deallocator)
   (lambda (obj)
     (check-com-type 'Release 'IUknown IUnknown? obj)
     ((IUnknown_vt-Release (cast (IUnknown-vt obj) _pointer _IUnknown_vt-pointer))
      obj))))

(define QueryInterface
  ((allocator Release)
   (lambda (obj refiid _type)
     (check-com-type 'QueryInterface 'IUknown IUnknown? obj)
     (unless (and (ctype? _type)
                  (eq? 'pointer (ctype->layout _type)))
       (raise-type-error 'QueryInterface "pointer ctype" _type))
     (define p ((IUnknown_vt-QueryInterface (cast (IUnknown-vt obj) _pointer _IUnknown_vt-pointer))
                obj
                refiid))
     (and p (cast p _pointer _type)))))

(define AddRef/no-release
  (lambda (obj)
    (check-com-type 'AddRef 'IUknown IUnknown? obj)
    ((IUnknown_vt-AddRef (cast (IUnknown-vt obj) _pointer _IUnknown_vt-pointer))
     obj)))

(define AddRef
  ((retainer Release) AddRef/no-release))

;; --------------------------------------------------
;; IDispatch

(define IID_IDispatch (string->iid "{00020400-0000-0000-C000-000000000046}"))

(define-com-interface (_IDispatch _IUnknown)
  ([GetTypeInfoCount (_hmfun (c : (_ptr o _UINT))
                             -> GetTypeInfoCount c)]
   [GetTypeInfo (_hmfun _UINT _LCID (p : (_ptr o _pointer))
                        -> GetTypeInfo (cast p _pointer _ITypeInfo-pointer))
                #:release-with-function Release]
   [GetIDsOfNames (_mfun _REFIID (_ptr i _string/utf-16)
                         (_UINT = 1) _LCID
                         (p : (_ptr o _DISPID))
                         -> (r : _HRESULT)
                         -> (values r p))]
   [Invoke (_mfun _DISPID _REFIID _LCID _WORD
                  _DISPPARAMS-pointer/null
                  _VARIANT-pointer/null
                  _pointer ; to _EXCEPINFO
                  _pointer ; to _UINT
                  -> _HRESULT)]))

(define error-index-ptr (malloc 'atomic-interior _UINT))

;; --------------------------------------------------
;; ITypeInfo

(define-com-interface (_ITypeInfo _IUnknown)
  ([GetTypeAttr (_hmfun (p : (_ptr o _TYPEATTR-pointer/null))
                        -> GetTypeAttr p)
                #:release-with-method ReleaseTypeAttr]
   [GetTypeComp _fpointer]
   [GetFuncDesc (_hmfun _UINT (p : (_ptr o _FUNCDESC-pointer/null))
                        -> GetFuncDesc p)
                #:release-with-method ReleaseFuncDesc]
   [GetVarDesc (_hmfun _UINT (p : (_ptr o _VARDESC-pointer/null))
                       -> GetVarDesc p)
               #:release-with-method ReleaseVarDesc]
   [GetNames (_hmfun _MEMBERID (s : (_ptr o _pointer)) ; string
                     (_UINT = 1) (i : (_ptr o _UINT))
                     -> GetNames (values (let ([name (cast s _pointer _string/utf-16)])
                                           (SysFreeString s)
                                           name)
                                         i))]
   [GetRefTypeOfImplType (_hmfun _UINT (p : (_ptr o _HREFTYPE))
                                 -> GetRefTypeOfImplType p)]
   [GetImplTypeFlags (_hmfun _UINT (p : (_ptr o _INT))
                             -> GetImplTypeFlags p)]
   [GetIDsOfNames/ti (_hmfun _pointer ; string array
                             _UINT 
                             _pointer ; _MEMBERID array
                             -> GetIDsOfNames (void))]
   [Invoke/ti (_hmfun _pointer
                      _MEMBERID _WORD _DISPPARAMS-pointer
                      (v : (_ptr o _VARIANT))
                      (e : (_ptr o _EXCEPINFO))
                      (err : (_ptr o _UINT))
                      -> Invoke (values v e err))]
   [GetDocumentation (_hmfun _MEMBERID
                             _pointer _pointer _pointer 
                             (p : (_ptr o _pointer))
                             -> GetDocumentation p)]
   [GetDllEntry _fpointer]
   [GetRefTypeInfo (_hmfun _HREFTYPE
                           (p : (_ptr o _pointer)) ; _ITypeInfo-pointer
                           -> GetRefTypeInfo (cast p _pointer _ITypeInfo-pointer))
                   #:release-with-function Release]
   [AddressOfMember _fpointer]
   [CreateInstance _fpointer]
   [GetMops _fpointer]
   [GetContainingTypeLib (_hmfun (p : (_ptr o _pointer)) ; _ITypeLib-pointer
                                 (i : (_ptr o _UINT))
                                 -> GetContainingTypeLib (cast p _pointer _ITypeLib-pointer))
                         #:release-with-function Release]
   [ReleaseTypeAttr (_mfun _TYPEATTR-pointer
                           -> _void)
                    #:releases]
   [ReleaseFuncDesc (_mfun _FUNCDESC-pointer
                           -> _void)
                    #:releases]
   [ReleaseVarDesc (_mfun _VARDESC-pointer
                           -> _void)
                   #:releases]))

;; --------------------------------------------------
;; ITypeLib

(define-com-interface (_ITypeLib _IUnknown)
  ([GetTypeInfoCount/tl (_mfun -> _UINT)]
   [GetTypeInfo/tl (_hmfun _UINT (p : (_ptr o _ITypeInfo-pointer/null))
                           -> GetTypeInfo p)
                   #:release-with-function Release]
   [GetTypeInfoType (_hmfun _UINT (p : (_ptr o _TYPEKIND))
                            -> GetTypeInfoType p)]
   [GetTypeInfoOfGuid (_hmfun _REFGUID (p : (_ptr o _ITypeInfo-pointer/null))
                              -> GetTypeInfoOfGuid p)
                      #:release-with-function Release]
   [GetLibAttr _fpointer]
   [GetTypeComp/tl _fpointer]
   [GetDocumentation/tl _fpointer]
   [IsName _fpointer]
   [FindName _fpointer]
   [ReleaseTLibAttr _fpointer]))

;; ----------------------------------------
;; IProvideClassInfo

(define IID_IProvideClassInfo (string->iid "{B196B283-BAB4-101A-B69C-00AA00341D07}"))

(define-com-interface (_IProvideClassInfo _IUnknown)
  ([GetClassInfo (_hmfun (p : (_ptr o _ITypeInfo-pointer/null))
                         -> GetClassInfo p)
                 #:release-with-function Release]))

;; ----------------------------------------
;; IConnectionPoint

(define IID_IConnectionPoint (string->iid "{B196B286-BAB4-101A-B69C-00AA00341D07}"))

(define-com-interface (_IConnectionPoint _IUnknown)
  ([GetConnectionInterface (_hmfun (g : (_ptr o _GUID))
                                   -> GetConnectionInterface g)]
   [GetConnectionPointContainer _fpointer]
   [Advise (_hmfun _IUnknown-pointer
                   (cookie : (_ptr o _DWORD))
                   -> Advise cookie)]
   [Unadvise (_hmfun _DWORD
                     -> Unadvise (void))]
   [EnumConnections _fpointer]))

;; ----------------------------------------
;; IConnectionPointContainer

(define IID_IConnectionPointContainer (string->iid "{B196B284-BAB4-101A-B69C-00AA00341D07}"))

(define-com-interface (_IConnectionPointContainer _IUnknown)
  ([EnumConnectionPoints _fpointer]
   [FindConnectionPoint (_hmfun _REFIID
                                (p : (_ptr o _IConnectionPoint-pointer/null))
                                -> FindConnectionPoint p)
                        #:release-with-function Release]))

;; ----------------------------------------
;; IClassFactory

(define IID_IClassFactory (string->iid "{00000001-0000-0000-C000-000000000046}"))

(define-com-interface (_IClassFactory _IUnknown)
  ([CreateInstance/factory (_hmfun _IUnknown-pointer/null _REFIID
                                   (p : (_ptr o _ISink-pointer/null))
                                   -> CreateInstance p)]
   [LockServer _fpointer]))


;; ----------------------------------------
;; COM object creation

(define-cstruct _COSERVERINFO ([dwReserved1 _DWORD]
                               [pwszName (_system-string/utf-16 '(in))]
                               [pAuthInfo _pointer]
                               [dwReserved2 _DWORD]))
(define-cstruct _MULTI_QI ([pIID _GUID-pointer]
                           [pItf _IUnknown-pointer/null]
                           [hr _HRESULT]))

(define-ole CoCreateInstance (_hfun _REFCLSID _pointer _DWORD _REFIID
                                    (p : (_ptr o _IUnknown-pointer/null))
                                    -> CoCreateInstance p)
  #:wrap (allocator Release))

(define-ole CoCreateInstanceEx (_hfun _REFCLSID _pointer _DWORD 
                                      _COSERVERINFO-pointer/null
                                      _DWORD
                                      (mqi : _MULTI_QI-pointer)
                                      -> CoCreateInstanceEx
                                      (and (zero? (MULTI_QI-hr mqi))
                                           (MULTI_QI-pItf mqi))))
(define-oleaut GetActiveObject (_hfun _REFCLSID
                                      (_pointer = #f)
                                      (p : (_ptr o _IUnknown-pointer/null))
                                      -> GetActiveObject p)
  #:wrap (allocator Release))

;; We want to create a finalizer on a `com-object' value,
;; and we don't want things that an object references to be
;; finalized before the object. So we use an indirection,
;; the the finalizer on a `com-object' will have the `impl'
;; in its closure:
(struct com-object (impl)
  #:property prop:equal+hash (list
                              (lambda (a b eql?)
                                (ptr-equal? (com-object-unknown a) (com-object-unknown b)))
                              (lambda (a ehc)
                                (ehc (com-object-unknown a)))
                              (lambda (a ehc2)
                                (ehc2 (com-object-unknown a)))))

(struct com-impl ([unknown #:mutable]
                  [dispatch #:mutable]
                  [type-info #:mutable]
                  [event-type-info #:mutable]
                  [clsid #:mutable]
                  [connection-point #:mutable]
                  [connection-cookie #:mutable]
                  [sink #:mutable]
                  [sink-table-links #:mutable]
                  [types #:mutable]
                  [scheme-types #:mutable]
                  [mref #:mutable]))

(define (com-object-unknown obj) (com-impl-unknown (com-object-impl obj)))
(define (com-object-dispatch obj) (com-impl-dispatch (com-object-impl obj)))
(define (com-object-type-info obj) (com-impl-type-info (com-object-impl obj)))
(define (com-object-event-type-info obj) (com-impl-event-type-info (com-object-impl obj)))
(define (com-object-clsid obj) (com-impl-clsid (com-object-impl obj)))
(define (com-object-connection-point obj) (com-impl-connection-point (com-object-impl obj)))
(define (com-object-connection-cookie obj) (com-impl-connection-cookie (com-object-impl obj)))
(define (com-object-sink obj) (com-impl-sink (com-object-impl obj)))
(define (com-object-sink-table-links obj) (com-impl-sink-table-links (com-object-impl obj)))
(define (com-object-types obj) (com-impl-types (com-object-impl obj)))
(define (com-object-scheme-types obj) (com-impl-scheme-types (com-object-impl obj)))
(define (com-object-mref obj) (com-impl-mref (com-object-impl obj)))

(define (com-object-eq? a b)
  (check-com-obj 'com-object-eq? a)
  (check-com-obj 'com-object-eq? b)
  (ptr-equal? (com-object-unknown a) (com-object-unknown b)))

(struct com-type (type-info clsid))

(define scheme_security_check_file
  (get-ffi-obj 'scheme_security_check_file #f (_fun _string _path _int -> _void)))

(define SCHEME_GUARD_FILE_EXECUTE #x4)

(define (register-with-custodian obj)
  (define impl (com-object-impl obj))
  (set-com-impl-mref!
   impl
   (register-custodian-shutdown impl impl-release #:at-exit? #t))
  ;; If we don't finalize the object, then it could
  ;; happen that the object becomes unreachable and
  ;; pointers that the object references could be
  ;; finalized at the same time that the custodian
  ;; changes its weak reference to a strong one; then,
  ;; a custodian shutdown would try to redundantly
  ;; free the pointers.
  (register-finalizer obj (lambda (obj) (impl-release impl))))

(define (do-cocreate-instance who clsid [where 'local])
  (init!)

  ;; Kind of a hack: synthesize a path to represent the class
  ;; to be loaded, so that we have a path for the security-guard
  ;; check. Putting it in the Windows system folder suggests
  ;; an appropriate level of trust: outside of the Racket installation,
  ;; but installed on the current machine.
  (scheme_security_check_file "com-create-instance"
                              (build-path (find-system-path 'sys-dir) 
                                          "com"
                                          (guid->string clsid))
                              SCHEME_GUARD_FILE_EXECUTE)

  (define machine
    (cond
     [(eq? where 'local) #f]
     [(eq? where 'remote) #f]
     [(string? where) where]
     [else (raise-type-error who
                             "'local, 'remote, or a string"
                             where)]))
  (call-as-atomic
   (lambda ()
     (define unknown
       (cond
        [(eq? where 'local)
         (CoCreateInstance clsid #f 
                           (bitwise-ior CLSCTX_LOCAL_SERVER CLSCTX_INPROC_SERVER)
                           IID_IUnknown)]
        [else
         (define cleanup (box null))
         (define csi (parameterize ([current-cleanup cleanup])
                       (make-COSERVERINFO 0 machine #f 0)))
         (define mqi (make-MULTI_QI IID_IUnknown #f 0))
         (define unknown
           (dynamic-wind
            void
            (lambda ()
              (CoCreateInstanceEx clsid #f CLSCTX_REMOTE_SERVER (and machine csi) 1 mqi))
            (lambda ()
              (for ([proc (in-list (unbox cleanup))]) (proc)))))
         (unless (and (zero? (MULTI_QI-hr mqi))
                      unknown)
           (error who "unable to obtain IUnknown interface for remote server"))
         unknown]))

     (make-com-object unknown clsid))))

(define (make-com-object unknown clsid #:manage? [manage? #t])
  (unless (com-iunknown? unknown) (raise-type-error 'make-com-object "com-iunknown" unknown))
  (unless (or (not clsid) (clsid? clsid)) (raise-type-error 'make-com-object "clsid or #f" clsid))
  (define obj (com-object 
               (com-impl unknown
                         #f
                         #f
                         #f
                         clsid
                         #f
                         #f
                         #f
                         #f
                         (make-hash)
                         (make-hash)
                         #f)))
  (when manage?
    (register-with-custodian obj))
  obj)

(define (com-release obj)
  (check-com-obj 'com-release obj)
  (impl-release (com-object-impl obj)))

(define (impl-release impl)
  (call-as-atomic
   (lambda ()
     (let ([mref (com-impl-mref impl)])
       (when mref
         (set-com-impl-mref! impl #f)
         (unregister-custodian-shutdown impl mref)))
     (release-type-types (com-impl-type-info impl))
     (define (bye! sel st!)
       (when (sel impl)
         (Release (sel impl))
         (st! impl #f)))
     (bye! com-impl-type-info
           set-com-impl-type-info!)
     (bye! com-impl-event-type-info
           set-com-impl-event-type-info!)
     (bye! com-impl-connection-point
           set-com-impl-connection-point!)
     (bye! com-impl-sink
           set-com-impl-sink!)
     (bye! com-impl-dispatch
           set-com-impl-dispatch!)
     (bye! com-impl-unknown
           set-com-impl-unknown!))))

(define (release-type-types type-info)
  (when type-info
    (let ([type (type-info-type type-info)])
      (set-type-ref-count! type (sub1 (type-ref-count type)))
      (when (zero? (type-ref-count type))
        (when (positive? (hash-count (type-types type)))
          (for ([td (in-hash-values (type-types type))])
            (release-type-desc td))
          (set-type-types! type (make-hash)))
        (hash-remove! types type-info)))))

(define (release-type-desc td)
  ;; call in atomic mode
  (define type-info (mx-com-type-desc-type-info td))
  (define type-info-impl (mx-com-type-desc-type-info-impl td))
  (define tdd (mx-com-type-desc-desc td))
  (cond
   [(list? tdd)
    (ReleaseFuncDesc type-info (car tdd))
    (when type-info-impl
      (ReleaseFuncDesc type-info-impl (cadr tdd)))]
   [else
    (ReleaseVarDesc type-info tdd)])
  (Release type-info)
  (when type-info-impl
    (Release type-info-impl)))

(define (gen->clsid who name)
  (cond
   [(clsid? name) name]
   [(string? name) (progid->clsid name)]
   [else
    (raise-type-error who "clsid or string" name)]))

(define (com-create-instance name [where 'local])
  (define clsid (gen->clsid 'com-create-instance name))
  (do-cocreate-instance 'com-create-instance clsid where))

(define (com-get-active-object name)
  (init!)
  (define clsid (gen->clsid 'com-get-active-object name))
  (call-as-atomic
   (lambda ()
     (define unknown (GetActiveObject clsid))
     (and unknown
          (make-com-object unknown clsid)))))

(define (check-com-obj who obj)
  (unless (com-object? obj)
    (raise-type-error who "com-object" obj)))

(define (com-object-set-clsid! obj clsid)
  (check-com-obj 'com-object-set-clsid! obj)
  (unless (clsid? clsid) (raise-type-error 'com-object-set-clsid! "clsid" clsid))
  (set-com-impl-clsid! (com-object-impl obj) clsid))

;; ----------------------------------------
;; Getting COM methods and types

(define (com-object-get-unknown obj)
  (or (com-object-unknown obj)
      (error 'com-object-get-unknown "COM object has been released: ~e" obj)))

(define (com-object-get-dispatch obj)
  (or (com-object-dispatch obj)
      (let ([dispatch (QueryInterface (com-object-get-unknown obj)
                                      IID_IDispatch 
                                      _IDispatch-pointer)])
        (unless dispatch
          (error 'com-object-get-idispatch "cannot get IDispatch interface for object: ~e" obj))
        (set-com-impl-dispatch! (com-object-impl obj) dispatch)
        dispatch)))

(struct type (type-info [types #:mutable]
                        scheme-types
                        [ref-count #:mutable]))
(define types (make-weak-hash))

(define (intern-type-info type-info)
  ;; called in atomic mode
  (let ([ti-e (hash-ref types type-info #f)])
    (if ti-e
        (let* ([t (ephemeron-value ti-e)]
               [ti (type-type-info t)])
          (set-type-ref-count! t (add1 (type-ref-count t)))
          (Release type-info)
          (AddRef ti)
          t)
        (let ([t (type type-info (make-hash) (make-hash) 1)])
          (hash-set! types type-info (make-ephemeron type-info t))
          t))))

(define (type-info-type type-info)
  (ephemeron-value (hash-ref types type-info)))

(define (type-info-from-com-object obj [exn? #t])
  (or (com-object-type-info obj)
      (let ([dispatch (com-object-get-dispatch obj)])
        (define c (GetTypeInfoCount dispatch))
        (if (zero? c)
            (if exn?
                (error "COM object does not expose type information")
                #f)
            (let ([type-info (GetTypeInfo
                              dispatch
                              0
                              LOCALE_SYSTEM_DEFAULT)])
              (unless type-info
                (error "Error getting COM type information"))
              (let* ([type (intern-type-info type-info)]
                     [type-info (type-type-info type)]
                     [impl (com-object-impl obj)])
                (set-com-impl-type-info! impl type-info)
                (set-com-impl-types! impl (type-types type))
                (set-com-impl-scheme-types! impl (type-scheme-types type))
                type-info))))))

(define (com-object-type obj)
  (check-com-obj 'com-object-type obj)
  (com-type (type-info-from-com-object obj)
            (com-object-clsid obj)))

(define (event-type-info-from-coclass-type-info coclass-type-info)
  (define type-attr (GetTypeAttr coclass-type-info))
  (for/or ([i (in-range (begin0
                         (TYPEATTR-cImplTypes type-attr)
                         (ReleaseTypeAttr coclass-type-info type-attr)))])
    (define type-flags (GetImplTypeFlags coclass-type-info i))
    (and (bit-and? type-flags IMPLTYPEFLAG_FSOURCE)
         (bit-and? type-flags IMPLTYPEFLAG_FDEFAULT)
         (let ()
           (define ref-type (GetRefTypeOfImplType coclass-type-info i))
           (GetRefTypeInfo coclass-type-info ref-type)))))

(define (type-info=? a b)
  ;; intensional equality
  (or (eq? a b)
      (let ([aa (GetTypeAttr a)]
            [ba (GetTypeAttr b)])
        (begin0
         (guid=? (TYPEATTR-guid aa)
                 (TYPEATTR-guid ba))
         (ReleaseTypeAttr a aa)
         (ReleaseTypeAttr b ba)))))

(define (com-type=? a b)
  (unless (com-type? a) (raise-type-error 'com-type=? "com-type" a))
  (unless (com-type? b) (raise-type-error 'com-type=? "com-type" b))
  (type-info=? (com-type-type-info a) (com-type-type-info b)))

(define (coclass-type-info-from-type-info type-info clsid)
  (define type-lib (GetContainingTypeLib type-info))
  ;; first, try using explicit clsId
  (cond
   [(and clsid
         (GetTypeInfoOfGuid type-lib clsid))
    => (lambda (coclass-type-info)
         (Release type-lib)
         coclass-type-info)]
   ;; if no CLSID, search for coclass implementing supplied
   ;; interface
   [else
    (define coclass-index
      (for/fold ([found #f]) ([i (in-range (GetTypeInfoCount/tl type-lib))])
        (define type-kind (GetTypeInfoType type-lib i))
        (cond
         [(= type-kind TKIND_COCLASS)
          (define coclass-type-info (GetTypeInfo/tl type-lib i))
          (define count (let ()
                          (define type-attr (GetTypeAttr coclass-type-info))
                          (begin0
                           (TYPEATTR-cImplTypes type-attr)
                           (ReleaseTypeAttr coclass-type-info type-attr))))
          (begin0
           (for/fold ([found found]) ([j (in-range count)])
             (define ref-type (GetRefTypeOfImplType coclass-type-info j))
             (define candidate-type-info (GetRefTypeInfo coclass-type-info ref-type))
             (begin0
              (if (type-info=? candidate-type-info type-info)
                  (if found
                      (error "Ambiguous coclass for object")
                      i)
                  found)
              (Release candidate-type-info)))
           (Release coclass-type-info))]
         [else found])))
    (begin0
     (and coclass-index
          (GetTypeInfo/tl type-lib coclass-index))
     (Release type-lib))]))

(define (event-type-info-from-com-object obj)
  (or (com-object-event-type-info obj)
      (let ([dispatch (com-object-get-dispatch obj)])
        (define provide-class-info (QueryInterface dispatch IID_IProvideClassInfo _IProvideClassInfo-pointer))
        (define coclass-type-info
          (cond
           [provide-class-info 
            (begin0 
             (GetClassInfo provide-class-info)
             (Release provide-class-info))]
           [else
            (define type-info (type-info-from-com-object obj))
            (coclass-type-info-from-type-info type-info
                                              (com-object-clsid obj))]))
        (define event-type-info (event-type-info-from-coclass-type-info
                                 coclass-type-info))
        (Release coclass-type-info)
        (set-com-impl-event-type-info! (com-object-impl obj) event-type-info)
        event-type-info)))

(define (is-dispatch-name? s)
  (member s '("AddRef" "GetIDsOfNames"
              "GetTypeInfo" "GetTypeInfoCount"
              "Invoke" "QueryInterface"
              "Release")))

(define (get-type-names type-info type-attr accum inv-kind)
  (define accum1
    (for/fold ([accum accum]) ([i (in-range (TYPEATTR-cImplTypes type-attr))])
      (define ref-type (GetRefTypeOfImplType type-info i))
      (define type-info-impl (GetRefTypeInfo type-info ref-type))
      (define type-attr-impl (GetTypeAttr type-info-impl))
      ;; recursion, to ascend the inheritance graph
      (define new-accum (get-type-names type-info-impl type-attr-impl accum inv-kind))
      (ReleaseTypeAttr type-info-impl type-attr-impl)
      (Release type-info-impl)
      new-accum))
  ;; properties can appear in list of functions
  ;; or in list of variables
  (define accum2
    (for/fold ([accum accum1]) ([i (in-range (TYPEATTR-cFuncs type-attr))])
      (define func-desc (GetFuncDesc type-info i))
      (define new-accum
        (if (= inv-kind (FUNCDESC-invkind func-desc))
            (let-values ([(name count) (GetNames type-info (FUNCDESC-memid func-desc))])
              ;; don't consider names inherited from IDispatch
              (if (or (not (= inv-kind INVOKE_FUNC))
                      (not (is-dispatch-name? name)))
                  (cons name accum)
                  accum))
            accum))
      (ReleaseFuncDesc type-info func-desc)
      new-accum))
  (if (= inv-kind INVOKE_FUNC) ; done, if not a property
      accum2
      (for/fold ([accum accum2]) ([i (in-range (TYPEATTR-cVars type-attr))])
        (define var-desc (GetVarDesc type-info i))
        (let-values ([(name count) (GetNames type-info (FUNCDESC-memid var-desc))])
          (begin0
           (cons name accum)
           (ReleaseVarDesc type-info var-desc))))))

(define (extract-type-info who obj exn?)
  (cond
   [(com-object? obj) (type-info-from-com-object obj exn?)]
   [(com-type? obj) (com-type-type-info obj)]
   [else (raise-type-error who "com-object or com-type" obj)]))

(define (do-get-methods who obj inv-kind)
  (call-as-atomic
   (lambda ()
     (define type-info (extract-type-info who obj #t))
     (define type-attr (GetTypeAttr type-info))
     (begin0
      (sort (get-type-names type-info type-attr null inv-kind) string-ci<?)
      (ReleaseTypeAttr type-info type-attr)))))

(define (com-methods obj)
  (do-get-methods 'com-methods obj INVOKE_FUNC))

(define (com-get-properties obj)
  (do-get-methods 'com-get-properties obj INVOKE_PROPERTYGET))

(define (com-set-properties obj)
  (do-get-methods 'com-set-properties obj INVOKE_PROPERTYPUT))

(define (event-type-info-from-type-info type-info clsid)
  (event-type-info-from-coclass-type-info
   (coclass-type-info-from-type-info type-info clsid)))

(define (extract-event-type-info who obj)
  (cond
   [(com-object? obj) (event-type-info-from-com-object obj)]
   [(com-type? obj) (event-type-info-from-com-type obj)]
   [else (raise-type-error who "com-object or com-type" obj)]))

(define (com-events obj)
  (define event-type-info (extract-event-type-info 'com-events obj))
  (define type-attr (GetTypeAttr event-type-info))
  (begin0
   (sort
    (for/list ([i (in-range (TYPEATTR-cFuncs type-attr))])
      (define func-desc (GetFuncDesc event-type-info i))
      (define-values (name count) (GetNames event-type-info (FUNCDESC-memid func-desc)))
      (begin0
       name
       (ReleaseFuncDesc event-type-info func-desc)))
    string-ci<?)
   (ReleaseTypeAttr event-type-info type-attr)))

(struct mx-com-type-desc (memid
                          type-info
                          type-info-impl
                          fun-offset
                          impl-guid
                          desc))

(define (function-type-desc? td)
  (list? (mx-com-type-desc-desc td)))

(define (type-desc-from-type-info name inv-kind type-info)
  (define type-attr (GetTypeAttr type-info))
  ;; can skip first 7, because those are IDispatch-specific
  (define num-funcs (TYPEATTR-cFuncs type-attr))
  (define found
    (or
     (for/or ([i (in-range 7 num-funcs)])
       (define func-desc (GetFuncDesc type-info i))
       (define-values (ti-name name-count) (GetNames type-info (FUNCDESC-memid func-desc)))
       ;; see if this FUNCDESC is the one we want
       (cond
        [(and (string=? ti-name name)
              (or (= inv-kind INVOKE_EVENT)
                  (= inv-kind (FUNCDESC-invkind func-desc))))
         (list func-desc i)]
        [else
         (ReleaseFuncDesc type-info func-desc)
         #f]))
     (and (or (= inv-kind INVOKE_PROPERTYGET)
              (= inv-kind INVOKE_PROPERTYPUT)
              (= inv-kind INVOKE_PROPERTYPUTREF))
          (for/or ([i (in-range (TYPEATTR-cVars type-attr))])
            (define var-desc (GetVarDesc type-info i))
            (define-values (ti-name name-count) (GetNames type-info (VARDESC-memid var-desc)))
            ;; see if this VARDESC is the one we want
            (cond
             [(string=? ti-name name)
              var-desc]
             [else
              (ReleaseVarDesc type-info var-desc)
              #f])))
     ;; search in inherited interfaces
     (for/or ([i (in-range (TYPEATTR-cImplTypes type-attr))])
       (define ref-type (GetRefTypeOfImplType type-info i))
       (define type-info-impl (GetRefTypeInfo type-info ref-type))
       ;; recursion, to ascend the inheritance graph
       (define type-desc (type-desc-from-type-info name inv-kind type-info-impl))
       (Release type-info-impl)
       type-desc)))

  (ReleaseTypeAttr type-info type-attr)

  (cond
   [(mx-com-type-desc? found) found]
   [(not found) #f]
   [(VARDESC? found)
    (mx-com-type-desc (VARDESC-memid found)
                      (begin
                        (AddRef type-info)
                        type-info)
                      #f
                      #f
                      #f
                      found)]
   [else
    (define ref-type (with-handlers ([exn:fail? (lambda (x) #f)])
                       (GetRefTypeOfImplType type-info -1)))
    (define type-info-impl (and ref-type
                                (GetRefTypeInfo type-info ref-type)))
    (define mx-type-desc
      (and type-info-impl
           (let ([type-attr-impl (GetTypeAttr type-info-impl)])
             ;; assumption: impl TypeInfo has FuncDescs in same order
             ;;             as the Dispatch TypeInfo
             ;; but num-funcs has IDispatch methods
             (define func-index (- (cadr found)
                                   (- num-funcs
                                      (TYPEATTR-cFuncs type-attr-impl))))
             (define func-desc-impl (GetFuncDesc type-info-impl func-index))
             (begin0
              (if (or (= (FUNCDESC-funckind func-desc-impl) FUNC_VIRTUAL)
                      (= (FUNCDESC-funckind func-desc-impl) FUNC_PUREVIRTUAL))
                  (mx-com-type-desc (FUNCDESC-memid (car found))
                                    (begin
                                      (AddRef type-info)
                                      type-info)
                                    (begin
                                      (AddRef type-info-impl)
                                      type-info-impl)
                                    (quotient (FUNCDESC-oVft func-desc-impl) (ctype-sizeof _pointer))
                                    (copy-guid (TYPEATTR-guid type-attr-impl))
                                    (list (car found)
                                          func-desc-impl))
                  (begin
                    (ReleaseFuncDesc type-info-impl func-desc-impl)
                    #f))
              (ReleaseTypeAttr type-info-impl type-attr-impl))
             (Release type-info-impl))))

    (or mx-type-desc
        (mx-com-type-desc (FUNCDESC-memid (car found))
                          (begin
                            (AddRef type-info)
                            type-info)
                          #f
                          #f
                          #f
                          (list (car found) #f)))]))

(define (event-type-info-from-com-type obj)
  (event-type-info-from-type-info (com-type-type-info obj)
                                  (com-type-clsid obj)))

(define (get-method-type obj name inv-kind [exn? #t])
  (or (hash-ref (com-object-types obj) (cons name inv-kind) #f)
      (let ([type-info
             (cond
              [(= inv-kind INVOKE_EVENT)
               (event-type-info-from-com-object obj)]
              [else
               (type-info-from-com-object obj exn?)])])
        (and type-info
             (let ([mx-type-desc (type-desc-from-type-info name inv-kind type-info)])
               (when mx-type-desc
                 (hash-set! (com-object-types obj) (cons name inv-kind) mx-type-desc))
               mx-type-desc)))))

(define (get-var-type-from-elem-desc elem-desc
                                     #:keep-safe-array? [keep-safe-array? #f])
  ;; hack: allow elem-desc as a TYPEDESC
  (define param-desc (and (ELEMDESC? elem-desc)
                          (union-ref (ELEMDESC-u elem-desc) 1)))
  (define flags (if param-desc
                    (PARAMDESC-wParamFlags param-desc)
                    0))
  (define (fixup-vt vt)
    (cond
     [(= vt (bitwise-ior VT_USERDEFINED VT_BYREF))
      VT_UNKNOWN]
     [(= vt VT_USERDEFINED)
      VT_INT]
     [(and (= vt VT_SAFEARRAY)
           (not keep-safe-array?))
      (bitwise-ior VT_ARRAY VT_VARIANT)]
     [else vt]))
  (define type-desc (if (ELEMDESC? elem-desc)
                        (ELEMDESC-tdesc elem-desc)
                        elem-desc))
  (cond
   [(and (bit-and? flags PARAMFLAG_FOPT)
         (bit-and? flags PARAMFLAG_FHASDEFAULT))
    (fixup-vt
     (VARIANT-vt (PARAMDESCEX-varDefaultValue (PARAMDESC-pparamdescex param-desc))))]
   [(= (TYPEDESC-vt type-desc) VT_PTR)
    (fixup-vt
     (bitwise-ior VT_BYREF
                  (TYPEDESC-vt (cast (union-ref (TYPEDESC-u type-desc) 0) 
                                     _pointer 
                                     _TYPEDESC-pointer))))]
   [else
    (fixup-vt (TYPEDESC-vt type-desc))]))

(define (elem-desc-has-default? elem-desc)
  (define param-desc (union-ref (ELEMDESC-u elem-desc) 1))
  (define flags (PARAMDESC-wParamFlags param-desc))
  (bit-and? flags PARAMFLAG_FHASDEFAULT))

(define (elem-desc-is-optional? elem-desc)
  (define param-desc (union-ref (ELEMDESC-u elem-desc) 1))
  (define flags (PARAMDESC-wParamFlags param-desc))
  (bit-and? flags PARAMFLAG_FOPT))

(define-syntax-rule (switch e [val expr] ... [else else-expr])
  (let ([v e])
    (cond
     [(= v val) expr]
     ...
     [else else-expr])))

(define (elem-desc-to-scheme-type elem-desc ignore-by-ref? is-opt? internal?)
  (define vt (let ([vt (get-var-type-from-elem-desc elem-desc #:keep-safe-array? #t)])
               (if (and ignore-by-ref?
                        (not (= vt (bitwise-ior VT_USERDEFINED VT_BYREF))))
                   (- vt (bitwise-and vt VT_BYREF))
                   vt)))
  (cond
   [(= vt (bitwise-ior VT_USERDEFINED VT_BYREF))
    ;; The convention is that these represent specific COM interfaces
    ;; that the caller and callee have agreed upon.  For our purposes,
    ;; it is an IUnknown pointer.
    (if is-opt?
        '(opt iunknown)
        'iunknown)]
   [(= vt VT_SAFEARRAY) `(array ? any)]
   [(bit-and? vt VT_ARRAY)
    (define array-desc (cast (union-ref (TYPEDESC-u (ELEMDESC-tdesc elem-desc)) 1)
                             _pointer
                             _ARRAYDESC-pointer))
    (define base
      (elem-desc-to-scheme-type (ARRAYDESC-tdescElem array-desc) #f #f internal?))
    (for/fold ([base base]) ([i (in-range (ARRAYDESC-cDims array-desc))])
      `(array ,(SAFEARRAYBOUND-cElements (ptr-ref (array-ptr (ARRAYDESC-rgbounds array-desc)) 
                                                  _SAFEARRAYBOUND
                                                  i))
              ,base))]
   [else
    (define as-iunk? (= vt (bitwise-ior VT_USERDEFINED VT_BYREF)))
    (define base (vt-to-scheme-type (if as-iunk?
                                        vt
                                        (- vt (bitwise-and vt VT_BYREF)))))
    (define new-base
      (if (and (not as-iunk?)
               (bit-and? vt VT_BYREF))
          `(box ,base)
          base))
    (if is-opt?
        `(opt ,new-base)
        new-base)]))

(define (vt-to-scheme-type vt)
  (switch 
   vt
   [VT_HRESULT 'void]
   [VT_EMPTY 'void]
   [VT_NULL 'void]
   [VT_UI1 'char]
   [VT_UI2 'unsigned-short]
   [VT_UI4 'unsigned-int]
   [VT_UINT 'unsigned-int]
   [VT_UI8 'unsigned-long-long]
   [VT_I1 'signed-char]
   [VT_I2 'short-int]
   [VT_I4 'int]
   [VT_INT 'int]
   [VT_I8 'long-long]
   [VT_R4 'float]
   [VT_R8 'double]
   [VT_BSTR 'string]
   [VT_CY 'currency]
   [VT_DATE 'date]
   [VT_BOOL 'boolean]
   [VT_ERROR 'scode]
   [VT_UNKNOWN 'iunknown]
   [VT_DISPATCH 'com-object]
   [VT_VARIANT 'any]
   [VT_USERDEFINED
    ;; Reporting this as `user-defined' is sure to confuse somebody.
    ;; The convention is that these are ENUMs that the caller and the
    ;; callee have agreed upon.  For our purposes, they will be INTs,
    ;; but we'll report them as an enumeration.
    'com-enumeration]
   [VT_VOID 'void]
   [VT_SAFEARRAY `(array ? any)]
   [else 
    (cond
     [(= VT_ARRAY (bitwise-and vt VT_ARRAY))
      `(array ? ,(vt-to-scheme-type (- vt VT_ARRAY)))]
     [(= vt (bitwise-ior VT_USERDEFINED VT_BYREF))
      'iunknown]
     [(= VT_BYREF (bitwise-and vt VT_BYREF))
      `(box ,(vt-to-scheme-type (- vt VT_BYREF)))]
     [else
      (string->symbol (format "COM-0x~x" vt))])]))

(define (arg-to-type arg [in-array 0])
  (cond
   [(type-described? arg)
    (type-described-description arg)]
   [(vector? arg) `(array ,(vector-length arg)
                          ,(if (zero? (vector-length arg))
                               'int
                               (for/fold ([t (arg-to-type (vector-ref arg 0))]) ([v (in-vector arg)])
                                 (if (equal? t (arg-to-type v))
                                     t
                                     'any))))]
   [(in-array . > . 1) 'any]
   [(boolean? arg) 'boolean]
   [(signed-int? arg 32) 'int]
   [(unsigned-int? arg 32) 'unsigned-int]
   [(signed-int? arg 64) 'long-long]
   [(unsigned-int? arg 64) 'unsigned-long-long]
   [(string? arg) 'string]
   [(real? arg) 'double]
   [(com-object? arg) 'com-object]
   [(IUnknown? arg) 'iunknown]
   [(eq? com-omit arg) 'any]
   [(box? arg) `(box ,(arg-to-type (unbox arg)))]
   [else (error 'com "cannot infer marshal format for value: ~e" arg)]))

(define (elem-desc-ref func-desc i)
  (ptr-add (FUNCDESC-lprgelemdescParam func-desc) i _ELEMDESC))

(define (is-last-param-retval? inv-kind func-desc)
  (define num-params (FUNCDESC-cParams func-desc))
  (and (positive? num-params)
       (or (= inv-kind INVOKE_PROPERTYGET)
           (= inv-kind INVOKE_FUNC))
       (bit-and?
        PARAMFLAG_FRETVAL
        (PARAMDESC-wParamFlags
         (union-ref
          (ELEMDESC-u (elem-desc-ref func-desc (sub1 num-params)))
          1)))))

(define (get-opt-param-count func-desc num-params)
  (for/fold ([count 0]) ([i (in-range num-params)])
    (if (bit-and?
         PARAMFLAG_FOPT
         (PARAMDESC-wParamFlags
          (union-ref
           (ELEMDESC-u (elem-desc-ref func-desc (sub1 num-params)))
           1)))
        (add1 count)
        0)))

(define (do-get-method-type who obj name inv-kind internal?)
  (call-as-atomic
   (lambda ()
     (or (and (com-object? obj)
              (hash-ref (com-object-scheme-types obj) (cons name inv-kind) #f))
         (let ([t (get-uncached-method-type who obj name inv-kind internal?)])
           (when (com-object? obj)
             (hash-set! (com-object-scheme-types obj) (cons name inv-kind) t))
           t)))))

(define (get-uncached-method-type who obj name inv-kind internal?)
  (define type-info (extract-type-info who obj (not internal?)))
  (when (and (= inv-kind INVOKE_FUNC)
             (is-dispatch-name? name))
    (error who "IDispatch methods not available"))
  (define mx-type-desc
    (cond
     [(com-object? obj) (get-method-type obj name inv-kind (not internal?))]
     [else (define x-type-info
             (if (= inv-kind INVOKE_EVENT)
                 (event-type-info-from-com-type obj)
                 type-info))
           (type-desc-from-type-info name inv-kind x-type-info)]))
  (cond
   [(not mx-type-desc)
    ;; there is no type info
    #f]
   [else
    (define-values (args ret)
      (cond
       [(function-type-desc? mx-type-desc)
        (define func-desc (car (mx-com-type-desc-desc mx-type-desc)))
        (define num-actual-params (FUNCDESC-cParams func-desc))
        (cond
         [(= -1 (FUNCDESC-cParamsOpt func-desc))
          ;; all args > pFuncDesc->cParams - 1 get packaged into SAFEARRAY,
          ;; but that is handled by COM automation; we just pass "any"s
          (values
           (append
            (for/list ([i (in-range (sub1 num-actual-params))])
                      (elem-desc-to-scheme-type (elem-desc-ref func-desc i)
                                                #f
                                                #f
                                                internal?))
            '(any ...))
           (elem-desc-to-scheme-type (FUNCDESC-elemdescFunc func-desc)
                                     #f
                                     #f
                                     internal?))]
         [else
          (define last-is-retval?
            (is-last-param-retval? inv-kind func-desc))
          (define num-params (- num-actual-params (if last-is-retval? 1 0)))
          ;; parameters that are optional with a default value in IDL are not
          ;; counted in pFuncDesc->cParamsOpt, so look for default bit flag
          (define num-opt-params (get-opt-param-count func-desc num-params))
          (define first-opt-arg (- num-params num-opt-params))
          (values
           (for/list ([i (in-range num-params)])
                     (elem-desc-to-scheme-type (elem-desc-ref func-desc i)
                                               #f
                                               (i . >= . first-opt-arg)
                                               internal?))
           (elem-desc-to-scheme-type (if last-is-retval?
                                         (elem-desc-ref func-desc num-params)
                                         (FUNCDESC-elemdescFunc func-desc))
                                     #t
                                     #f
                                     internal?))])]
       [(= inv-kind INVOKE_PROPERTYGET)
        (define var-desc (mx-com-type-desc-desc mx-type-desc))
        (values null
                (elem-desc-to-scheme-type (VARDESC-elemdescVar var-desc)
                                          #f
                                          #f
                                          internal?))]
       [(= inv-kind INVOKE_PROPERTYPUT)
        (define var-desc (mx-com-type-desc-desc mx-type-desc))
        (values (list (elem-desc-to-scheme-type (VARDESC-elemdescVar var-desc)
                                                #f
                                                #f
                                                internal?))
                'void)]
       [(= inv-kind INVOKE_EVENT)
        (values null 'void)]))
    `(-> ,args ,ret)]))

(define (com-method-type obj name)
  (do-get-method-type 'com-method-type obj name INVOKE_FUNC #f))

(define (com-get-property-type obj name)
  (do-get-method-type 'com-get-property-type obj name INVOKE_PROPERTYGET #f))

(define (com-set-property-type obj name)
  (do-get-method-type 'com-set-property-type obj name INVOKE_PROPERTYPUT #f))

(define (com-event-type obj name)
  (do-get-method-type 'com-event-type obj name INVOKE_EVENT #f))

(define (get-func-desc-for-event name type-info)
  (for/or ([i (in-range (let ()
                          (define type-attr (GetTypeAttr type-info))
                          (begin0
                           (TYPEATTR-cFuncs type-attr)
                           (ReleaseTypeAttr type-info type-attr))))])
    (define func-desc (GetFuncDesc type-info i))
    (define-values (fname index) (GetNames type-info (FUNCDESC-memid func-desc)))
    (if (string=? name fname)
        func-desc
        (begin
          (ReleaseFuncDesc type-info func-desc)
          #f))))

;; ----------------------------------------
;; Calling COM Methods via IDispatch

(define-oleaut VariantInit (_wfun _VARIANT-pointer -> _void))

(define com-omit 
  (let ()
    (struct com-omit ())
    (com-omit)))

(define CY-factor 10000)

(define (currency? v)
  (and (real? v)
       (exact? v)
       (integer? (* v CY-factor))
       (signed-int? (* v CY-factor) 64)))

(define-cstruct _SYSTEMTIME ([wYear _WORD]
                             [wMonth _WORD]
                             [wDayOfWeek _WORD]
                             [wDay _WORD]
                             [wHour _WORD]
                             [wMinute _WORD]
                             [wSecond _WORD]
                             [wMilliseconds _WORD]))

(define-ole VariantTimeToSystemTime (_wfun _DATE _SYSTEMTIME-pointer
                                           -> _INT))
(define-ole SystemTimeToVariantTime (_wfun _SYSTEMTIME-pointer (d : (_ptr o _DATE))
                                           -> (r : _int)
                                           -> (and (zero? r) d)))

(define _date
  (make-ctype _DATE
              (lambda (d)
                (define s (make-SYSTEMTIME (date-year d)
                                           (date-month d)
                                           (date-week-day d)
                                           (date-day d)
                                           (date-hour d)
                                           (date-minute d)
                                           (date-second d)
                                           (if (date*? d)
                                               (inexact->exact (floor (* (date*-nanosecond d) 1000)))
                                               0)))
                (define d (SystemTimeToVariantTime s))
                (or d
                    (error 'date "error converting date to COM date")))
              (lambda (d)
                (define s (make-SYSTEMTIME 0 0 0 0 0 0 0 0))
                (unless (zero? (VariantTimeToSystemTime d s))
                  (error 'date "error converting date from COM date"))
                (seconds->date
                 (find-seconds (SYSTEMTIME-wSecond s)
                               (SYSTEMTIME-wMinute s)
                               (SYSTEMTIME-wHour s)
                               (SYSTEMTIME-wDay s)
                               (SYSTEMTIME-wMonth s)
                               (SYSTEMTIME-wYear s))))))

(define _currency
  (make-ctype _CY
              (lambda (s)
                (* s CY-factor))
              (lambda (s)
                (/ s CY-factor))))
              

(define (unsigned-int? v n)
  (and (exact-integer? v)
       (positive? v)
       (zero? (arithmetic-shift v (- n)))))

(define (signed-int? v n)
  (and (exact-integer? v)
       (if (negative? v)
           (= -1 (arithmetic-shift v (- (sub1 n))))
           (zero? (arithmetic-shift v (- (sub1 n)))))))

(define (ok-argument? arg type)
  (cond
   [(type-described? arg)
    (ok-argument? (type-described-value arg) type)]
   [(symbol? type)
    (case type
      [(void) (void? arg)]
      [(char) (byte? arg)]
      [(unsigned-short) (unsigned-int? arg 16)]
      [(unsigned-int) (unsigned-int? arg 32)]
      [(unsigned-long-long) (unsigned-int? arg 64)]
      [(signed-char) (signed-int? arg 8)]
      [(short-int) (signed-int? arg 16)]
      [(int) (signed-int? arg 32)]
      [(long-long) (signed-int? arg 64)]
      [(float double) (real? arg)]
      [(string) (string? arg)]
      [(currency) (currency? arg)]
      [(date) (date? arg)]
      [(boolean) #t]
      [(scode) (signed-int? arg 32)]
      [(iunknown) (or (IUnknown? arg)
                      (com-object? arg))]
      [(com-object) (com-object? arg)]
      [(any ...) #t]
      [(com-enumeration) (signed-int? arg 32)]
      [else #f])]
   [(eq? 'opt (car type))
    (or (eq? com-omit arg)
        (ok-argument? arg (cadr type)))]
   [(eq? 'box (car type))
    (and (box? arg)
         (ok-argument? (unbox arg) (cadr type)))]
   [(eq? 'array (car type))
    (and (vector? arg)
         (or (eq? (cadr type) '?)
             (= (vector-length arg) (cadr type)))
         (for/and ([v (in-vector arg)])
           (ok-argument? v (caddr type))))]
   [(eq? 'variant (car type))
    (ok-argument? arg (cadr type))]
   [else #f]))

(define (type-description? type)
  (cond
   [(symbol? type)
    (hash-ref
     #hasheq((void . #t)
             (char . #t)
             (unsigned-short . #t)
             (unsigned-int . #t)
             (unsigned-long-long . #t)
             (signed-char . #t)
             (short-int . #t)
             (int . #t)
             (long-long . #t)
             (float . #t)
             (double . #t)
             (string . #t)
             (currency . #t)
             (date . #t)
             (boolean . #t)
             (scode . #t)
             (iunknown . #t)
             (com-object . #t)
             (any . #t)
             (com-enumeration . #t)
             ;; meant to to be used only at the end 
             ;; of an argument list:
             (... . #t))
     type
     #f)]
   [(and (list? type)
         (pair? type))
    (cond
     [(eq? 'opt (car type))
      (and (= (length type) 2)
           (type-description? (cadr type)))]
     [(eq? 'box (car type))
      (and (= (length type) 2)
           (type-description? (cadr type)))]
     [(eq? 'array (car type))
      (and (= (length type) 3)
           (or (exact-positive-integer? (cadr type))
               (eq? '? (cadr type)))
           (type-description? (caddr type)))]
     [(eq? 'variant (car type))
      (and (= (length type) 2)
           (type-description? (cadr type)))]
     [else #f])]
   [else #f]))

(struct type-described (value description))

(define (type-describe v desc)
  (unless (type-description? desc)
    (raise-type-error 'type-describe "type description" desc))
  (type-described v desc))

(define (check-argument who method arg type)
  (unless (ok-argument? arg type)
    (raise-type-error (string->symbol method) (format "~s" type) arg)))

(define (get-lcid-param-index func-desc)
  (for/or ([i (in-range (FUNCDESC-cParams func-desc))])
    (and (bit-and? (PARAMDESC-wParamFlags (union-ref (ELEMDESC-u (elem-desc-ref func-desc i)) 1))
                   PARAMFLAG_FLCID)
         i)))

(define prop-put-long (malloc _LONG 'atomic-interior))
(ptr-set! prop-put-long _LONG DISPID_PROPERTYPUT)

(define (variant-set! var type val)
  (ptr-set! (union-ptr (VARIANT-u var)) type val))

(define (scheme-to-variant! var a elem-desc scheme-type #:mode [mode '(in)])
  (cond
   [(type-described? a)
    (scheme-to-variant! var (type-described-value a) elem-desc scheme-type #:mode mode)]
   [(and (pair? scheme-type) (eq? 'variant (car scheme-type)))
    (scheme-to-variant! var a elem-desc (cadr scheme-type) #:mode mode)]
   [(eq? a com-omit)
    (if (and elem-desc
             (elem-desc-has-default? elem-desc))
        (begin
          (memcpy var 
                  (PARAMDESCEX-varDefaultValue 
                   (PARAMDESC-pparamdescex (union-ref (ELEMDESC-u elem-desc) 1)))
                  1
                  _VARIANT))
        (begin
          (set-VARIANT-vt! var VT_ERROR)
          (variant-set! var _ulong DISP_E_PARAMNOTFOUND)))]
   [(and elem-desc (not (any-type? scheme-type)))
    (set-VARIANT-vt! var (get-var-type-from-elem-desc elem-desc))
    (variant-set! var (to-ctype scheme-type #:mode mode) a)]
   [else
    (define use-scheme-type (if (any-type? scheme-type)
                                (arg-to-type a)
                                scheme-type))
    (set-VARIANT-vt! var (to-vt use-scheme-type))
    (variant-set! var (to-ctype use-scheme-type #:mode mode) a)]))

(define (any-type? t)
  (or (eq? t 'any)
      (and (pair? t)
           (eq? (car t) 'opt)
           (any-type? (cadr t)))))

(define _float*
  (make-ctype _float
              (lambda (v) (exact->inexact v))
              (lambda (v) v)))

(define (_box/permanent _t)
  (define (extract p)
    (if (eq? _t _VARIANT)
        (variant-to-scheme (cast p _pointer _VARIANT-pointer) #:mode '(in out))
        (ptr-ref p _t)))
  (make-ctype _pointer
              (lambda (v)
                (define p (malloc 'raw 1 _t))
                (if (eq? _t _VARIANT)
                    (let ([p (cast p _pointer _VARIANT-pointer)]
                          [v (unbox v)])
                      (VariantInit p)
                      (scheme-to-variant! p v #f (arg-to-type v) #:mode '(in out)))
                    (ptr-set! p _t (unbox v)))
                (register-cleanup! 
                 (lambda ()
                   (set-box! v (extract p))
                   (free p)))
                p)
              (lambda (p)
                ;; We box the value, but we don't support reflecting box
                ;; changes back to changes of the original reference:
                (box (extract p)))))

(define (make-a-VARIANT [mode 'atomic-interior])
  (define var (cast (malloc _VARIANT mode)
                    _pointer 
                    (if (eq? mode 'raw)
                        _VARIANT-pointer
                        (_gcable _VARIANT-pointer))))
  (VariantInit var)
  var)

(define (extract-variant-pointer var get? [vt (VARIANT-vt var)])
  (define ptr (union-ptr (VARIANT-u var)))
  (switch
   vt
   [VT_BSTR (if get? ptr (ptr-ref ptr _pointer))]
   [VT_DISPATCH (if get? ptr (ptr-ref ptr _pointer))]
   [VT_UNKNOWN (if get? ptr (ptr-ref ptr _pointer))]
   [VT_VARIANT var]
   [else ptr]))

(define (_safe-array/vectors given-dims base mode)
  (make-ctype _pointer
              (lambda (v)
                (define base-vt (to-vt base))
                (define dims (if (equal? given-dims '(?))
                                 (list (vector-length v))
                                 given-dims))
                (define sa (SafeArrayCreate base-vt
                                            (length dims)
                                            (for/list ([d (in-list dims)])
                                              (make-SAFEARRAYBOUND d 0))))
                (register-cleanup!
                 (lambda () (SafeArrayDestroy sa)))
                (let loop ([v v] [index null] [dims dims])
                  (for ([v (in-vector v)]
                        [i (in-naturals)])
                    (define idx (cons i index))
                    (if (null? (cdr dims))
                        (let ([var (make-a-VARIANT)])
                          (scheme-to-variant! var v #f base #:mode mode)
                          (SafeArrayPutElement sa (reverse idx) 
                                               (extract-variant-pointer var #f base-vt)))
                        (loop v idx (cdr dims)))))
                sa)
              (lambda (_sa)
                (define sa (cast _sa _pointer _SAFEARRAY-pointer))
                (define dims (for/list ([i (in-range (SafeArrayGetDim sa))])
                               (- (add1 (SafeArrayGetUBound sa (add1 i)))
                                  (SafeArrayGetLBound sa (add1 i)))))
                (define vt (SafeArrayGetVartype sa))
                (let loop ([dims dims] [level 1] [index null])
                  (define lb (SafeArrayGetLBound sa level))
                  (for/vector ([i (in-range (car dims))])
                    (if (null? (cdr dims))
                        (let ([var (make-a-VARIANT)])
                          (set-VARIANT-vt! var vt)
                          (SafeArrayGetElement sa (reverse (cons i index)) 
                                               (extract-variant-pointer var #t))
                          (variant-to-scheme var #:mode mode))
                        (loop (cdr dims) (add1 level) (cons i index))))))))

(define (_IUnknown-pointer-or-com-object mode)
  (make-ctype 
   _IUnknown-pointer/null
   (lambda (v)
     (define p
       (if (com-object? v)
           (com-object-get-iunknown v)
           v))
     (when (memq 'out mode)
       (register-commit! (lambda () (AddRef/no-release p))))
     p)
   (lambda (p) 
     (if p
         (begin
           (if (memq 'out mode)
               (((allocator Release) (lambda () p)))
               (AddRef p))
           (make-com-object p #f))
         p))))

(define (_com-object mode)
  (_IUnknown-pointer-or-com-object mode))

(define (to-ctype type [as-boxed? #f] #:mode [mode '()])
  (cond
   [(symbol? type)
    (case type
      [(void) #f]
      [(char) _byte]
      [(unsigned-short) _ushort]
      [(unsigned-int) _uint]
      [(unsigned-long-long) _ullong]
      [(signed-char) _sbyte]
      [(short-int) _short]
      [(int) _int]
      [(long-long) _llong]
      [(float) _float*]
      [(double) _double*]
      [(string) (_system-string/utf-16 mode)]
      [(currency) _currency]
      [(date) _date]
      [(boolean) _bool]
      [(scode) _SCODE]
      [(iunknown) (_IUnknown-pointer-or-com-object mode)]
      [(com-object) (_com-object mode)]
      [(any ...) (if as-boxed?
                     _VARIANT
                     (error "internal error: cannot marshal to any"))]
      [(com-enumeration) _int]
      [else (error 'to-ctype "internal error: unknown type ~s" type)])]
   [(eq? 'opt (car type))
    (to-ctype (cadr type) #:mode mode)]
   [(eq? 'box (car type))
    (_box/permanent (to-ctype (cadr type) #t #:mode '(in out)))]
   [(eq? 'array (car type))
    (define-values (dims base)
      (let loop ([t type] [?-ok? #t])
        (cond
         [(and (pair? t) (eq? 'array (car t)) (or ?-ok? (number? (cadr t))))
          (define-values (d b) (if (number? (cadr t))
                                   (loop (caddr t) #f)
                                   (values null (cadr t))))
          (values (cons (cadr t) d) b)]
         [else
          (values null t)])))
    (_safe-array/vectors dims base mode)]
   [(eq? 'variant (car type))
    (to-ctype (cadr type) #:mode mode)]
   [else #f]))

(define (to-vt type)
  ;; used for inferred or described types
  (case type
    [(void) VT_VOID]
    [(char) VT_UI1]
    [(unsigned-short) VT_UI2]
    [(unsigned-int) VT_UI4]
    [(unsigned-long-long) VT_UI8]
    [(signed-char) VT_I1]
    [(short-int) VT_I2]
    [(int) VT_I4]
    [(long-long) VT_I8]
    [(float) VT_R4]
    [(double) VT_R8]
    [(string) VT_BSTR]
    [(currency) VT_CY]
    [(date) VT_DATE]
    [(boolean) VT_BOOL]
    [(iunknown) VT_UNKNOWN]
    [(com-object) VT_DISPATCH]
    [(any ...) VT_VARIANT]
    [(com-enumeration) VT_INT]
    [else 
     (case (and (pair? type)
                (car type))
       [(array) (bitwise-ior VT_ARRAY (to-vt (caddr type)))]
       [(opt) (to-vt (cadr type))]
       [(variant) VT_VARIANT]
       [(box) (bitwise-ior VT_BYREF (to-vt (cadr type)))]
       [else
        (error 'to-vt "internal error: unsupported type ~s" type)])]))

(define (build-method-arguments-using-function-desc func-desc scheme-types inv-kind args)
  (define lcid-index (and func-desc (get-lcid-param-index func-desc)))
  (define last-is-retval? (and func-desc (is-last-param-retval? inv-kind func-desc)))
  (define last-is-repeat-any? (and func-desc (= -1 (FUNCDESC-cParamsOpt func-desc))))
  (define base-count (if func-desc
                         (- (FUNCDESC-cParams func-desc)
                            (if lcid-index 1 0)
                            (if last-is-retval? 1 0))
                         (length scheme-types)))
  (define count (if last-is-repeat-any?
                    (if (or lcid-index
                            last-is-retval?)
                        (error "cannot handle combination of `any ...' and lcid/retval")
                        (length scheme-types))
                    base-count))
  (build-method-arguments-from-desc count
                                    (lambda (i)
                                      (and func-desc 
                                           (or (not last-is-repeat-any?)
                                               (i . < . (sub1 base-count)))
                                           (elem-desc-ref func-desc i)))
                                    scheme-types
                                    inv-kind
                                    args))

(define (build-method-arguments-from-desc count get-elem-desc scheme-types inv-kind args)
  (define vars (if (zero? count)
                   #f
                   (malloc count _VARIANTARG 'raw)))
  (define cleanup (box (if vars
                           (list (lambda () (free vars)))
                           null)))
  (define commit (box null))
  (parameterize ([current-cleanup cleanup]
                 [current-commit commit])
    (for ([i (in-range count)]
          [a (in-sequences (in-list args)
                           (in-cycle (list com-omit)))]
          [scheme-type (in-list scheme-types)])
      (define var (ptr-ref vars _VARIANT (- count i 1))) ; reverse order
      (VariantInit var)
      (scheme-to-variant! var 
                          a 
                          (get-elem-desc i)
                          scheme-type)))
  (define disp-params (cast (malloc _DISPPARAMS 'raw)
                            _pointer
                            _DISPPARAMS-pointer))
  (memcpy disp-params
          (make-DISPPARAMS vars 
                           (if (= inv-kind INVOKE_PROPERTYPUT)
                               prop-put-long
                               #f)
                           count 
                           (if (= inv-kind INVOKE_PROPERTYPUT)
                               count
                               0))
          (ctype-sizeof _DISPPARAMS))
  (values count
          disp-params
          (cons (lambda () (free disp-params)) (unbox cleanup))
          (unbox commit)))

(define (build-method-arguments-using-var-desc var-desc scheme-types inv-kind args)
  (build-method-arguments-from-desc (if (= inv-kind INVOKE_PROPERTYPUT)
                                        1
                                        0)
                                    (lambda (i)
                                      (VARDESC-elemdescVar var-desc))
                                    scheme-types
                                    inv-kind
                                    args))

(define (variant-to-scheme var #:mode [mode '(out)])
  (define _t (to-ctype (vt-to-scheme-type (VARIANT-vt var)) #:mode mode))
  (if _t
      (ptr-ref (union-ptr (VARIANT-u var)) _t)
      (void)))

(define (build-method-arguments type-desc scheme-types inv-kind args)
  (cond
   [(not type-desc)
    (build-method-arguments-using-function-desc #f
                                                scheme-types
                                                inv-kind args)]
   [(function-type-desc? type-desc)
    (build-method-arguments-using-function-desc (car (mx-com-type-desc-desc type-desc)) 
                                                scheme-types
                                                inv-kind args)]
   [else
    (build-method-arguments-using-var-desc (mx-com-type-desc-desc type-desc)
                                           scheme-types
                                           inv-kind args)]))

(define (find-memid who obj name)
  (define-values (r memid)
    (GetIDsOfNames (com-object-get-dispatch obj) IID_NULL name LOCALE_SYSTEM_DEFAULT))
  (cond
   [(zero? r) memid]
   [(= r DISP_E_UNKNOWNNAME) (error who "unknown method name: ~e" name)]
   [else
    (windows-error (format "~a: error getting ID of method ~s" who name)
                     r)]))

(define (adjust-any-... args t)
  (define ta (cadr t))
  (define len (length ta))
  (if (and (len . >= . 2)
           ((length args) . >= . (- len 2))
           (eq? '... (list-ref ta (sub1 len)))
           (eq? 'any (list-ref ta (- len 2))))
      ;; Replace `any ...' with the right number of `any's
      `(,(car t) ,(append (take ta (- len 2))
                          (make-list (- (length args) (- len 2)) 'any))
        . ,(cddr t))
      t))

(define (do-com-invoke who obj name args inv-kind)
  (check-com-obj who obj)
  (unless (string? name) (raise-type-error who "string" name))
  (let* ([t (or (do-get-method-type who obj name inv-kind #t)
                ;; wing it by inferring types from the arguments:
                `(-> ,(map arg-to-type args) any))]
         [t (adjust-any-... args t)])
    (unless (<= (for/fold ([n 0]) ([v (in-list (cadr t))])
                  (if (and (pair? v) (eq? (car v) 'opt))
                      (add1 n)
                      n))
                (length args)
                (length (cadr t)))
            (error 'com-invoke "bad argument count for ~s" name))
    (for ([arg (in-list args)]
          [type (in-list (cadr t))])
      (check-argument 'com-invoke name arg type))
    (call-as-atomic
     (lambda ()
       (define type-desc (get-method-type obj name inv-kind #f)) ; cached
       (cond
        [(if type-desc
             (mx-com-type-desc-memid type-desc)
             (find-memid who obj name))
         => (lambda (memid)
              (define-values (num-params-passed method-arguments arg-cleanups commits)
                (build-method-arguments type-desc
                                        (cadr t)
                                        inv-kind
                                        args))
              ;; from this point, don't escape/return without running cleanups
              (when #f
                ;; for debugging, inspect constructed arguments:
                (eprintf "~e ~e\n" 
                         t
                         (reverse
                          (for/list ([i (in-range num-params-passed)])
                            (variant-to-scheme (ptr-ref (DISPPARAMS-rgvarg method-arguments) 
                                                        _VARIANT 
                                                        i)
                                               #:mode '())))))
              (define exn-info-ptr (malloc 'atomic-interior _EXCEPINFO))
              (define-values (method-result cleanups)
                (if (= inv-kind INVOKE_PROPERTYPUT)
                    (values #f arg-cleanups)
                    (let ([r (make-a-VARIANT 'raw)])
                      (values r (cons (lambda () (free r))
                                      arg-cleanups)))))
              (for ([proc (in-list commits)]) (proc))
              (define hr
                ;; Note that all arguments to `Invoke' should
                ;; not be movable by a GC. A call to `Invoke'
                ;; may use the Windows message queue, and other
                ;; libraries (notably `racket/gui') may have
                ;; callbacks triggered via messages.
                (Invoke (com-object-get-dispatch obj)
                        memid IID_NULL LOCALE_SYSTEM_DEFAULT
                        inv-kind method-arguments
                        method-result
                        exn-info-ptr error-index-ptr))
              (cond
               [(zero? hr)
                (begin0
                 (if method-result
                     (variant-to-scheme method-result)
                     (void))
                 (for ([proc (in-list cleanups)]) (proc)))]
               [(= hr DISP_E_EXCEPTION)
                (for ([proc (in-list cleanups)]) (proc))
                (define exn-info (cast exn-info-ptr _pointer _EXCEPINFO-pointer))
                (define has-error-code? (positive? (EXCEPINFO-wCode exn-info)))
                (define desc (EXCEPINFO-bstrDescription exn-info))
                (windows-error
                 (if has-error-code?
                     (format "COM object exception during ~s, error code 0x~x~a~a"
                             name
                             (EXCEPINFO-wCode exn-info)
                             (if desc "\nDescription: " "")
                             (or desc ""))
                     (format "COM object exception during ~s~a~a"
                             name
                             (if desc "\nDescription: " "")
                             (or desc "")))
                 (EXCEPINFO-scode exn-info))]
               [else
                (for ([proc (in-list cleanups)]) (proc))
                (windows-error (format "~a: failed for ~s" who name) hr)]))]
        [else (error "not yet implemented")])))))

(define (com-invoke obj name . args)
  (do-com-invoke 'com-invoke obj name args INVOKE_FUNC))

(define (follow-chain who obj names len)
  (for ([s (in-list names)]
        [i (in-range len)])
    (unless (string? s) (raise-type-error who "string" s)))
  (define-values (target-obj release?)
    (for/fold ([obj obj] [release? #f]) ([i (in-range (sub1 len))]
                                         [s (in-list names)])
      (define new-obj (com-get-property obj s))
      (when release?
        (com-release obj))
      (unless (com-object? new-obj)
        (error who "result for ~s not a com-object: ~e" s new-obj))
      (values new-obj #t)))
  target-obj)

(define com-get-property
  (case-lambda
   [(obj name)
    (do-com-invoke 'com-get-property obj name null INVOKE_PROPERTYGET)]
   [(obj name1 . more-names)
    (check-com-obj 'com-get-property obj)
    (define names (cons name1 more-names))
    (define len (length names))
    (define target-obj (follow-chain 'com-get-property obj names len))
    (begin0
     (com-get-property target-obj (list-ref names (sub1 len)))
     (com-release target-obj))]))

(define (com-get-property* obj name . args)
  (do-com-invoke 'com-get-property obj name args INVOKE_PROPERTYGET))

(define com-set-property!
  (case-lambda
   [(obj name val)
    (do-com-invoke 'com-set-property! obj name (list val) INVOKE_PROPERTYPUT)]
   [(obj name1 name2 . names+val)
    (check-com-obj 'com-set-property obj)
    (define names (list* name1 name2 names+val))
    (define len (sub1 (length names)))
    (define val (list-ref names len))
    (define target-obj (follow-chain 'com-set-property! obj names len))
    (begin0
     (com-set-property! target-obj (list-ref names (sub1 len)) val)
     (com-release target-obj))]))

;; ----------------------------------------
;; COM event executor

(struct com-event-executor (t ch)
        #:property prop:evt (lambda (self)
                              (guard-evt
                               (lambda ()
                                 (thread-resume (com-event-executor-t self)
                                                (current-thread))
                                 (wrap-evt 
                                  (com-event-executor-ch self)
                                  (lambda (v)
                                    (lambda ()
                                      (apply (car v) (cdr v)))))))))

(define (com-make-event-executor)
  (define ch (make-channel))
  (define t (thread/suspend-to-kill
             (lambda ()
               (let loop ()
                 (channel-put ch (thread-receive))
                 (loop)))))
  (com-event-executor t ch))

;; ----------------------------------------
;; COM event handlers

(define-runtime-path myssink-dll '(so "myssink.dll"))

(define CLSID_Sink
  ;; "myssink.dll":
  (string->clsid "{DA064DCD-0881-11D3-B5CA-0060089002FF}"))

(define IID_ISink
  (string->clsid "{DA064DCC-0881-11D3-B5CA-0060089002FF}"))

(define-com-interface (_ISink _IDispatch)
  ([set_myssink_table (_hmfun _pointer -> set_myssink_table (void))]
   [register_handler (_hmfun _DISPID _pointer -> register_handler (void))]
   [unregister_handler (_hmfun _DISPID -> unregister_handler (void))]))

(define-syntax-rule (_sfun type ...)
  (_fun #:atomic? #t #:async-apply (lambda (f) (f)) type ...))

(define-cstruct _MYSSINK_TABLE
  ([psink_release_handler (_sfun _pointer -> _void)]
   [psink_release_arg (_sfun _pointer -> _void)]
   [psink_apply (_sfun _pointer _int _pointer -> _void)]
   [psink_variant_to_scheme (_sfun _VARIANTARG-pointer -> _pointer)]
   [psink_unmarshal_scheme (_sfun _pointer _VARIANTARG-pointer -> _void)]
   [pmake_scode (_sfun _SCODE -> _pointer)]))

(define (sink-release-handler h)
  (free-immobile-cell h))

(define (sink-release-arg a)
  (free-immobile-cell a))

(define (sink-apply f-in argc argv)
  (define f (ptr-ref f-in _racket))
  (thread-send (com-event-executor-t (car f))
               (cons (cdr f)
                     (for/list ([i (in-range argc)])
                       (ptr-ref (ptr-ref argv _pointer i) _racket)))))

(define (sink-variant-to-scheme var)
  (malloc-immobile-cell (variant-to-scheme var #:mode '(in add-ref))))

(define (sink-unmarshal-scheme p var)
  (define a (ptr-ref p _racket))
  (scheme-to-variant! var a #f (arg-to-type a)))

(define (sink-make-scode v)
  (malloc-immobile-cell v))

(define myssink-table (cast (malloc _MYSSINK_TABLE 'atomic-interior)
                            _pointer
                            (_gcable _MYSSINK_TABLE-pointer)))
(define sink-table-links 
  ;; used to ensure that everything is retained long enough:
  (list myssink-table
        sink-release-handler
        sink-release-arg
        sink-apply
        sink-variant-to-scheme
        sink-unmarshal-scheme
        sink-make-scode))
(memcpy myssink-table
        (apply make-MYSSINK_TABLE (cdr sink-table-links))
        (ctype-sizeof _MYSSINK_TABLE))

(define (connect-com-object-to-event-sink obj)
  (or (com-object-connection-point obj)
      (let ([dispatch (com-object-get-dispatch obj)])
        (define connection-point-container
          (QueryInterface dispatch IID_IConnectionPointContainer _IConnectionPointContainer-pointer))
        (define type-info (event-type-info-from-com-object obj))
        (define type-attr (GetTypeAttr type-info))
        (define connection-point (FindConnectionPoint connection-point-container
                                                      (TYPEATTR-guid type-attr)))
        (ReleaseTypeAttr type-info type-attr)
        ;; emulate CoCreateInstance on athe myssink DLL, which avoids the
        ;; need for registration:
        (define myssink-lib (ffi-lib myssink-dll))
        (define myssink-DllGetClassObject 
          (get-ffi-obj 'DllGetClassObject myssink-lib 
                       (_hfun _GUID-pointer _GUID-pointer 
                              (u : (_ptr o _IClassFactory-pointer/null))
                              -> DllGetClassObject u)))
        (define sink-factory
          (myssink-DllGetClassObject CLSID_Sink IID_IClassFactory))
        (define sink-unknown
          ;; This primitive method doesn't AddRef the object, 
          ;; so don't Release it:
          (CreateInstance/factory sink-factory #f CLSID_Sink))
        (define sink (QueryInterface sink-unknown IID_ISink _ISink-pointer))
        (set_myssink_table sink myssink-table)
        (define cookie (Advise connection-point sink))
        (define impl (com-object-impl obj))
        (set-com-impl-connection-point! impl connection-point)
        (set-com-impl-connection-cookie! impl cookie)
        (set-com-impl-sink! impl sink)
        (set-com-impl-sink-table-links! impl sink-table-links)
        (Release connection-point-container)
        connection-point)))

(define (do-register-event-callback who obj name proc? proc executor)
  (check-com-obj who obj)
  (unless (string? name) (raise-type-error who "string" name))
  (when proc?
    (unless (procedure? proc) (raise-type-error who "procedure" proc))
    (unless (com-event-executor? executor)
      (raise-type-error who "com-event-executor" executor)))
  (when (or proc? (com-object-sink obj))
    (define connection-point (connect-com-object-to-event-sink obj))
    (define type-info (event-type-info-from-com-object obj))
    (define sink (com-object-sink obj))
    (define func-desc (get-func-desc-for-event name type-info))
    (unless func-desc
      (error who "event not found: ~e" name))
    (if proc
        (register_handler sink (FUNCDESC-memid func-desc) (malloc-immobile-cell (cons executor proc)))
        (unregister_handler sink (FUNCDESC-memid func-desc)))
    (ReleaseFuncDesc type-info func-desc)))

(define (com-register-event-callback obj name proc executor)
  (do-register-event-callback 'com-register-event-callback obj name #t proc executor))

(define (com-unregister-event-callback obj name)
  (do-register-event-callback 'com-unregister-event-callback obj name #f #f #f))

;; ----------------------------------------
;; Extract raw interface pointers

(define (com-object-get-iunknown obj)
  (check-com-obj 'com-object-get-iunknown obj)
  (call-as-atomic
   (lambda ()
     (com-object-get-unknown obj))))

(define (com-object-get-idispatch obj)
  (check-com-obj 'com-object-get-idispatch obj)
  (call-as-atomic
   (lambda ()
     (com-object-get-dispatch obj))))

(define (com-iunknown? v) (and (IUnknown? v) #t))
(define (com-idispatch? v) (and (IDispatch? v) #t))

;; ----------------------------------------
;; Initialize

(define-ole CoInitialize (_wfun (_pointer = #f) -> (r : _HRESULT)
                                -> (cond
                                    [(= r 0) (void)] ; ok
                                    [(= r 1) (void)] ; already initialized
                                    [else (windows-error (format "~a: failed" 'CoInitialize) r)])))

(define inited? #f)
(define (init!)
  (unless inited?
    (CoInitialize)
    (set! inited? #t)))
