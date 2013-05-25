#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out file-creator-and-type))

(define coreserv-lib (ffi-lib (format "/System/Library/Frameworks/CoreServices.framework/CoreServices")))

(define-ffi-definer define-coreserv coreserv-lib)

(define kFSCatInfoFinderInfo #x00000800)
(define _FSCatalogInfoBitmap _uint32)

(define _FSVolumeRefNum _int16)

(define-cstruct _UTCDateTime
  ([highSeconds _uint16]
   [lowSeconds _uint32]
   [fraction _uint16])
  #:alignment 2)

(define-cstruct _Point
  ([v _short]
   [h _short]))

(define _OSType _uint32)

(define-cstruct _FileInfo
  ([fileType _OSType]
   [fileCreator _OSType]
   [finderFlags _uint16]
   [location _Point]
   [reservedField _uint16])
  #:alignment 2)

(define-cstruct _FSPermissionInfo
  ([userID _uint32]
   [groupID _uint32]
   [word _uint32]
   [fileSec _pointer])
  #:alignment 2)

(define-cstruct _FSCatalogInfo
  ([nodeFlags _uint16]
   [volume _FSVolumeRefNum]
   [parentDirID _uint32]
   [nodeID _uint32]
   [sharingFlags _uint8]
   [userPrivileges _uint8]
   [reserved1 _uint8]
   [reserved2 _uint8]
   [createDate _UTCDateTime]
   [contentModDate _UTCDateTime]
   [attributeModDate _UTCDateTime]
   [accessDate _UTCDateTime]
   [backupDate _UTCDateTime]
   [permissions _FSPermissionInfo]
   [finderInfo _FileInfo]
   ;; .... 144 or 148 bytes total
   )
  #:alignment 2)

(define _FSRef _pointer) ; 80 bytes

(define-coreserv FSPathMakeRef (_fun _path _FSRef (_pointer = #f) -> _OSStatus))

(define-coreserv FSGetCatalogInfo
  (_fun _FSRef
        _FSCatalogInfoBitmap 
        _FSCatalogInfo-pointer
        _pointer ; outname, #f is ok
        _pointer ; fsSpec, #f is ok
        _pointer ; parentRef, #f is ok
        -> _OSStatus))

(define-coreserv FSSetCatalogInfo
  (_fun _FSRef
        _FSCatalogInfoBitmap 
        _FSCatalogInfo-pointer
        -> _OSStatus))

(define (path->fsref s)
  (let ([fs (malloc 80)])
    (let ([r (FSPathMakeRef s fs)])
      (unless (zero? r)
        (filesystem-error 'file-creator-and-type "could not access file (~a): ~v"
                          r
                          s)))
    fs))

(define (int->str v)
  (bytes (arithmetic-shift (bitwise-and v #xFF000000) -24)
         (arithmetic-shift (bitwise-and v #xFF0000) -16)
         (arithmetic-shift (bitwise-and v #xFF00) -8)
         (bitwise-and v #xFF)))

(define (str->int v)
  (bitwise-ior (arithmetic-shift (bytes-ref v 0) 24)
               (arithmetic-shift (bytes-ref v 1) 16)
               (arithmetic-shift (bytes-ref v 2) 8)
               (bytes-ref v 3)))

(define (get-info v fs path)
  (let ([r (FSGetCatalogInfo fs
                             kFSCatInfoFinderInfo
                             v
                             #f #f #f)])
    (unless (zero? r)
      (filesystem-error 'file-creator-and-type "lookup failed (~a): ~e"
                        r
                        path))))

(define file-creator-and-type
  (case-lambda
   [(path)
    (unless (path-string? path)
      (raise-type-error 'file-creator-and-type "path string" path))
    (let ([info (let ([fs (path->fsref path)]
                      [v (cast (malloc 256) _gcpointer (_gcable _FSCatalogInfo-pointer))])
                  (get-info v fs path)
                  (FSCatalogInfo-finderInfo v))])
      (values (int->str (FileInfo-fileCreator info))
              (int->str (FileInfo-fileType info))))]
   [(path creator type)
    (unless (path-string? path)
      (raise-type-error 'file-creator-and-type "path string" path))
    (unless (and (bytes? creator) (= 4 (bytes-length creator)))
      (raise-type-error 'file-creator-and-type "bytes string of length 4" creator))
    (unless (and (bytes? type) (= 4 (bytes-length type)))
      (raise-type-error 'file-creator-and-type "bytes string of length 4" type))
    (let ([fs (path->fsref path)]
          [v (cast (malloc 256) _gcpointer (_gcable _FSCatalogInfo-pointer))])
      (get-info v fs path)
      (let ([info (FSCatalogInfo-finderInfo v)])
        (set-FileInfo-fileCreator! info (str->int creator))
        (set-FileInfo-fileType! info (str->int type)))
      (let ([r (FSSetCatalogInfo fs
                                 kFSCatInfoFinderInfo
                                 v)])
        (unless (zero? r)
          (filesystem-error 'file-creator-and-type "change failed (~a): ~e"
                            r
                            path))))
    (void)]))


(define (filesystem-error sym fmt . args)
  (raise (exn:fail:filesystem
          (string-append (format "~a: " sym)
                         (apply format fmt args))
          (current-continuation-marks))))
