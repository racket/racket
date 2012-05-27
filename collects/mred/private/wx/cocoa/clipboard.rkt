#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt"
         "image.rkt"
         racket/draw/unsafe/bstr
          "../../syntax.rkt"
          "../../lock.rkt")

(provide 
 (protect-out clipboard-driver%
              has-x-selection?))

(import-class NSPasteboard NSArray NSData NSImage NSGraphicsContext)
(import-protocol NSPasteboardOwner)

(define (has-x-selection?) #f)

(define (map-type s)
  (cond
   [(string=? s "TEXT") "public.utf8-plain-text"]
   [else (string-append "org.racket-lang." s)]))

(define (unmap-type s)
  (cond
   [(string=? s "public.utf8-plain-text") "TEXT"]
   [(regexp-match #rx"^org[.]racket-lang[.](.*)$" s)
    => (lambda (m) (cadr m))]
   [else s]))

(defclass clipboard-driver% object%
  (init x-selection?) ; always #f
  (super-new)

  (define client #f)
  (define counter -1)

  (define/public (clear-client)
    ;; called in event-pump thread
    (set! client #f))

  (define/public (get-client) 
    (and client
         (let ([c (tell #:type _NSInteger (tell NSPasteboard generalPasteboard) 
                        changeCount)])
           (if (= c counter)
               client
               (begin
                 (set! client #f)
                 #f)))))

  (define/public (set-client c types)
    (atomically
     (with-autorelease
      (let ([pb (tell NSPasteboard generalPasteboard)]
            [a (tell NSArray arrayWithObjects:
                     #:type (_list i _NSString) (map map-type types)
                     count: #:type _NSUInteger (length types))])
        (set! counter (tell #:type _NSInteger pb 
                            declareTypes: a 
                            owner: #f))
        (set! client c)
        (for ([type (in-list types)])
          (let ([bstr (send c get-data type)])
            (when bstr
              (let* ([bstr (if (string? bstr)
                               (string->bytes/utf-8 bstr)
                               bstr)]
                     [data (tell NSData 
                                 dataWithBytes: #:type _bytes bstr
                                 length: #:type _NSUInteger (bytes-length bstr))])
                (tellv (tell NSPasteboard generalPasteboard)
                       setData: data
                       forType: #:type _NSString (map-type type))))))))))
  
  (define/public (get-data-for-type type)
    (log-error "didn't expect clipboard data request"))

  (define/public (get-text-data)
    (let ([bstr (get-data "TEXT")])
      (or (and bstr
               (bytes->string/utf-8 bstr #\?))
          "")))
  
  (define/public (get-data type)
    (atomically
     (with-autorelease
      (let* ([pb (tell NSPasteboard generalPasteboard)]
             [data (tell pb dataForType: #:type _NSString (map-type type))])
        (and data
             (let ([len (tell #:type _NSUInteger data length)]
                   [bstr (tell #:type _pointer data bytes)])
               (scheme_make_sized_byte_string bstr len 1)))))))

  (define/public (get-bitmap-data)
    (atomically
     (with-autorelease
      (let ([i (tell (tell NSImage alloc) 
                     initWithPasteboard: (tell NSPasteboard generalPasteboard))])
        (and i
             (image->bitmap i))))))

  (define/public (set-bitmap-data bm timestamp)
    (define image (bitmap->image bm))
    (atomically
     (with-autorelease
      (let ([pasteboard (tell NSPasteboard generalPasteboard)])
        (tell pasteboard clearContents)
        (let ([copied-objects (tell NSArray arrayWithObject: image)])
          (tell pasteboard writeObjects: copied-objects)
          (void)))))))
