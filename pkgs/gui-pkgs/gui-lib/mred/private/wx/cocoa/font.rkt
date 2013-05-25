#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         "../../lock.rkt"
         "const.rkt"
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out font->NSFont))

(import-class NSFont NSFontManager)

(define NSItalicFontMask #x00000001)
(define NSBoldFontMask #x00000002)

(define (font->NSFont f)
  (let* ([weight (send f get-weight)]
         [style (send f get-style)]
         [name (or (send f get-face)
                   (send the-font-name-directory
                         get-screen-name
                         (send the-font-name-directory
                               find-family-default-font-id
                               (send f get-family))
                         weight
                         style))]
         [name (regexp-replace #rx",.*" name "")])
    (atomically
     (with-autorelease
      (let ([f (tell NSFont 
                     fontWithName: #:type _NSString name
                     size: #:type _CGFloat (send f get-point-size))])
        (if (and (eq? 'normal weight)
                 (eq? 'normal style))
            (begin
              (retain f)
              f)
            (let ([fm (tell NSFontManager sharedFontManager)])
              (let ([f (tell fm
                             convertFont: f 
                             toHaveTrait: #:type _int (bitwise-ior
                                                       (if (eq? weight 'bold) NSBoldFontMask 0)
                                                       (if (eq? style 'italic) NSItalicFontMask 0)))])
                (begin
                  (retain f)
                  f)))))))))
