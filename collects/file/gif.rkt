#|
/******************************************************************************
 *   "Gif-Lib" - Yet another gif library.
 *
 * Written by:  Gershon Elber                Ver 1.1, Aug. 1990
 ******************************************************************************
 * The kernel of the GIF Encoding process can be found here.
 ******************************************************************************
 * History:
 * 14 Jun 89 - Version 1.0 by Gershon Elber.
 *  3 Sep 90 - Version 1.1 by Gershon Elber (Support for Gif89, Unique names).
 * 26 Jun 96 - Version 3.0 by Eric S. Raymond (Full GIF89 support)
 *  5 Jan 07 - Ported to MzScheme by Matthew
 *****************************************************************************/
|#

#lang at-exp racket/base

(require racket/contract
         scribble/srcdoc
         (prefix-in octree: file/private/octree-quantize)
         (for-doc racket/base
                  scribble/manual))

(define LZ_MAX_CODE 4095)

(define GifVersionPrefix #"GIF89a")

(provide
 (proc-doc/names gif-stream?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is a GIF stream created by
                           @racket[gif-write], @racket[#f] otherwise.})
 
 (proc-doc/names image-ready-gif-stream?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is a GIF stream that is not in
                           @racket['done] mode, @racket[#f] otherwise.})
 (proc-doc/names image-or-control-ready-gif-stream?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is a GIF stream that is in
                           @racket['init] or @racket['image-or-control] mode, @racket[#f]
                           otherwise.})
 (proc-doc/names empty-gif-stream?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is a GIF stream that in
                           @racket['init] mode, @racket[#f] otherwise.})
 (proc-doc/names gif-colormap?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] represets a colormap,
                           @racket[#f] otherwise.  A colormap is a list whose size is a power
                           of @math{2} between @math{2^1} and @math{2^8}, and whose elements
                           are vectors of size 3 containing colors (i.e., exact integers
                                                                          between @math{0} and @math{255} inclusive).})
 (proc-doc/names color?
                 (-> any/c boolean?)
                 (v)
                 @{The same as @racket[byte?].})
 (proc-doc/names dimension?
                 (-> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is an exact integer between
                           @racket[#x0] and @racket[#xFFFF] inclusive, @racket[#f]
                           otherwise.}))

(define-struct gif-stream
  (port SWidth SHeight SBackGroundColor SColorMap [FileState #:mutable]))

(define (image-ready-gif-stream? g)
  (and (gif-stream? g)
       (memq (gif-stream-FileState g) '(init image-or-control image))))

(define (image-or-control-ready-gif-stream? g)
  (and (gif-stream? g)
       (memq (gif-stream-FileState g) '(init image-or-control))))

(define (empty-gif-stream? g)
  (and (gif-stream? g)
       (memq (gif-stream-FileState g) '(init))))

(define color? byte?)
(define (dimension? x) (and (exact? x) (integer? x) (<= 0 x #xFFFF)))

(define (gif-colormap? l)
  (and (list? l)
       (member (length l) '(2 4 8 16 32 64 128 256))
       (andmap (lambda (c)
                 (and (vector? c)
                      (= 3 (vector-length c))
                      (color? (vector-ref c 0))
                      (color? (vector-ref c 1))
                      (color? (vector-ref c 2))))
               l)))

(define (color-map->bytes ColorMap)
  (apply bytes (apply append (map vector->list ColorMap))))

(define (bits-per-pixel ColorMap)
  (case (length ColorMap)
    [(2) 1] [(4) 2] [(8) 3] [(16) 4] [(32) 5] [(64) 6] [(128) 7] [(256) 8]
    [else (error 'bits-per-pixel
                 "strange colormap size: ~e"
                 (length ColorMap))]))

(define (WRITE g bytes)
  (write-bytes bytes (gif-stream-port g)))

(provide/doc (proc-doc/names gif-state
                             (-> gif-stream? symbol?)
                             (stream)
                             @{Returns the state of @racket[stream].}))
(define (gif-state GifFile)
  (gif-stream-FileState GifFile))

;;/******************************************************************************
;; * This routine should be called before any other EGif calls, immediately
;; * follows the GIF file openning.
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-start
              (-> output-port?
                  dimension?
                  dimension?
                  color?
                  (or/c gif-colormap? #f)
                  gif-stream?)
              (out w h bg-color cmap)
              @{Writes the start of a GIF file to the given output port, and
                returns a GIF stream that adds to the output port.

                The width and height determine a virtual space for the overall
                GIF image.  Individual images added to the GIF stream must fit
                within this virtual space.  The space is initialized by the
                given background color.

                Finally, the default meaning of color numbers (such as the
                background color) is determined by the given colormap, but
                individual images within the GIF file can have their own
                colormaps.

                A global colormap need not be supplied, in which case a
                colormap must be supplied for each image. Beware that the
                bg-color is ill-defined if a global colormap is not
                provided.}))
(define (gif-start port Width Height BackGround ColorMap)
  (define GifFile
    (make-gif-stream port Width Height BackGround ColorMap 'init))

  (when ColorMap
    (unless (BackGround . < . (length ColorMap))
      (error 'gif-start
             "background color ~a is out of range for the color map: ~e"
             BackGround
             ColorMap)))

  (WRITE GifFile GifVersionPrefix)

  ;;/*
  ;; * Put the logical screen descriptor into the file:
  ;; */
  ;;/* Logical Screen Descriptor: Dimensions */
  (EGifPutWord Width GifFile)
  (EGifPutWord Height GifFile)

  ;;/* Logical Screen Descriptor: Packed Fields */
  ;;/* Note: We have actual size of the color table default to the largest
  ;; * possible size (7+1 == 8 bits) because the decoder can use it to decide
  ;; * how to display the files.
  ;; */
  (WRITE GifFile
         (bytes (bitwise-ior
                 (if ColorMap #x80 #x00)
                 (arithmetic-shift #x7 4) ; /* Bits originally allocated to each primary color */
                 (if ColorMap (sub1 (bits-per-pixel ColorMap)) #x07)) ; /* Actual size of the color table. */
                BackGround ; /* Index into the ColorTable for background color */
                0)) ; /* Pixel Aspect Ratio */

  ;; /* If we have Global color map - dump it also: */
  (when ColorMap
    (WRITE GifFile (color-map->bytes ColorMap)))

  GifFile)

;;/******************************************************************************
;; * This routine should be called before any attempt to dump an image - any
;; * call to any of the pixel dump routines.
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-add-image
              (-> image-ready-gif-stream?
                  dimension?
                  dimension?
                  dimension?
                  dimension?
                  any/c
                  (or/c gif-colormap? #f)
                  bytes?
                  void?)
              (stream left top width height interlaced? cmap bstr)
              @{Writes an image to the given GIF stream. The @racket[left],
                @racket[top], @racket[width], and @racket[height] values
                specify the location and size of the image within the overall
                GIF image's virtual space.

                If @racket[interlaced?] is true, then @racket[bstr] should
                provide bytes ininterlaced order instead of top-to-bottom
                order. Interlaced order is:

                @(itemize @item{every 8th row, starting with 0}
                          @item{every 8th row, starting with 4}
                          @item{every 4th row, starting with 2}
                          @item{every 2nd row, starting with 1})

                If a global color is provided with @racket[gif-start], a
                @racket[#f] value can be provided for @racket[cmap].

                The @racket[bstr] argument specifies the pixel content of the
                image.  Each byte specifies a color (i.e., an index in the
                colormap).  Each row is provided left-to-right, and the rows
                provided either top-to-bottom or in interlaced order (see
                above).  If the image is prefixed with a control that specifies
                an transparent index (see @racket[gif-add-control]), then the
                corresponding ``color'' doesn't draw into the overall GIF
                image.

                An exception is raised if any byte value in @racket[bstr] is
                larger than the colormap's length, if the @racket[bstr] length
                is not @racket[width] times @racket[height], or if the
                @racket[top], @racket[left], @racket[width], and
                @racket[height] dimensions specify a region beyond the overall
                GIF image's virtual space.}))
(define (gif-add-image GifFile Left Top Width Height Interlace ColorMap Line)

  (unless ColorMap
    (unless (gif-stream-SColorMap GifFile)
      (error 'gif-add-image
             "no color map for image")))

  (unless (= (bytes-length Line)
             (* Width Height))
    (error 'gif-add-image
           "bytes string size doesn't match width times height: ~e"
           Line))

  (unless (and ((+ Left Width) . <= . (gif-stream-SWidth GifFile))
               ((+ Top Height) . <= . (gif-stream-SHeight GifFile)))
    (error 'gif-add-image
           "image extends beyond GIF virtual screen"))

  (WRITE GifFile #",") ; /* Image seperator character. */
  (EGifPutWord Left GifFile)
  (EGifPutWord Top GifFile)
  (EGifPutWord Width GifFile)
  (EGifPutWord Height GifFile)
  (WRITE GifFile
         (bytes
          (bitwise-ior
           (if ColorMap #x80 #x00)
           (if Interlace #x40 #x00)
           (if ColorMap (sub1 (bits-per-pixel ColorMap)) #x00))))

  ;; /* If we have local color map - dump it also: */
  (when ColorMap
    (WRITE GifFile (color-map->bytes ColorMap)))

  (let* ([cmap (or ColorMap (gif-stream-SColorMap GifFile))])

    (check-line-bytes (length cmap) Line)

    (EGifCompress GifFile
                  (max 2 ;; min code size of LZW is 2
                       (bits-per-pixel cmap))
                  Line))

  (set-gif-stream-FileState! GifFile 'image-or-control))

(define (check-line-bytes sz Line)
  (let loop ([i (bytes-length Line)])
    (unless (zero? i)
      (let ([i (sub1 i)])
        (unless ((bytes-ref Line i) . < . sz)
          (error 'gif-add-image
                 "out-of-range color index ~a in line: ~e"
                 (bytes-ref Line i)
                 Line))
        (loop i)))))


;;/******************************************************************************
;; * This routine should be called to add graphic control before the next image
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-add-control
              (-> image-or-control-ready-gif-stream?
                  (or/c 'any 'keep 'restore-bg 'restore-prev)
                  any/c
                  dimension?
                  (or/c color? #f)
                  void?)
              (stream disposal wait-for-input? delay transparent)
              @{Writes an image-control command to a GIF stream. Such a control
                must appear just before an image, and it applies to the
                following image.

                The GIF image model involves processing images one by one,
                placing each image into the specified position within the
                overall image's virtual space. An image-control command can
                specify a delay before an image is added (to create animated
                GIFs), and it also specifies how the image should be kept or
                removed from the overall image before proceeding to the next
                one (also for GIF animation).

                The @racket[disposal] argument specifies how to proceed:

                @(itemize @item{@racket['any] : doesn't matter (perhaps because
                                the next image completely overwrites the
                                current one)}
                          @item{@racket['keep] : leave the image in place}
                          @item{@racket['restore-bg] : replace the image with
                                the background color}
                          @item{@racket['restore-prev] : restore the overall
                                image content to the content before the image
                                is added})

                If @racket[wait-for-input?] is true, then the display program
                may wait for some cue from the user (perhaps a mouse click)
                before adding the image.

                The @racket[delay] argument specifies a delay in 1/100s of a
                second.

                If the @racket[transparent] argument is a color, then it
                determines an index that is used to represent transparent
                pixels in the follow image (as opposed to the color specified
                by the colormap for the index).

                An exception is raised if a control is already added to
                @racket[stream] without a corresponding image.}))

(define (gif-add-control GifFile
                         Disposal
                         UserInput?
                         Delay ; 1/100s of a second
                         TransparentColor)
  (WRITE GifFile #"\x21\xF9\x04")
  (WRITE GifFile
         (bytes (bitwise-ior
                 (case Disposal
                   [(any) 0]
                   [(keep) #x4]
                   [(restore-bg) #x8]
                   [(restore-prev) #xC])
                 (if UserInput? #x2 0)
                 (if TransparentColor #x1 0))))
  (EGifPutWord Delay GifFile)
  (WRITE GifFile (bytes
                  (or TransparentColor 0)
                  0)) ; block terminator
  (set-gif-stream-FileState! GifFile 'image))

;;/******************************************************************************
;; * This routine should be called to add the "loop" graphic control
;;   before adding any images
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-add-loop-control
              (-> empty-gif-stream? dimension? void?)
              (stream iteration)
              @{Writes a control command to a GIF stream for which no images or
                other commands have already been written. The command causes
                the animating sequence of images in the GIF to be repeated
                `iteration-dimension' times, where 0 can be used to mean
                ``infinity.''

                An exception is raise if some control or image has been added
                to the stream already.}))
(define (gif-add-loop-control GifFile
                              Iterations)
  (WRITE GifFile #"\x21\xFF\x0BNETSCAPE2.0\x03\x01")
  (EGifPutWord Iterations GifFile)
  (WRITE GifFile #"\x00")
  (set-gif-stream-FileState! GifFile 'image-or-control))

;;/******************************************************************************
;; * This routine should be called to add arbitrary comment text
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-add-comment
              (-> image-or-control-ready-gif-stream?
                  bytes?
                  void?)
              (stream bstr)
              @{Adds a generic comment to the GIF stream.

                An exception is raised if an image-control command was just
                written to the stream (so that an image is required next).}))
(define (gif-add-comment GifFile Str)
  (WRITE GifFile #"\x21\xFE")
  (let loop ([pos 0])
    (when (pos . < . (bytes-length Str))
      (let ([amt (min 255 (- (bytes-length Str) pos))])
        (write-byte amt (gif-stream-port GifFile))
        (write-bytes Str (gif-stream-port GifFile) pos (+ pos amt))
        (loop (+ pos amt)))))
  (WRITE GifFile #"\0")
  (set-gif-stream-FileState! GifFile 'image-or-control))

;;/******************************************************************************
;; * This routine should be called last, to end GIF file.
;; *****************************************************************************/
(provide/doc (proc-doc/names
              gif-end
              (-> image-or-control-ready-gif-stream?
                  void?)
              (stream)
              @{Finishes writing a GIF file. The GIF stream's output port is
                not automatically closed.

                An exception is raised if an image-control command was just
                written to the stream (so that an image is required next).}))
(define (gif-end GifFile)
  (WRITE GifFile #";")
  (set-gif-stream-FileState! GifFile 'done))

;;/******************************************************************************
;; * Put 2 bytes (word) into the given file:
;; *****************************************************************************/
(define (EGifPutWord Word GifFile)
  (WRITE GifFile (integer->integer-bytes Word 2 #f #f)))

;;/******************************************************************************
;; * LZ compression output
;; *****************************************************************************/
(define (write-buffered-byte b buf port)
  (let ([cnt (add1 (bytes-ref buf 0))])
    (bytes-set! buf cnt b)
    (bytes-set! buf 0 cnt)
    (when (= cnt 255)
      (write-bytes buf port)
      (bytes-set! buf 0 0))))

(define (CompressOutput port buf Code
                        RunningBits RunningCode MaxCode1
                        CrntShiftState CrntShiftDWord)
  (let-values ([(CrntShiftState CrntShiftDWord)
                (let loop ([CrntShiftDWord
                            (bitwise-ior CrntShiftDWord
                                         (arithmetic-shift Code CrntShiftState))]
                           [CrntShiftState (+ CrntShiftState RunningBits)])
                  (if (CrntShiftState . >= . 8)
                    (begin
                      (write-buffered-byte (bitwise-and CrntShiftDWord #xff) buf port)
                      (loop (arithmetic-shift CrntShiftDWord -8)
                            (- CrntShiftState 8)))
                    (values CrntShiftState CrntShiftDWord)))])
    (if (and (RunningCode . >= . MaxCode1)
             (Code . <= . 4095))
      (values (add1 RunningBits) (arithmetic-shift 1 (add1 RunningBits))
              CrntShiftState CrntShiftDWord)
      (values RunningBits MaxCode1
              CrntShiftState CrntShiftDWord))))

;;/******************************************************************************
;; * LZ compression
;; *****************************************************************************/
(define (EGifCompress GifFile BitsPerPixel Line)

  (WRITE GifFile (bytes BitsPerPixel))

  (let* ([ClearCode (arithmetic-shift 1 BitsPerPixel)]
         [EOFCode (add1 ClearCode)]
         [RunningCode (add1 EOFCode)]
         [RunningBits (add1 BitsPerPixel)];    /* Number of bits per code. */
         [MaxCode1 (arithmetic-shift 1 RunningBits)];    /* Max. code + 1. */
         [HashTable (make-hasheq)]
         [CrntShiftState 0];    /* No information in CrntShiftDWord. */
         [CrntShiftDWord 0]
         [port (gif-stream-port GifFile)]
         [len (bytes-length Line)]
         [buf (make-bytes 256 0)])

    (let-values ([(RunningBits MaxCode1 CrntShiftState CrntShiftDWord)
                  (CompressOutput port buf ClearCode
                                  RunningBits RunningCode MaxCode1
                                  CrntShiftState CrntShiftDWord)])
      (let loop ([CrntCode (bytes-ref Line 0)]
                 [RunningCode RunningCode]
                 [RunningBits RunningBits]
                 [MaxCode1 MaxCode1]
                 [CrntShiftState CrntShiftState]
                 [CrntShiftDWord CrntShiftDWord]
                 [HashTable HashTable]
                 [i 1])
        (if (= i len)
          ;; Finish:
          (let-values ([(RunningBits MaxCode1 CrntShiftState CrntShiftDWord)
                        (CompressOutput port buf CrntCode
                                        RunningBits RunningCode MaxCode1
                                        CrntShiftState CrntShiftDWord)])
            (let-values ([(RunningBits MaxCode1 CrntShiftState CrntShiftDWord)
                          (CompressOutput port buf EOFCode
                                          RunningBits RunningCode MaxCode1
                                          CrntShiftState CrntShiftDWord)])
              ;; Flush output:
              (let loop ([CrntShiftState CrntShiftState]
                         [CrntShiftDWord CrntShiftDWord])
                (when (CrntShiftState . > . 0)
                  (write-buffered-byte (bitwise-and CrntShiftDWord #xff)
                                       buf port)
                  (loop (arithmetic-shift CrntShiftDWord -8)
                        (- CrntShiftState 8))))
              (unless (zero? (bytes-ref buf 0))
                (write-bytes buf port 0 (add1 (bytes-ref buf 0))))
              (write-bytes #"\0" port)))
          ;; /* Get next pixel from stream. */
          (let ([Pixel (bytes-ref Line i)])
            ;; /* Form a new unique key to search hash table for the code combines 
            ;;  * CrntCode as Prefix string with Pixel as postfix char.
            ;;  */
            (let* ([NewKey (bitwise-ior (arithmetic-shift CrntCode 8) Pixel)]
                   [NewCode (hash-ref HashTable NewKey #f)])
              (if NewCode
                ;;/* This Key is already there, or the string is old one, so
                ;; * simple take new code as our CrntCode:
                ;; */
                (loop NewCode
                      RunningCode RunningBits MaxCode1
                      CrntShiftState CrntShiftDWord
                      HashTable (add1 i))
                ;;/* Put it in hash table, output the prefix code, and make our
                ;; * CrntCode equal to Pixel. */
                (let-values ([(RunningBits MaxCode1 CrntShiftState CrntShiftDWord)
                              (CompressOutput port buf CrntCode
                                              RunningBits RunningCode MaxCode1
                                              CrntShiftState CrntShiftDWord)])
                  (let ([CrntCode Pixel])
                    ;; /* If however the HashTable if full, we send a clear first and
                    ;;  * Clear the hash table. */
                    (if (RunningCode . >= . LZ_MAX_CODE)
                      ;; /* Time to do some clearance: */
                      (let-values ([(RunningBits MaxCode1 CrntShiftState CrntShiftDWord)
                                    (CompressOutput port buf ClearCode
                                                    RunningBits RunningCode MaxCode1
                                                    CrntShiftState CrntShiftDWord)])
                        (loop CrntCode
                              (add1 EOFCode) (add1 BitsPerPixel) (arithmetic-shift 1 (add1 BitsPerPixel))
                              CrntShiftState CrntShiftDWord
                              (make-hasheq) (add1 i)))
                      ;; /* Put this unique key with its relative Code in hash table: */
                      (begin
                        (hash-set! HashTable NewKey RunningCode)
                        (loop CrntCode
                              (add1 RunningCode) RunningBits MaxCode1
                              CrntShiftState CrntShiftDWord
                              HashTable (add1 i))))))))))))))

;;/******************************************************************************
;; * color quantization utility
;; *****************************************************************************/

(provide/doc
 (proc-doc/names
  quantize
  (-> (and/c bytes?
             (lambda (bstr)
               (zero? (remainder (bytes-length bstr) 4))))
      (values bytes? gif-colormap? (or/c color? #f)))
  (bstr)
  @{Each image in a GIF stream is limited to 256 colors, including the
    transparent ``color,'' if any. The @racket[quantize] function
    converts a 24-bit image (plus alpha channel) into an
    indexed-color image, reducing the number of colors if necessary.
    
    Given a set of pixels expressed in ARGB format (i.e., each four
    bytes is a set of values for one pixel: alpha, red, blue, and
    green), @racket[quantize] produces produces
    
    @(itemize @item{bytes for the image (i.e., a array of colors,
                    expressed as a byte string)}
              @item{a colormap}
              @item{either @racket[#f] or a color index for the
                           transparent ``color''})
    
    The conversion treats alpha values less than 128 as transparent
    pixels, and other alpha values as solid.
    
    The quantization process uses Octrees @cite["Gervautz1990"] to construct an adaptive
    palette for all (non-transparent) colors in the image.  This implementation is
    based on an article by Dean Clark @cite["Clark1996"]. 
    
    To convert a collection of images all with the same quantization,
    simply append them for the input of a single call of
    @racket[quantize], and then break apart the result bytes.}))
(define (quantize argb)
  (octree:quantize argb))
