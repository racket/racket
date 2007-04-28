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

(module gifwrite mzscheme
  (require (lib "contract.ss"))

  (define LZ_MAX_CODE 4095)

  (define GifVersionPrefix #"GIF89a")

  (define-struct gif-stream (port 
                             SWidth
                             SHeight
                             SBackGroundColor
                             SColorMap
                             FileState))

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
  (define (disposal? s)
    (memq s '(any keep restore-bg restore-prev)))

  (define (color-map? l)
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
      [(2) 1]
      [(4) 2]
      [(8) 3]
      [(16) 4]
      [(32) 5]
      [(64) 6]
      [(128) 7]
      [(256) 8]
      [else (error 'bits-per-pixel
                   "strange colormap size: ~e"
                   (length ColorMap))]))

  (define (WRITE g bytes)
    (write-bytes bytes (gif-stream-port g)))

  (provide/contract [gif-state
                     (gif-stream? . -> . symbol?)])
  (define (gif-state GifFile)
    (gif-stream-FileState GifFile))

  ;;/******************************************************************************
  ;; * This routine should be called before any other EGif calls, immediately
  ;; * follows the GIF file openning.
  ;; *****************************************************************************/
  (provide/contract [gif-start
                     (output-port?
                      dimension?
                      dimension?
                      color?
                      (or/c false/c color-map?)
                      . -> . gif-stream?)])
  (define (gif-start port
                     Width
                     Height
                     BackGround
                     ColorMap)
    (define GifFile
      (make-gif-stream port
                       Width
                       Height
                       BackGround
                       ColorMap
                       'init))

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
  (provide/contract [gif-add-image
                     (image-ready-gif-stream?
                      dimension?
                      dimension?
                      dimension?
                      dimension?
                      any/c
                      (or/c false/c color-map?)
                      bytes?
                      . -> . any)])
  (define (gif-add-image GifFile
                        Left
                        Top
                        Width
                        Height
                        Interlace
                        ColorMap
                        Line)

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

    (let* ([cmap (or ColorMap
                     (gif-stream-SColorMap GifFile))])

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
  (provide/contract [gif-add-control
                     (image-or-control-ready-gif-stream?
                      disposal?
                      any/c
                      dimension?
                      (or/c false/c color?)
                      . -> . any)])
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
  (provide/contract [gif-add-loop-control
                     (empty-gif-stream?
                      dimension?
                      . -> . any)])
  (define (gif-add-loop-control GifFile
                                Iterations)
    (WRITE GifFile #"\x21\xFF\x0BNETSCAPE2.0\x03\x01")
    (EGifPutWord Iterations GifFile)
    (WRITE GifFile #"\x00")
    (set-gif-stream-FileState! GifFile 'image-or-control))

  ;;/******************************************************************************
  ;; * This routine should be called to add arbitrary comment text
  ;; *****************************************************************************/
  (provide/contract [gif-add-comment
                     (image-or-control-ready-gif-stream?
                      bytes?
                      . -> . any)])
  (define (gif-add-comment GifFile
                           Str)
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
  (provide/contract [gif-end
                     (image-or-control-ready-gif-stream?
                      . -> . any)])
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
                  (let loop ([CrntShiftDWord (bitwise-ior
                                              CrntShiftDWord
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
           [HashTable (make-hash-table)]
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
                      (write-buffered-byte (bitwise-and CrntShiftDWord #xff) buf port)
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
                       [NewCode (hash-table-get HashTable NewKey #f)])
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
                                      (make-hash-table) (add1 i)))
                              ;; /* Put this unique key with its relative Code in hash table: */
                              (begin
                                (hash-table-put! HashTable NewKey RunningCode)
                                (loop CrntCode
                                      (add1 RunningCode) RunningBits MaxCode1
                                      CrntShiftState CrntShiftDWord
                                      HashTable (add1 i))))))))))))))

  ;;/******************************************************************************
  ;; * color quantization utility
  ;; *****************************************************************************/

  (define (argb-bytes? b)
    (and (bytes? b)
         (zero? (remainder (bytes-length b) 4))))
  
  (provide/contract
   [quantize ((argb-bytes?)
              . ->* . 
              (bytes? color-map? (or/c false/c color?)))])

  (define (quantize argb)
    (let* ([len (quotient (bytes-length argb) 4)]
           [result (make-bytes len)]
           [rgb-key (lambda (r g b)
                      (bitwise-ior (bitwise-ior
                                    (arithmetic-shift r 16)
                                    (arithmetic-shift g 8))
                                   b))]
           [key->rgb (lambda (k)
                       (vector (arithmetic-shift k -16)
                               (bitwise-and #xFF (arithmetic-shift k -8))
                               (bitwise-and #xFF k)))])
      (let loop ([masks (list
                         ;; 8 bits per color
                         (lambda (v) v)
                         ;; 4 bits per color
                         (lambda (v)
                           (bitwise-ior
                            (bitwise-ior
                             v
                             (arithmetic-shift (bitwise-and v #x55) 1))
                            (arithmetic-shift (bitwise-and v #xCC) -1)))
                         ;; 1 bit per color
                         (lambda (v)
                           (if (v . > . 127)
                               255
                               0)))])
        (let ([mask (car masks)]
              [transparent #f]
              [table (make-hash-table)] ; relying on fixnums
              [idx 0])
          ;; Iterate over image to count colors
          ;; (as reduced by mask)
          (let loop ([i 0][pos 0])
            (unless (= i len)
              (let ([this-idx
                     (if ((bytes-ref argb pos) . < . 128)
                         (begin
                           (unless transparent
                             (set! transparent idx)
                             (set! idx (add1 idx)))
                           transparent)
                         (let ([vec (rgb-key
                                     (mask (bytes-ref argb (+ 1 pos)))
                                     (mask (bytes-ref argb (+ 2 pos)))
                                     (mask (bytes-ref argb (+ 3 pos))))])
                           (let ([v (hash-table-get table vec #f)])
                             (or v
                                 (begin
                                   (hash-table-put! table vec idx)
                                   (set! idx (add1 idx))
                                   (sub1 idx))))))])
                (unless (= this-idx 256)
                  (bytes-set! result i this-idx)
                  (loop (add1 i) (+ pos 4))))))
          (if ((hash-table-count table) . > . (if transparent
                                                  255
                                                  256))
              ;; Try again
              (loop (cdr masks))
              ;; Found an ok quantization
              (values result
                      (let* ([cnt (+ (hash-table-count table)
                                     (if transparent 1 0))]
                             [size (cond
                                    [(<= cnt 2) 2]
                                    [(<= cnt 4) 4]
                                    [(<= cnt 8) 8]
                                    [(<= cnt 16) 16]
                                    [(<= cnt 32) 32]
                                    [(<= cnt 64) 64]
                                    [(<= cnt 128) 128]
                                    [else 256])]
                             [t (make-vector size #(0 0 0))])
                        (hash-table-for-each 
                         table
                         (lambda (k v)
                           (vector-set! t v (key->rgb k))))
                        (vector->list t))
                      transparent)))))))
