#lang racket/base

(require ffi/unsafe)

(define (ffi-try-libs . libs)
  (let loop ([libs* libs]
             [exceptions '()])
    (if (null? libs*)
      (error 'ffi-try-libs
             "Could not load any of the libraries in ~a\n~a\n" libs exceptions)
      (let ([lib (caar libs*)]
            [version (cdar libs*)])
        (with-handlers ([exn:fail:filesystem?
                         (lambda (e)
                           (loop (cdr libs*) (cons e exceptions)))])
          (ffi-lib lib version))))))

(define libwand (ffi-try-libs '("libWand" "6.0.1" "6")
                              '("libMagickWand" "1" "3")))

;; ===== Main Objects =========================================================

(define-syntax defmagick-pointer-type
  (syntax-rules ()
    [(_ _name destructor s->c)
     (define-cpointer-type _name #f s->c
       (lambda (ptr)
         (if ptr
           (begin (register-finalizer ptr destructor) ptr)
           (error '_name "got a NULL pointer"))))]
    [(_ _name destructor) (defmagick-pointer-type _name destructor #f)]))

(defmagick-pointer-type _MagickWand DestroyMagickWand)
(defmagick-pointer-type _DrawingWand DestroyDrawingWand)
(defmagick-pointer-type _Image DestroyImage)
(defmagick-pointer-type _DrawInfo DestroyDrawInfo)
(defmagick-pointer-type _PixelWand DestroyPixelWand
  ;; This can be implicitly built when given a color name
  (lambda (x)
    (let loop ([x x])
      (cond [(string? x) (loop (NewPixelWand x))]
            [(list?   x) (loop (NewPixelWand x))]
            [else x])))) ; can use NULL as a pixel wand (see floodfill)

;; Use a struct for this because we want to keep the associated image width
(define-struct PixelIterator (ptr [width #:mutable]))
(define _PixelIterator
  (make-ctype _pointer PixelIterator-ptr
    (lambda (ptr)
      (if ptr
        (let ([new (make-PixelIterator ptr #f)]) ; width set by makers
          (register-finalizer new DestroyPixelIterator)
          new)
        (error '_PixelIterator "got a NULL pointer")))))

;; ===== Utilities ============================================================

(define (raise-wand-exception w)
  ((cond [(MagickWand?    w) MagickGetException       ]
         [(PixelWand?     w) PixelGetException        ]
         [(PixelIterator? w) PixelIteratorGetException]
         [(DrawingWand?   w) DrawGetException         ]
         [else (error 'raise-wand-exception "got an unknown value: ~e" w)])
   w)
  (error 'wand-exception "an undefined error occurred with ~e" w))

(define-fun-syntax _status
  (syntax-id-rules (_status)
    [_status (type: _bool
              1st-arg: 1st
              post: (r => (unless r (raise-wand-exception 1st))))]))

(define-syntax defmagick
  (syntax-rules (:)
    [(_ id : x ...) (define id (get-ffi-obj 'id libwand (_fun x ...)))]))

(define-syntax defmagick*
  (syntax-rules ()
    [(_ name x ...) (begin (defmagick name x ...) (provide name))]))

;; Used to convert (small) square matrices (lists of lists) to a memory block.
(define (_sqmatrix-of type)
  (make-ctype _pointer
    (lambda (m)
      (let ([len (and (list? m) (length m))])
        (unless (and len (< 0 len)
                     (andmap (lambda (x) (and (list? x) (= len (length x))))
                             m))
          (error '_sqmatrix "expecting a rectangular matrix"))
        (let ([block (malloc (* len len) type)])
          (let loop ([n 0] [xs (apply append m)])
            (unless (null? xs)
              (ptr-set! block type n (car xs))
              (loop (add1 n) (cdr xs))))
          block)))
    (lambda (x) (error '_sqmatrix "can be used only for ffi inputs"))))

;; This is a hack.  What is needed is to be able to define a type that is a
;; struct of two doubles that translates to a list of two values.
(define _Points
  (make-ctype _pointer
    (lambda (l)
      (unless (and (list? l)
                   (andmap (lambda (x) (and (list? x) (= 2 (length x)))) l))
        (error '_Points "expecting a list of two-element lists, got ~e" l))
      (list->cblock (apply append l) _double*))
    (lambda (x)
      (error '_Points "cannot be used as an output type"))))

;; Hack for the same reason as above.
(define _AffineMatrix
  (make-ctype _pointer
    (lambda (l)
      (unless (and (list? l) (= 6 (length l)))
        (error '_Points "expecting a list of six numbers, got ~e" l))
      (list->cblock l _double*))
    (lambda (x) (cblock->list x _double* 6))))

;; Utilities for MagickGetImagePixels/MagickSetImagePixels

(define (StorageType->type storage-type)
  (case storage-type
    [(UndefinedPixel) (error 'StorageType->type "got an UndefinedPixel")]
    [(CharPixel)    _byte]
    [(ShortPixel)   _word]
    [(IntegerPixel) _uint]
    [(LongPixel)    _ulong]
    [(FloatPixel)   _float]
    [(DoublePixel)  _double*]))

;; Gets a list and a number, and returns a list of lists of length n.
(define (n-split l n)
  (let loop ([l l][i 0][a2 null][a null])
    (cond
     [(null? l) (let ([a (if (null? a2)
                             a
                             (cons (reverse a2) a))])
                  (reverse a))]
     [(= i n) (loop l 0 null (cons (reverse a2) a))]
     [else (loop (cdr l) (add1 i) (cons (car l) a2) a)])))

;; _Quantum is something that the library tells us how big it is
(define _Quantum
  (let* ([q ((get-ffi-obj "MagickGetQuantumDepth" libwand
                          (_fun _pointer -> _string))
             #f)]
         [err (lambda ()
                (error 'libwand
                       "MagickGetQuantumDepth returned a bad value: ~s" q))])
    (cond [(and (string? q) (regexp-match #rx"^Q([0-9]+)$" q))
           => (lambda (m)
                (case (string->number (cadr m))
                  [( 8) _uint8] [(16) _uint16] [(32) _uint32]
                  [else (err)]))]
          [else (err)])))

;; Used twice (PixelGetIndex/PixelSetIndex).
(define _IndexPacket _Quantum)

(define _MagickSizeType _uint64)

;; This type only needs to be created for PixelGetQuantumColor results.
;; (Could also define it as a macro for (_list ? _Quantum 4).)
(define-struct PixelPacket (ptr))
(define _PixelPacket
  (make-ctype _pointer PixelPacket-ptr make-PixelPacket))
(define (NewPixelPacket)
  (make-PixelPacket (malloc _Quantum 4)))

;; ===== Enumeration Types ====================================================

(define _MagickExceptionType
  (_enum '(UndefinedException
           WarningException = 300
           ResourceLimitWarning = 300
           TypeWarning = 305
           OptionWarning = 310
           DelegateWarning = 315
           MissingDelegateWarning = 320
           CorruptImageWarning = 325
           FileOpenWarning = 330
           BlobWarning = 335
           StreamWarning = 340
           CacheWarning = 345
           CoderWarning = 350
           ModuleWarning = 355
           DrawWarning = 360
           ImageWarning = 365
           WandWarning = 370
           XServerWarning = 380
           MonitorWarning = 385
           RegistryWarning = 390
           ConfigureWarning = 395
           ErrorException = 400
           ResourceLimitError = 400
           TypeError = 405
           OptionError = 410
           DelegateError = 415
           MissingDelegateError = 420
           CorruptImageError = 425
           FileOpenError = 430
           BlobError = 435
           StreamError = 440
           CacheError = 445
           CoderError = 450
           ModuleError = 455
           DrawError = 460
           ImageError = 465
           WandError = 470
           XServerError = 480
           MonitorError = 485
           RegistryError = 490
           ConfigureError = 495
           FatalErrorException = 700
           ResourceLimitFatalError = 700
           TypeFatalError = 705
           OptionFatalError = 710
           DelegateFatalError = 715
           MissingDelegateFatalError = 720
           CorruptImageFatalError = 725
           FileOpenFatalError = 730
           BlobFatalError = 735
           StreamFatalError = 740
           CacheFatalError = 745
           CoderFatalError = 750
           ModuleFatalError = 755
           DrawFatalError = 760
           ImageFatalError = 765
           WandFatalError = 770
           XServerFatalError = 780
           MonitorFatalError = 785
           RegistryFatalError = 790
           ConfigureFatalError = 795
           )))

(define _CompositeOperator
  (_enum '(UndefinedCompositeOp
           NoCompositeOp
           AddCompositeOp
           AtopCompositeOp
           BlendCompositeOp
           BumpmapCompositeOp
           ClearCompositeOp
           ColorBurnCompositeOp
           ColorDodgeCompositeOp
           ColorizeCompositeOp
           CopyBlackCompositeOp
           CopyBlueCompositeOp
           CopyCompositeOp
           CopyCyanCompositeOp
           CopyGreenCompositeOp
           CopyMagentaCompositeOp
           CopyOpacityCompositeOp
           CopyRedCompositeOp
           CopyYellowCompositeOp
           DarkenCompositeOp
           DstAtopCompositeOp
           DstCompositeOp
           DstInCompositeOp
           DstOutCompositeOp
           DstOverCompositeOp
           DifferenceCompositeOp
           DisplaceCompositeOp
           DissolveCompositeOp
           ExclusionCompositeOp
           HardLightCompositeOp
           HueCompositeOp
           InCompositeOp
           LightenCompositeOp
           LuminizeCompositeOp
           MinusCompositeOp
           ModulateCompositeOp
           MultiplyCompositeOp
           OutCompositeOp
           OverCompositeOp
           OverlayCompositeOp
           PlusCompositeOp
           ReplaceCompositeOp
           SaturateCompositeOp
           ScreenCompositeOp
           SoftLightCompositeOp
           SrcAtopCompositeOp
           SrcCompositeOp
           SrcInCompositeOp
           SrcOutCompositeOp
           SrcOverCompositeOp
           SubtractCompositeOp
           ThresholdCompositeOp
           XorCompositeOp
           )))

(define _ColorspaceType
  (_enum '(UndefinedColorspace
           RGBColorspace
           GRAYColorspace
           TransparentColorspace
           OHTAColorspace
           LABColorspace
           XYZColorspace
           YCbCrColorspace
           YCCColorspace
           YIQColorspace
           YPbPrColorspace
           YUVColorspace
           CMYKColorspace
           sRGBColorspace
           HSBColorspace
           HSLColorspace
           HWBColorspace
           )))

(define _CompressionType
  (_enum '(UndefinedCompression
           NoCompression
           BZipCompression
           FaxCompression
           Group4Compression
           JPEGCompression
           LosslessJPEGCompression
           LZWCompression
           RLECompression
           ZipCompression
           )))

(define _DisposeType
  (_enum '(UnrecognizedDispose
           UndefinedDispose = 0
           NoneDispose = 1
           BackgroundDispose = 2
           PreviousDispose = 3
           )))

(define _ImageType
  (_enum '(UndefinedType
           BilevelType
           GrayscaleType
           GrayscaleMatteType
           PaletteType
           PaletteMatteType
           TrueColorType
           TrueColorMatteType
           ColorSeparationType
           ColorSeparationMatteType
           OptimizeType
           )))

(define _InterlaceType
  (_enum '(UndefinedInterlace
           NoInterlace
           LineInterlace
           PlaneInterlace
           PartitionInterlace
           )))

(define _ChannelType
  (_bitmask '(UndefinedChannel = #x0000
              RedChannel = #x0001
              CyanChannel = #x0001
              GreenChannel = #x0002
              MagentaChannel = #x0002
              BlueChannel = #x0004
              YellowChannel = #x0004
              AlphaChannel = #x0008
              OpacityChannel = #x0008
              MatteChannel = #x0008 ; deprecated
              BlackChannel = #x0020
              IndexChannel = #x0020
              AllChannels = #x7fffffff
              )))

(define _MetricType
  (_enum '(UndefinedMetric
           MeanAbsoluteErrorMetric
           MeanSquaredErrorMetric
           PeakAbsoluteErrorMetric
           PeakSignalToNoiseRatioMetric
           RootMeanSquaredErrorMetric
           )))

(define _NoiseType
  (_enum '(UndefinedNoise
           UniformNoise
           GaussianNoise
           MultiplicativeGaussianNoise
           ImpulseNoise
           LaplacianNoise
           PoissonNoise
           )))

(define _MagickEvaluateOperator
  (_enum '(UndefinedEvaluateOperator
           AddEvaluateOperator
           AndEvaluateOperator
           DivideEvaluateOperator
           LeftShiftEvaluateOperator
           MaxEvaluateOperator
           MinEvaluateOperator
           MultiplyEvaluateOperator
           OrEvaluateOperator
           RightShiftEvaluateOperator
           SetEvaluateOperator
           SubtractEvaluateOperator
           XorEvaluateOperator
           )))

(define _ResourceType
  (_enum '(UndefinedResource
           AreaResource
           DiskResource
           FileResource
           MapResource
           MemoryResource
           )))

(define _StorageType
  (_enum '(UndefinedPixel
           CharPixel
           ShortPixel
           IntegerPixel
           LongPixel
           FloatPixel
           DoublePixel
           )))

(define _RenderingIntent
  (_enum '(UndefinedIntent
           SaturationIntent
           PerceptualIntent
           AbsoluteIntent
           RelativeIntent
           )))

(define _ResolutionType
  (_enum '(UndefinedResolution
           PixelsPerInchResolution
           PixelsPerCentimeterResolution
           )))

(define _VirtualPixelMethod
  (_enum '(UndefinedVirtualPixelMethod
           ConstantVirtualPixelMethod
           EdgeVirtualPixelMethod
           MirrorVirtualPixelMethod
           TileVirtualPixelMethod
           )))

(define _PreviewType
  (_enum '(UndefinedPreview
           RotatePreview
           ShearPreview
           RollPreview
           HuePreview
           SaturationPreview
           BrightnessPreview
           GammaPreview
           SpiffPreview
           DullPreview
           GrayscalePreview
           QuantizePreview
           DespecklePreview
           ReduceNoisePreview
           AddNoisePreview
           SharpenPreview
           BlurPreview
           ThresholdPreview
           EdgeDetectPreview
           SpreadPreview
           SolarizePreview
           ShadePreview
           RaisePreview
           SegmentPreview
           SwirlPreview
           ImplodePreview
           WavePreview
           OilPaintPreview
           CharcoalDrawingPreview
           JPEGPreview
           )))

(define _FilterTypes
  (_enum '(UndefinedFilter
           PointFilter
           BoxFilter
           TriangleFilter
           HermiteFilter
           HanningFilter
           HammingFilter
           BlackmanFilter
           GaussianFilter
           QuadraticFilter
           CubicFilter
           CatromFilter
           MitchellFilter
           LanczosFilter
           BesselFilter
           SincFilter
           )))

(define _MontageMode
  (_enum '(UndefinedMode
           FrameMode
           UnframeMode
           ConcatenateMode
           )))

(define _StretchType
  (_enum '(UndefinedStretch
           NormalStretch
           UltraCondensedStretch
           ExtraCondensedStretch
           CondensedStretch
           SemiCondensedStretch
           SemiExpandedStretch
           ExpandedStretch
           ExtraExpandedStretch
           UltraExpandedStretch
           AnyStretch
           )))

(define _StyleType
  (_enum '(UndefinedStyle
           NormalStyle
           ItalicStyle
           ObliqueStyle
           AnyStyle
           )))

(define _GravityType
  (_enum '(UndefinedGravity
           ForgetGravity = 0
           NorthWestGravity = 1
           NorthGravity = 2
           NorthEastGravity = 3
           WestGravity = 4
           CenterGravity = 5
           EastGravity = 6
           SouthWestGravity = 7
           SouthGravity = 8
           SouthEastGravity = 9
           StaticGravity = 10
           )))

(define _ClipPathUnits
  (_enum '(UndefinedPathUnits
           UserSpace
           UserSpaceOnUse
           ObjectBoundingBox
           )))

(define _DecorationType
  (_enum '(UndefinedDecoration
           NoDecoration
           UnderlineDecoration
           OverlineDecoration
           LineThroughDecoration
           )))

(define _FillRule
  (_enum '(UndefinedRule
           EvenOddRule
           NonZeroRule
           )))

(define _LineCap
  (_enum '(UndefinedCap
           ButtCap
           RoundCap
           SquareCap
           )))

(define _LineJoin
  (_enum '(UndefinedJoin
           MiterJoin
           RoundJoin
           BevelJoin
           )))

(define _PaintMethod
  (_enum '(UndefinedMethod
           PointMethod
           ReplaceMethod
           FloodfillMethod
           FillToBorderMethod
           ResetMethod
           )))

;; ===== MagickWand API =======================================================

;; MagickGetException returns the severity, reason, and description of any
;; error that occurs when using other methods in this API (as an exception).
(defmagick* MagickGetException :
  _MagickWand (severity : (_ptr o _MagickExceptionType)) -> (message : _string)
  -> (unless (eq? severity 'UndefinedException)
       (error 'MagickWand "(~a) ~a" severity message)))

;; DestroyMagickWand deallocates memory associated with an MagickWand.
;; Intended for internal use only, must be defined after the above.
(defmagick DestroyMagickWand :
  _MagickWand -> _void)

;; CloneMagickWand makes an exact copy of the specified wand.
(defmagick* CloneMagickWand :
  _MagickWand -> _MagickWand)

;; GetImageFromMagickWand returns the current image from the magick wand.
(defmagick* GetImageFromMagickWand :
  _MagickWand -> _Image)

;; MagickAdaptiveThresholdImage selects an individual threshold for each pixel
;; based on the range of intensity values in its local neighborhood.  This
;; allows for thresholding of an image whose global intensity histogram doesn't
;; contain distinctive peaks.
(defmagick* MagickAdaptiveThresholdImage :
  _MagickWand (width : _ulong) (height : _ulong) (offset : _long) -> _status)

;; MagickAddImage adds the specified images at the current image location.
(defmagick* MagickAddImage :
  _MagickWand (insert-wand : _MagickWand) -> _status)

;; MagickAddNoiseImage adds random noise to the image.
(defmagick* MagickAddNoiseImage :
  _MagickWand _NoiseType -> _status)

;; MagickAffineTransformImage transforms an image as dictated by the affine
;; matrix of the drawing wand.
(defmagick* MagickAffineTransformImage :
  _MagickWand _DrawingWand -> _status)

;; MagickAnimateImages animates an image or image sequence.
(defmagick* MagickAnimateImages :
  (w . server) ::
  (w : _MagickWand) (_string = (and (pair? server) (car server))) -> _status)

;; MagickAnnotateImage annotates an image with text.
(defmagick* MagickAnnotateImage :
  _MagickWand _DrawingWand
  (x : _double*) (y : _double*) (angle : _double*) (text : _string)
  -> _status)

;; MagickAppendImages append a set of images.
(defmagick* MagickAppendImages :
  _MagickWand (top-to-bottom? : _bool) -> _MagickWand)

;; MagickAverageImages average a set of images.
(defmagick* MagickAverageImages :
  _MagickWand -> _MagickWand)

;; MagickBlackThresholdImage is like MagickThresholdImage but forces all pixels
;; below the threshold into black while leaving all pixels above the threshold
;; unchanged.
(defmagick* MagickBlackThresholdImage :
  _MagickWand _PixelWand -> _status)

;; MagickBlurImage blurs an image.  We convolve the image with a gaussian
;; operator of the given radius and standard deviation (sigma).  For reasonable
;; results, the radius should be larger than sigma.  Use a radius of 0 and
;; MagickBlurImage selects a suitable radius for you.
(defmagick* MagickBlurImage :
  _MagickWand (radius : _double*) (sigma : _double*) -> _status)

;; MagickBlurImageChannel blurs one or more image channels.  We convolve the
;; image cnannel with a gaussian operator of the given radius and standard
;; deviation (sigma).  For reasonable results, the radius should be larger than
;; sigma.  Use a radius of 0 and MagickBlurImageChannel selects a suitable
;; radius for you.
(defmagick* MagickBlurImageChannel :
  _MagickWand _ChannelType (radius : _double*) (sigma : _double*) -> _status)

;; MagickBorderImage surrounds the image with a border of the color defined by
;; the bordercolor pixel wand.
(defmagick* MagickBorderImage :
  _MagickWand (border : _PixelWand) (width : _ulong) (height : _ulong)
  -> _status)

;; MagickCharcoalImage simulates a charcoal drawing.
(defmagick* MagickCharcoalImage :
  _MagickWand (radius : _double*) (sigma : _double*) -> _status)

;; MagickChopImage removes a region of an image and collapses the image to
;; occupy the removed portion.
(defmagick* MagickChopImage :
  _MagickWand (width : _ulong) (height : _ulong) (x : _long) (y : _long)
  -> _status)

;; MagickClipImage clips along the first path from the 8BIM profile, if
;; present.
(defmagick* MagickClipImage :
  _MagickWand -> _status)

;; MagickClipPathImage clips along the named paths from the 8BIM profile, if
;; present.  Later operations take effect inside the path.  Id may be a number
;; if preceded with #, to work on a numbered path, e.g., "#1" to use the first
;; path.  If inside? is non-zero, later operations take effect inside clipping
;; path.  Otherwise later operations take effect outside clipping path.
(defmagick* MagickClipPathImage :
  _MagickWand (name : _string) (inside? : _bool) -> _status)

;; MagickCoalesceImages composites a set of images while respecting any page
;; offsets and disposal methods.  GIF, MIFF, and MNG animation sequences
;; typically start with an image background and each subsequent image varies in
;; size and offset.  MagickCoalesceImages returns a new sequence where each
;; image in the sequence is the same size as the first and composited with the
;; next image in the sequence.
(defmagick* MagickCoalesceImages :
  _MagickWand -> _MagickWand)

;; MagickColorFloodfillImage changes the color value of any pixel that matches
;; target and is an immediate neighbor.  If the method FillToBorderMethod is
;; specified, the color value is changed for any neighbor pixel that does not
;; match the bordercolor member of image.
(defmagick* MagickColorFloodfillImage :
  _MagickWand (fill : _PixelWand) (fuzz : _double*) (border : _PixelWand)
  (x : _long) (y : _long)
  -> _status)

;; MagickColorizeImage blends the fill color with each pixel in the image.
(defmagick* MagickColorizeImage :
  _MagickWand (colorize : _PixelWand) (opacity : _PixelWand) -> _status)

;; MagickCombineImages combines one or more images into a single image.  The
;; grayscale value of the pixels of each image in the sequence is assigned in
;; order to the specified hannels of the combined image.  The typical ordering
;; would be image 1 => Red, 2 => Green, 3 => Blue, etc.
(defmagick* MagickCombineImages :
  _MagickWand _ChannelType -> _MagickWand)

;; MagickCommentImage adds a comment to your image.
(defmagick* MagickCommentImage :
  _MagickWand (comment : _string) -> _status)

;; MagickCompareImageChannels compares one or more image channels and returns
;; the specified distortion metric.
(defmagick* MagickCompareImageChannels :
  _MagickWand (reference : _MagickWand)
  _ChannelType _MetricType (distortion : (_ptr o _double*))
  -> (comp : _MagickWand)
  -> (list distortion comp))

;; MagickCompositeImage composite one image onto another at the specified
;; offset.
(defmagick* MagickCompositeImage :
  _MagickWand (composite : _MagickWand) _CompositeOperator
  (x-offset : _long) (y-offset : _long)
  -> _status)

;; MagickContrastImage enhances the intensity differences between the lighter
;; and darker elements of the image.  Set sharpen? to a value other than 0 to
;; increase the image contrast otherwise the contrast is reduced.
(defmagick* MagickContrastImage :
  _MagickWand (sharpen? : _bool) -> _status)

;; MagickConvolveImage applies a custom convolution kernel to the image.
(defmagick* MagickConvolveImage :
  (w kernel) ::
  (w : _MagickWand)
  (_ulong = (length kernel)) (kernel : (_sqmatrix-of _double*))
  -> _status)

;; MagickConvolveImageChannel applies a custom convolution kernel to one or
;; more image channels.
(defmagick* MagickConvolveImageChannel :
  (w channels kernel) ::
  (w : _MagickWand) (channels : _ChannelType)
  (_ulong = (length kernel)) (kernel : (_sqmatrix-of _double*))
  -> _status)

;; MagickCropImage extracts a region of the image.
(defmagick* MagickCropImage :
  _MagickWand (width : _ulong) (height : _ulong) (x : _long) (y : _long)
  -> _status)

;; MagickCycleColormapImage displaces an image's colormap by a given number of
;; positions.  If you cycle the colormap a number of times you can produce a
;; psychodelic effect.
(defmagick* MagickCycleColormapImage :
  _MagickWand (displace : _long) -> _status)

;; MagickDeconstructImages compares each image with the next in a sequence and
;; returns the maximum bounding region of any pixel differences it discovers.
(defmagick* MagickDeconstructImages :
  _MagickWand -> _MagickWand)

;; MagickDescribeImage describes an image by printing its attributes to the
;; file.  Attributes include the image width, height, size, and others.
(defmagick* MagickDescribeImage :
  _MagickWand -> _string)

;; MagickDespeckleImage reduces the speckle noise in an image while perserving
;; the edges of the original image.
(defmagick* MagickDespeckleImage :
  _MagickWand -> _status)

;; MagickDisplayImage displays an image.
(defmagick* MagickDisplayImage :
  (w . server) ::
  (w : _MagickWand) (_string = (and (pair? server) (car server))) -> _status)

;; MagickDisplayImages displays an image or image sequence.
(defmagick* MagickDisplayImages :
  (w . server) ::
  (w : _MagickWand) (_string = (and (pair? server) (car server))) -> _status)

;; MagickDrawImage draw an image the specified number of degrees.
(defmagick* MagickDrawImage :
  _MagickWand _DrawingWand -> _status)

;; MagickEdgeImage enhance edges within the image with a convolution filter of
;; the given radius.  Use a radius of 0 and MagickEdgeImage selects a
;; suitable radius for you.
(defmagick* MagickEdgeImage :
  _MagickWand (radius : _double*) -> _status)

;; MagickEmbossImage returns a grayscale image with a three-dimensional effect.
;; We convolve the image with a Gaussian operator of the given radius and
;; standard deviation (sigma).  For reasonable results, radius should be larger
;; than sigma.  Use a radius of 0 and MagickEmbossImage selects a suitable
;; radius for you.
(defmagick* MagickEmbossImage :
  _MagickWand (radius : _double*) (sigma : _double*) -> _status)

;; MagickEnhanceImage applies a digital filter that improves the quality of a
;; noisy image.
(defmagick* MagickEnhanceImage :
  _MagickWand -> _status)

;; MagickEqualizeImage equalizes the image histogram.
(defmagick* MagickEqualizeImage :
  _MagickWand -> _status)

;; Use MagickEvaluateImage to apply an arithmetic, relational, or logical
;; operator to an image.  These operations can be used to lighten or darken an
;; image, to increase or decrease contrast in an image, or to produce the
;; "negative" of an image.
(defmagick* MagickEvaluateImage :
  _MagickWand _MagickEvaluateOperator (const : _double*) -> _status)

;; Use MagickEvaluateImageChannel to apply an arithmetic, relational, or
;; logical operator to an image.  These operations can be used to lighten or
;; darken an image, to increase or decrease contrast in an image, or to produce
;; the "negative" of an image.
(defmagick* MagickEvaluateImageChannel :
  _MagickWand _ChannelType _MagickEvaluateOperator (const : _double*)
  -> _status)

;; MagickFlattenImages merges a sequence of images.  This is useful for
;; combining Photoshop layers into a single image.
(defmagick* MagickFlattenImages :
  _MagickWand -> _MagickWand)

;; MagickFlipImage creates a vertical mirror image by reflecting the pixels
;; around the central x-axis.
(defmagick* MagickFlipImage :
  _MagickWand -> _status)

;; MagickFlopImage creates a horizontal mirror image by reflecting the pixels
;; around the central y-axis.
(defmagick* MagickFlopImage :
  _MagickWand -> _status)

;; MagickFrameImage adds a simulated three-dimensional border around the image.
;; The width and height specify the border width of the vertical and horizontal
;; sides of the frame.  The inner and outer bevels indicate the width of the
;; inner and outer shadows of the frame.
(defmagick* MagickFrameImage :
  _MagickWand (matte : _PixelWand)
  (width : _ulong) (height : _ulong)
  (inner-bevel : _long) (outer-bevel : _long)
  -> _status)

;; MagickFxImage evaluate expression for each pixel in the image.
(defmagick* MagickFxImage :
  _MagickWand (expr : _string) -> _MagickWand)

;; MagickFxImageChannel evaluate expression for each pixel in the specified
;; channel.
(defmagick* MagickFxImageChannel :
  _MagickWand _ChannelType (expr : _string) -> _MagickWand)

;; Use MagickGammaImage to gamma-correct an image.  The same image viewed on
;; different devices will have perceptual differences in the way the image's
;; intensities are represented on the screen.  Specify individual gamma levels
;; for the red, green, and blue channels, or adjust all three with the gamma
;; parameter.  Values typically range from 0.8 to 2.3.  You can also reduce the
;; influence of a particular channel with a gamma value of 0.
(defmagick* MagickGammaImage :
  _MagickWand (gamma : _double*) -> _status)

;; Use MagickGammaImageChannel to gamma-correct a particular image channel.
;; The same image viewed on different devices will have perceptual differences
;; in the way the image's intensities are represented on the screen.  Specify
;; individual gamma levels for the red, green, and blue channels, or adjust all
;; three with the gamma parameter.  Values typically range from 0.8 to 2.3.
(defmagick* MagickGammaImageChannel :
  _MagickWand _ChannelType (gamma : _double*) -> _status)

;; MagickGaussianBlurImage blurs an image.  We convolve the image with a
;; Gaussian operator of the given radius and standard deviation (sigma).  For
;; reasonable results, the radius should be larger than sigma.  Use a radius of
;; 0 and MagickGaussianBlurImage selects a suitable radius for you.
(defmagick* MagickGaussianBlurImage :
  _MagickWand (radius : _double*) (sigma : _double*) -> _status)

;; MagickGaussianBlurImageChannel blurs one or more image channels.  We
;; convolve the image cnannel with a Gaussian operator of the given radius and
;; standard deviation (sigma).  For reasonable results, the radius should be
;; larger than sigma.  Use a radius of 0 and MagickGaussianBlurImageChannel
;; selects a suitable radius for you.
(defmagick* MagickGaussianBlurImageChannel :
  _MagickWand _ChannelType (radius : _double*) (sigma : _double*) -> _status)

;; MagickGetCopyright returns the ImageMagick API copyright as a string.
(defmagick* MagickGetCopyright :
  -> _string)

;; MagickGetFilename returns the filename associated with an image sequence.
(defmagick* MagickGetFilename :
  _MagickWand -> _file)

;; MagickGetHomeURL returns the ImageMagick home URL.
(defmagick* MagickGetHomeURL :
  -> _string)

;; MagickGetImage gets the image at the current image index.
(defmagick* MagickGetImage :
  _MagickWand -> _MagickWand)

;; MagickGetImageBackgroundColor returns the image background color.
(defmagick* MagickGetImageBackgroundColor :
  _MagickWand (c : _PixelWand = (NewPixelWand)) -> _status -> c)

;; MagickGetImageBluePrimary returns the chromaticy blue primary point for the
;; image.
(defmagick* MagickGetImageBluePrimary :
  _MagickWand (x : (_ptr o _double*)) (y : (_ptr o _double*)) -> _status
  -> (list x y))

;; MagickGetImageBorderColor returns the image border color.
(defmagick* MagickGetImageBorderColor :
  _MagickWand (c : _PixelWand = (NewPixelWand)) -> _status -> c)

;; MagickGetImageChannelDepth gets the depth for a particular image channel.
(defmagick* MagickGetImageChannelDepth :
  _MagickWand _ChannelType -> _ulong)

;; MagickGetImageChannelExtrema gets the extrema for one or more image
;; channels.
(defmagick* MagickGetImageChannelExtrema :
  _MagickWand _ChannelType (min : (_ptr o _ulong)) (max : (_ptr o _ulong))
  -> _status
  -> (list min max))

;; MagickGetImageChannelMean gets the mean and standard deviation of one or
;; more image channels.
(defmagick* MagickGetImageChannelMean :
  _MagickWand _ChannelType
  (mean : (_ptr o _double*)) (standard-deviation : (_ptr o _double*))
  -> _status
  -> (list mean standard-deviation))

;; MagickGetImageColormapColor returns the color of the specified colormap
;; index.
(defmagick* MagickGetImageColormapColor :
  _MagickWand (colormap-index : _ulong) (c : _PixelWand = (NewPixelWand))
  -> _status -> c)

;; MagickGetImageColors gets the number of unique colors in the image.
(defmagick* MagickGetImageColors :
  _MagickWand -> _ulong)

;; MagickGetImageColorspace gets the image colorspace.
(defmagick* MagickGetImageColorspace :
  _MagickWand -> _ColorspaceType)

;; MagickGetImageCompose returns the composite operator associated with the
;; image.
(defmagick* MagickGetImageCompose :
  _MagickWand -> _CompositeOperator)

;; MagickGetImageCompression gets the image compression.
(defmagick* MagickGetImageCompression :
  _MagickWand -> _CompressionType)

;; MagickGetImageCompressionQuality gets the image compression quality.
(defmagick* MagickGetImageCompressionQuality :
  _MagickWand -> _ulong)

;; MagickGetImageDelay gets the image delay.
(defmagick* MagickGetImageDelay :
  _MagickWand -> _ulong)

;; MagickGetImageDepth gets the image depth.
(defmagick* MagickGetImageDepth :
  _MagickWand -> _ulong)

;; MagickGetImageDispose gets the image disposal method.
(defmagick* MagickGetImageDispose :
  _MagickWand -> _DisposeType)

;; MagickGetImageExtrema gets the extrema for the image.
(defmagick* MagickGetImageExtrema :
  _MagickWand (min : (_ptr o _ulong)) (max : (_ptr o _ulong)) -> _status
  -> (list min max))

;; MagickGetImageFilename returns the filename of a particular image in a
;; sequence.
(defmagick* MagickGetImageFilename :
  _MagickWand -> _string)

;; MagickGetImageFormat returns the format of a particular image in a sequence.
(defmagick* MagickGetImageFormat :
  _MagickWand -> _string)

;; MagickGetImageGamma gets the image gamma.
(defmagick* MagickGetImageGamma :
  _MagickWand -> _double*)

;; MagickGetImageGreenPrimary returns the chromaticy green primary point.
(defmagick* MagickGetImageGreenPrimary :
  _MagickWand (x : (_ptr o _double*)) (y : (_ptr o _double*)) -> _status
  -> (list x y))

;; MagickGetImageHeight returns the image height.
(defmagick* MagickGetImageHeight :
  _MagickWand -> _ulong)

;; MagickGetImageHistogram returns the image histogram as an array of PixelWand
;; wands.
(defmagick* MagickGetImageHistogram :
  _MagickWand (len : (_ptr o _ulong)) -> (_list o _PixelWand len))

;; MagickGetImageIndex returns the index of the current image.
(defmagick* MagickGetImageIndex :
  _MagickWand -> _long)

;; MagickGetImageInterlaceScheme gets the image interlace scheme.
(defmagick* MagickGetImageInterlaceScheme :
  _MagickWand -> _InterlaceType)

;; MagickGetImageIterations gets the image iterations.
(defmagick* MagickGetImageIterations :
  _MagickWand -> _ulong)

;; MagickGetImageMatteColor returns the image matte color.
(defmagick* MagickGetImageMatteColor :
  _MagickWand (c : _PixelWand = (NewPixelWand)) -> _status -> c)

;; MagickGetImagePixels extracts pixel data from an image and returns it to
;; you.  The method returns False on success otherwise True if an error is
;; encountered.  The data is returned as char, short int, int, long, float, or
;; double in the order specified by map.  Suppose you want to extract the first
;; scanline of a 640x480 image as character data in red-green-blue order:
;;     (MagickGetImagePixels wand 0 0 640 1 "RGB" 'CharPixel)
;; `map' is a string that reflects the expected ordering of the pixel array.
;; It can be any combination or order of R = red, G = green, B = blue,
;; A = alpha (0 is transparent), O = opacity (0 is opaque), C = cyan,
;; Y = yellow, M = magenta, K = black, I = intensity (for grayscale), P = pad.
;; Note: the scheme interface uses a list of rows, each a list of values for
;; each element in the map.
(defmagick* MagickGetImagePixels :
  _MagickWand (x : _long) (y : _long) (width : _ulong) (height : _ulong)
  (map : _string) (storage-type : _StorageType)
  ;; create the block, remember size and type
  (size  : _?       = (* width height (string-length map)))
  (type  : _?       = (StorageType->type storage-type))
  (block : _pointer = (malloc size type))
  -> _status
  -> (let loop ([n (sub1 size)] [r '()])
       (if (< n 0)
         (n-split (n-split r (string-length map)) width)
         (loop (sub1 n) (cons (ptr-ref block type n) r)))))

;; MagickGetImageProfile returns the named image profile.
(defmagick* MagickGetImageProfile :
  _MagickWand (profile-name : _string) (len : (_ptr o _ulong))
  -> (_bytes o len))

;; MagickGetImageRedPrimary returns the chromaticy red primary point.
(defmagick* MagickGetImageRedPrimary :
  _MagickWand (x : (_ptr o _double*)) (y : (_ptr o _double*)) -> _status
  -> (list x y))

;; MagickGetImageRenderingIntent gets the image rendering intent.
(defmagick* MagickGetImageRenderingIntent :
  _MagickWand -> _RenderingIntent)

;; MagickGetImageSignature generates an SHA-256 message digest for the image
;; pixel stream.
(defmagick* MagickGetImageSignature :
  _MagickWand -> _string)

;; MagickGetImageSize returns the image size.
(defmagick* MagickGetImageSize :
  _MagickWand -> _MagickSizeType)

;; MagickGetImageType gets the image type.
(defmagick* MagickGetImageType :
  _MagickWand -> _ImageType)

;; MagickGetImageUnits gets the image units of resolution.
(defmagick* MagickGetImageUnits :
  _MagickWand -> _ResolutionType)

;; MagickGetImageVirtualPixelMethod returns the virtual pixel method for the
;; sepcified image.
(defmagick* MagickGetImageVirtualPixelMethod :
  _MagickWand -> _VirtualPixelMethod)

;; MagickGetImageWhitePoint returns the chromaticy white point.
(defmagick* MagickGetImageWhitePoint :
  _MagickWand (x : (_ptr o _double*)) (y : (_ptr o _double*)) -> _status
  -> (list x y))

;; MagickGetImageWidth returns the image width.
(defmagick* MagickGetImageWidth :
  _MagickWand -> _ulong)

;; MagickGetImageResolution gets the image X & Y resolution.
(defmagick* MagickGetImageResolution :
  _MagickWand (res-x : (_ptr o _double*)) (res-y : (_ptr o _double*))
  -> _status
  -> (list res-x res-y))

;; MagickGetNumberImages returns the number of images associated with a magick
;; wand.
(defmagick* MagickGetNumberImages :
  _MagickWand -> _ulong)

;; MagickGetPackageName returns the ImageMagick package name.
(defmagick* MagickGetPackageName :
  -> _string)

;; MagickGetQuantumDepth returns the ImageMagick quantum depth.
(defmagick* MagickGetQuantumDepth :
  (q : (_ptr o _ulong)) -> _string -> q)

;; MagickGetReleaseDate returns the ImageMagick release date.
(defmagick* MagickGetReleaseDate :
  -> _string)

;; MagickGetResourceLimit returns the specified resource in megabytes.
(defmagick* MagickGetResourceLimit :
  _ResourceType -> _ulong)

;; MagickGetSamplingFactors gets the horizontal and vertical sampling factor.
(defmagick* MagickGetSamplingFactors :
  _MagickWand (len : (_ptr o _ulong)) -> (_list o _double* len))

;; MagickGetSize returns the size associated with the magick wand.
(defmagick* MagickGetSize :
  _MagickWand (width : (_ptr o _ulong)) (height : (_ptr o _ulong)) -> _status
  -> (list width height))

;; MagickGetVersion returns the ImageMagick API version as a string and as a
;; number.
(defmagick* MagickGetVersion :
  (v : (_ptr o _ulong)) -> (s : _string) -> (list v s))

;; MagickHasNextImage returns True if the wand has more images when traversing
;; the list in the forward direction
(defmagick* MagickHasNextImage :
  _MagickWand -> _bool)

;; MagickHasPreviousImage returns True if the wand has more images when
;; traversing the list in the reverse direction
(defmagick* MagickHasPreviousImage :
  _MagickWand -> _bool)

;; MagickImplodeImage creates a new image that is a copy of an existing one
;; with the image pixels "implode" by the specified percentage.
(defmagick* MagickImplodeImage :
  _MagickWand (amount : _double*) -> _status)

;; MagickLabelImage adds a label to your image.
(defmagick* MagickLabelImage :
  _MagickWand (label : _string) -> _status)

;; MagickLevelImage adjusts the levels of an image by scaling the colors
;; falling between specified white and black points to the full available
;; quantum range.  The parameters provided represent the black, mid, and white
;; points.  The black point specifies the darkest color in the image.  Colors
;; darker than the black point are set to zero.  Mid point specifies a gamma
;; correction to apply to the image.  White point specifies the lightest color
;; in the image.  Colors brighter than the white point are set to the maximum
;; quantum value.
(defmagick* MagickLevelImage :
  _MagickWand
  (black-point : _double*) (gamma : _double*) (white-point : _double*)
  -> _status)

;; MagickLevelImageChannel adjusts the levels of the specified channel of the
;; reference image by scaling the colors falling between specified white and
;; black points to the full available quantum range.  The parameters provided
;; represent the black, mid, and white points.  The black point specifies the
;; darkest color in the image.  Colors darker than the black point are set to
;; zero.  Mid point specifies a gamma correction to apply to the image.  White
;; point specifies the lightest color in the image.  Colors brighter than the
;; white point are set to the maximum quantum value.
(defmagick* MagickLevelImageChannel :
  _MagickWand _ChannelType
  (black-point : _double*) (gamma : _double*) (white-point : _double*)
  -> _status)

;; MagickMagnifyImage is a convenience method that scales an image
;; proportionally to twice its original size.
(defmagick* MagickMagnifyImage :
  _MagickWand -> _status)

;; MagickMapImage replaces the colors of an image with the closest color from a
;; reference image.
(defmagick* MagickMapImage :
  _MagickWand (mapping : _MagickWand) (dither? : _bool) -> _status)

;; MagickMatteFloodfillImage changes the transparency value of any pixel that
;; matches target and is an immediate neighbor.  If the method
;; FillToBorderMethod is specified, the transparency value is changed for any
;; neighbor pixel that does not match the bordercolor member of image.
(defmagick* MagickMatteFloodfillImage :
  _MagickWand
  (opacity : _Quantum) (fuzz : _double*) (border : _PixelWand)
  (x : _long) (y : _long)
  -> _status)

;; MagickMedianFilterImage applies a digital filter that improves the quality
;; of a noisy image.  Each pixel is replaced by the median in a set of
;; neighboring pixels as defined by radius.
(defmagick* MagickMedianFilterImage :
  _MagickWand (radius : _double*) -> _status)

;; MagickMinifyImage is a convenience method that scales an image
;; proportionally to one-half its original size
(defmagick* MagickMinifyImage :
  _MagickWand -> _status)

;; MagickModulateImage lets you control the brightness, saturation, and hue of
;; an image.
(defmagick* MagickModulateImage :
  _MagickWand (brightness : _double*) (saturation : _double*) (hue : _double*)
  -> _status)

;; Use MagickMontageImage to create a composite image by combining several
;; separate images.  The images are tiled on the composite image with the name
;; of the image optionally appearing just below the individual tile.
(defmagick* MagickMontageImage :
  _MagickWand _DrawingWand
  (tile-geometry : _string) (thumbnail-geometry : _string)
  _MontageMode (frame-geometry : _string) -> _MagickWand)

;; MagickMorphImages method morphs a set of images.  Both the image pixels and
;; size are linearly interpolated to give the appearance of a meta-morphosis
;; from one image to the next.
(defmagick* MagickMorphImages :
  _MagickWand (num-of-frames : _ulong) -> _MagickWand)

;; MagickMosaicImages inlays an image sequence to form a single coherent
;; picture.  It returns a wand with each image in the sequence composited at
;; the location defined by the page offset of the image.
(defmagick* MagickMosaicImages :
  _MagickWand -> _MagickWand)

;; MagickMotionBlurImage simulates motion blur.  We convolve the image with a
;; Gaussian operator of the given radius and standard deviation (sigma).  For
;; reasonable results, radius should be larger than sigma.  Use a radius of 0
;; and MagickMotionBlurImage selects a suitable radius for you.  Angle gives
;; the angle of the blurring motion.
(defmagick* MagickMotionBlurImage :
  _MagickWand (radius : _double*) (sigma : _double*) (angle : _double*)
  -> _status)

;; MagickNegateImage negates the colors in the reference image.  The Grayscale
;; option means that only grayscale values within the image are negated.
(defmagick* MagickNegateImage :
  _MagickWand (gray? : _bool) -> _status)

;; MagickNegateImageChannel negates the colors in the specified channel of the
;; reference image.  The Grayscale option means that only grayscale values
;; within the image are negated.  You can also reduce the influence of a
;; particular channel with a gamma value of 0.
(defmagick* MagickNegateImageChannel :
  _MagickWand _ChannelType (gray? : _bool) -> _status)

;; MagickNextImage associates the next image in the image list with a magick
;; wand.
(defmagick* MagickNextImage :
  _MagickWand -> _status)

;; MagickNormalizeImage enhances the contrast of a color image by adjusting the
;; pixels color to span the entire range of colors available
(defmagick* MagickNormalizeImage :
  _MagickWand -> _status)

;; MagickOilPaintImage applies a special effect filter that simulates an oil
;; painting.  Each pixel is replaced by the most frequent color occurring in a
;; circular region defined by radius.
(defmagick* MagickOilPaintImage :
  _MagickWand _double* -> _status)

;; MagickOpaqueImage changes any pixel that matches color with the color
;; defined by fill.
(defmagick* MagickOpaqueImage :
  _MagickWand (target : _PixelWand) (fill : _PixelWand) (fuzz : _double*)
  -> _status)

;; MagickPingImage is like MagickReadImage except the only valid information
;; returned is the image width, height, size, and format.  It is designed to
;; efficiently obtain this information from a file without reading the entire
;; image sequence into memory.
(defmagick* MagickPingImage :
  _MagickWand _file -> _status)

;; MagickPosterizeImage reduces the image to a limited number of color level.
(defmagick* MagickPosterizeImage :
  _MagickWand (levels : _ulong) (dither? : _bool) -> _status)

;; MagickPreviewImages tiles 9 thumbnails of the specified image with an image
;; processing operation applied at varying strengths.  This is helpful to
;; quickly pin-point an appropriate parameter for an image processing
;; operation.
(defmagick* MagickPreviewImages :
  _MagickWand _PreviewType -> _MagickWand)

;; MagickPreviousImage assocates the previous image in an image list with the
;; magick wand.
(defmagick* MagickPreviousImage :
  _MagickWand -> _status)

;; Use MagickProfileImage to add or remove a ICC, IPTC, or generic profile from
;; an image.  If the profile is #f (NULL), it is removed from the image
;; otherwise added.  Use a name of '*' and a profile of NULL to remove all
;; profiles from the image.
(defmagick* MagickProfileImage :
  _MagickWand (profile-name : _string)
  (profile : _bytes) (_ulong = (bytes-length profile))
  -> _status)

;; MagickQuantizeImage analyzes the colors within a reference image and chooses
;; a fixed number of colors to represent the image.  The goal of the algorithm
;; is to minimize the color difference between the input and output image while
;; minimizing the processing time.
(defmagick* MagickQuantizeImage :
  _MagickWand (num-colors : _ulong) _ColorspaceType (tree-depth : _ulong)
  (dither? : _bool) (measure-error? : _bool)
  -> _status)

;; MagickQuantizeImages analyzes the colors within a sequence of images and
;; chooses a fixed number of colors to represent the image.  The goal of the
;; algorithm is to minimize the color difference between the input and output
;; image while minimizing the processing time.
(defmagick* MagickQuantizeImages :
  _MagickWand
  (num-colors : _ulong) _ColorspaceType (tree-depth : _ulong)
  (dither? : _bool) (measure-error? : _bool)
  -> _status)

;; MagickQueryConfigureOptions returns any configure options that match the
;; specified pattern (e.g. "*" for all).  Options include NAME, VERSION,
;; LIB_VERSION, etc.
(defmagick* MagickQueryConfigureOptions :
  (pattern : _string) (len : (_ptr o _ulong)) -> (_list o _string len))

;; MagickQueryFontMetrics returns a 7 element list representing the following
;; font metrics: char-width, char-height, ascender, descender, text-width,
;; text-height, max-horizontal-advance.
(defmagick* MagickQueryFontMetrics :
  _MagickWand _DrawingWand (text : _string) -> (_list o _double* 7))

;; MagickQueryFonts returns any font that match the specified pattern (e.g. "*"
;; for all).
(defmagick* MagickQueryFonts :
  (pattern : _string) (len : (_ptr o _ulong)) -> (_list o _string len))

;; MagickQueryFormats returns any image formats that match the specified
;; pattern (e.g. "*" for all).
(defmagick* MagickQueryFormats :
  (pattern : _string) (len : (_ptr o _ulong)) -> (_list o _string len))

;; MagickRadialBlurImage radial blurs an image.
(defmagick* MagickRadialBlurImage :
  _MagickWand (angle : _double*) -> _status)

;; MagickRaiseImage creates a simulated three-dimensional button-like effect by
;; lightening and darkening the edges of the image.  Members width and height
;; of raise_info define the width of the vertical and horizontal edge of the
;; effect.
(defmagick* MagickRaiseImage :
  _MagickWand
  (width : _ulong) (height : _ulong) (x : _long) (y : _long) (raise? : _bool)
  -> _status)

;; MagickReadImage reads an image or image sequence.
;; Extended: the wand argument is optional -- will be made and returned if not
;; given.
(defmagick* MagickReadImage :
  (arg . args) ::
  (w : _MagickWand = (if (null? args) (NewMagickWand) arg))
  (_file = (if (null? args) arg (car args)))
  -> _status
  -> (if (null? args) w (void)))

;; MagickReadImageBlob reads an image or image sequence from a blob.
(defmagick* MagickReadImageBlob :
  _MagickWand (blob : _bytes) (_ulong = (bytes-length blob)) -> _status)

;; MagickReadImageFile reads an image or image sequence from an open file
;; descriptor.
(defmagick* MagickReadImageFile :
  _MagickWand (FILE* : _pointer) -> _status)

;; MagickReduceNoiseImage smooths the contours of an image while still
;; preserving edge information.  The algorithm works by replacing each pixel
;; with its neighbor closest in value.  A neighbor is defined by radius.  Use a
;; radius of 0 and MagickReduceNoiseImage selects a suitable radius for you.
(defmagick* MagickReduceNoiseImage :
  _MagickWand (radius : _double*) -> _status)

;; MagickRelinquishMemory relinquishes memory resources returned by such
;; methods as MagickDescribeImage, MagickGetException, etc.
;; Looks like this is not needed with a GC (tried it with the result of
;; MagickDescribeImage many times, got an error).
;; (defmagick* MagickRelinquishMemory :
;;   _pointer -> _status)

;; MagickRemoveImage removes an image from the image list.
(defmagick* MagickRemoveImage :
  _MagickWand -> _status)

;; MagickRemoveImageProfile removes the named image profile and returns it.
(defmagick* MagickRemoveImageProfile :
  _MagickWand (profile-name : _string) (len : (_ptr o _ulong))
  -> (_bytes o len))

;; MagickResampleImage resample image to desired resolution.  Most of the
;; filters are FIR (finite impulse response), however, Bessel, Gaussian, and
;; Sinc are IIR (infinite impulse response).  Bessel and Sinc are windowed
;; (brought down to zero) with the Blackman filter.
(defmagick* MagickResampleImage :
  _MagickWand
  (x-res : _double*) (y-res : _double*) _FilterTypes (blur-factor : _double*)
  -> _status)

;; MagickResetIterator resets the wand iterator.  Use it in conjunction with
;; MagickNextImage to iterate over all the images in a wand container.
(defmagick* MagickResetIterator :
  _MagickWand -> _void)

;; MagickResizeImage scales an image to the desired dimensions with some
;; filter.  Most of the filters are FIR (finite impulse response), however,
;; Bessel, Gaussian, and Sinc are IIR (infinite impulse response).  Bessel and
;; Sinc are windowed (brought down to zero) with the Blackman filter.
(defmagick* MagickResizeImage :
  _MagickWand
  (width : _ulong) (height : _ulong) _FilterTypes (blur-factor : _double*)
  -> _status)

;; MagickRollImage offsets an image as defined by x_offset and y_offset.
(defmagick* MagickRollImage :
  _MagickWand (x-offset : _long) (y-offset : _long) -> _status)

;; MagickRotateImage rotates an image the specified number of degrees.  Empty
;; triangles left over from rotating the image are filled with the background
;; color.
(defmagick* MagickRotateImage :
  _MagickWand (background : _PixelWand) (degrees : _double*) -> _status)

;; MagickSampleImage scales an image to the desired dimensions with pixel
;; sampling.  Unlike other scaling methods, this method does not introduce any
;; additional color into the scaled image.
(defmagick* MagickSampleImage :
  _MagickWand (width : _ulong) (height : _ulong) -> _status)

;; MagickScaleImage scales the size of an image to the given dimensions.
(defmagick* MagickScaleImage :
  _MagickWand (width : _ulong) (height : _ulong) -> _status)

;; MagickSeparateImageChannel separates a channel from the image and returns a
;; grayscale image.  A channel is a particular color component of each pixel in
;; the image.
(defmagick* MagickSeparateImageChannel :
  _MagickWand _ChannelType -> _status)

;; MagickSetFilename sets the filename before you read or write an image file.
(defmagick* MagickSetFilename :
  _MagickWand _file -> _status)

;; MagickSetImage replaces the last image returned by MagickSetImageIndex,
;; MagickNextImage, MagickPreviousImage with the images from the specified
;; wand.
(defmagick* MagickSetImage :
  _MagickWand (set-wand : _MagickWand) -> _status)

;; MagickSetImageBackgroundColor sets the image background color.
(defmagick* MagickSetImageBackgroundColor :
  _MagickWand (background : _PixelWand) -> _status)

;; MagickSetImageBluePrimary sets the image chromaticity blue primary point.
(defmagick* MagickSetImageBluePrimary :
  _MagickWand (x : _double*) (y : _double*) -> _status)

;; MagickSetImageBorderColor sets the image border color.
(defmagick* MagickSetImageBorderColor :
  _MagickWand (border-color : _PixelWand) -> _status)

;; MagickSetImageChannelDepth sets the depth of a particular image channel.
(defmagick* MagickSetImageChannelDepth :
  _MagickWand _ChannelType (depth : _ulong) -> _status)

;; MagickSetImageColormapColor sets the color of the specified colormap index.
(defmagick* MagickSetImageColormapColor :
  _MagickWand (index : _ulong) _PixelWand -> _status)

;; MagickSetImageColorspace sets the image colorspace.
(defmagick* MagickSetImageColorspace :
  _MagickWand _ColorspaceType -> _status)

;; MagickSetImageCompose sets the image composite operator, useful for
;; specifying how to composite the image thumbnail when using the
;; MagickMontageImage method.
(defmagick* MagickSetImageCompose :
  _MagickWand _CompositeOperator -> _status)

;; MagickSetImageCompression sets the image compression.
(defmagick* MagickSetImageCompression :
  _MagickWand _CompressionType -> _status)

;; MagickSetImageCompressionQuality sets the image compression quality.
(defmagick* MagickSetImageCompressionQuality :
  _MagickWand (quality : _ulong) -> _status)

;; MagickSetImageDelay sets the image delay.
(defmagick* MagickSetImageDelay :
  _MagickWand (delay : _ulong) -> _status)

;; MagickSetImageDepth sets the image depth.
(defmagick* MagickSetImageDepth :
  _MagickWand (depth : _ulong) -> _status)

;; MagickSetImageDispose sets the image disposal method.
(defmagick* MagickSetImageDispose :
  _MagickWand _DisposeType -> _status)

;; MagickSetImageFilename sets the filename of a particular image in a
;; sequence.
(defmagick* MagickSetImageFilename :
  _MagickWand _file -> _status)

;; MagickSetImageGamma sets the image gamma.
(defmagick* MagickSetImageGamma :
  _MagickWand (gamma : _double*) -> _status)

;; MagickSetImageGreenPrimary sets the image chromaticity green primary point.
(defmagick* MagickSetImageGreenPrimary :
  _MagickWand (y : _double*) (x : _double*) -> _status)

;; MagickSetImageIndex replaces the last image returned by MagickSetImageIndex,
;; MagickNextImage, MagickPreviousImage with the images from the specified
;; wand.
(defmagick* MagickSetImageIndex :
  _MagickWand (index : _long) -> _status)

;; MagickSetImageInterlaceScheme sets the image interlace scheme.
(defmagick* MagickSetImageInterlaceScheme :
  _MagickWand _InterlaceType -> _status)

;; MagickSetImageIterations sets the image iterations.
(defmagick* MagickSetImageIterations :
  _MagickWand (iterations : _ulong) -> _status)

;; MagickSetImageMatteColor sets the image matte color.
(defmagick* MagickSetImageMatteColor :
  _MagickWand (matte : _PixelWand) -> _status)

;; MagickSetImageOption associates one or options with a particular image
;; format (e.g. (MagickSetImageOption wand "jpeg" "perserve" "yes")).
(defmagick* MagickSetImageOption :
  _MagickWand (format : _string) (key : _string) (value : _string) -> _status)

;; MagickSetImagePixels accepts pixel data and stores it in the image at the
;; location you specify.  The method returns False on success otherwise True if
;; an error is encountered.  The pixel data can be either char, short int, int,
;; long, float, or double in the order specified by map.  Suppose your want
;; want to upload the first scanline of a 640x480 image from character data in
;; red-green-blue order:
;;     (MagickSetImagePixels wand 0 0 640 1 "RGB" 'CharPixel pixels)
;; `map' is a string that reflects the expected ordering of the pixel array.
;; It can be any combination or order of R = red, G = green, B = blue,
;; A = alpha (0 is transparent), O = opacity (0 is opaque), C = cyan,
;; Y = yellow, M = magenta, K = black, I = intensity (for grayscale), P = pad.
;; Note: the scheme interface uses a list of rows, each a list of values for
;; each element in the map.  Also, the map here should not have the type
;; character as in the MagickWand API (it is a documentation bug), and there is
;; no need for width and height too.
(defmagick* MagickSetImagePixels :
  (w x y map storage-type matrix) ::
  (w : _MagickWand) (x : _long) (y : _long)
  (width : _ulong = (length (car matrix))) (height : _ulong = (length matrix))
  (map : _string) (storage-type : _StorageType)
  (_pointer = (let* ([size   (* width height (string-length map))]
                     [type   (StorageType->type storage-type)]
                     [block  (malloc size type)])
                (let loop ([m matrix] [n 0])
                  (cond [(null? m) n]
                        [(pair? m) (loop (cdr m) (loop (car m) n))]
                        [else (ptr-set! block type n m) (add1 n)]))
                block))
  -> _status)

;; MagickSetImageProfile adds a named profile to the magick wand.  If a profile
;; with the same name already exists, it is replaced.  This method differs from
;; the MagickProfileImage method in that it does not apply any CMS color
;; profiles.
(defmagick* MagickSetImageProfile :
  _MagickWand (profile-name : _string)
  (profile : _bytes) (_ulong = (bytes-length profile))
  -> _status)

;; MagickSetImageRedPrimary sets the image chromaticity red primary point.
(defmagick* MagickSetImageRedPrimary :
  _MagickWand (x : _double*) (y : _double*) -> _status)

;; MagickSetImageRenderingIntent sets the image rendering intent.
(defmagick* MagickSetImageRenderingIntent :
  _MagickWand _RenderingIntent -> _status)

;; MagickSetImageResolution sets the image resolution.
(defmagick* MagickSetImageResolution :
  _MagickWand (res-x : _double*) (res-y : _double*) -> _status)

;; MagickSetImageScene sets the image scene.
(defmagick* MagickSetImageScene :
  _MagickWand (schene-number : _ulong) -> _status)

;; MagickSetImageType sets the image type.
(defmagick* MagickSetImageType :
  _MagickWand _ImageType -> _status)

;; MagickSetImageUnits sets the image units of resolution.
(defmagick* MagickSetImageUnits :
  _MagickWand _ResolutionType -> _status)

;; MagickSetImageVirtualPixelMethod sets the image virtual pixel method.
(defmagick* MagickSetImageVirtualPixelMethod :
  _MagickWand _VirtualPixelMethod -> _status)

;; MagickSetImageWhitePoint sets the image chromaticity white point.
(defmagick* MagickSetImageWhitePoint :
  _MagickWand (x : _double*) (y : _double*) -> _status)

;; MagickSetInterlaceScheme sets the image compression.
(defmagick* MagickSetInterlaceScheme :
  _MagickWand _InterlaceType -> _status)

;; MagickSetPassphrase sets the passphrase.
(defmagick* MagickSetPassphrase :
  _MagickWand (passphrase : _string) -> _status)

;; MagickSetResourceLimit sets the limit for a particular resource in
;; megabytes.
(defmagick* MagickSetResourceLimit :
  _ResourceType (limit : _ulong) -> _status)

;; MagickSetSamplingFactors sets the image sampling factors.
(defmagick* MagickSetSamplingFactors :
  (w factors) ::
  (w : _MagickWand) (_ulong = (length factors)) (factors : (_list i _double*))
  -> _status)

;; MagickSetSize sets the size of the magick wand.  Set it before you read a
;; raw image format such as RGB, GRAY, or CMYK.
(defmagick* MagickSetSize :
  _MagickWand (width : _ulong) (height : _ulong) -> _status)

;; MagickSharpenImage sharpens an image.  We convolve the image with a Gaussian
;; operator of the given radius and standard deviation (sigma).  For reasonable
;; results, the radius should be larger than sigma.  Use a radius of 0 and
;; SharpenImage selects a suitable radius for you.
(defmagick* MagickSharpenImage :
  _MagickWand (radius : _double*) (sigma : _double*) -> _status)

;; MagickSharpenImageChannel sharpens one or more image channels.  We convolve
;; the image cnannel with a gaussian operator of the given radius and standard
;; deviation (sigma).  For reasonable results, the radius should be larger than
;; sigma.  Use a radius of 0 and GaussinSharpenImageChannel selects a suitable
;; radius for you.
(defmagick* MagickSharpenImageChannel :
  _MagickWand _ChannelType (radius : _double*) (sigma : _double*) -> _status)

;; MagickShaveImage shaves pixels from the image edges.  It allocates the
;; memory necessary for the new Image structure and returns a pointer to the
;; new image.
(defmagick* MagickShaveImage :
  _MagickWand (width : _ulong) (height : _ulong) -> _status)

;; MagickShearImage slides one edge of an image along the X or Y axis, creating
;; a parallelogram.  An X direction shear slides an edge along the X axis,
;; while a Y direction shear slides an edge along the Y axis.  The amount of
;; the shear is controlled by a shear angle.  For X direction shears, x_shear
;; is measured relative to the Y axis, and similarly, for Y direction shears
;; y_shear is measured relative to the X axis.  Empty triangles left over from
;; shearing the image are filled with the background color.
(defmagick* MagickShearImage :
  _MagickWand (background : _PixelWand)
  (x-shear : _double*) (y-shear : _double*)
  -> _status)

;; MagickSolarizeImage applies a special effect to the image, similar to the
;; effect achieved in a photo darkroom by selectively exposing areas of photo
;; sensitive paper to light.  Threshold ranges from 0 to MaxRGB and is a
;; measure of the extent of the solarization.
(defmagick* MagickSolarizeImage :
  _MagickWand (threshold : _double*) -> _status)

;; MagickSpliceImage splices a solid color into the image.
(defmagick* MagickSpliceImage :
  _MagickWand (width : _ulong) (height : _ulong) (x : _long) (y : _long)
  -> _status)

;; MagickSpreadImage is a special effects method that randomly displaces each
;; pixel in a block defined by the radius parameter.
(defmagick* MagickSpreadImage :
  _MagickWand (radius : _double*) -> _status)

;; Use MagickSteganoImage to hide a digital watermark within the image.
;; Recover the hidden watermark later to prove that the authenticity of an
;; image.  Offset defines the start position within the image to hide the
;; watermark.
(defmagick* MagickSteganoImage :
  _MagickWand (watermark : _MagickWand) (offset : _long) -> _MagickWand)

;; MagickStereoImage composites two images and produces a single image that is
;; the composite of a left and right image of a stereo pair.
(defmagick* MagickStereoImage :
  _MagickWand (offset : _MagickWand) -> _MagickWand)

;; MagickStripImage strips an image of all profiles and comments.
(defmagick* MagickStripImage :
  _MagickWand -> _status)

;; MagickSwirlImage swirls the pixels about the center of the image, where
;; degrees indicates the sweep of the arc through which each pixel is moved.
;; You get a more dramatic effect as the degrees move from 1 to 360.
(defmagick* MagickSwirlImage :
  _MagickWand (degrees : _double*) -> _status)

;; MagickTextureImage repeatedly tiles the texture image across and down the
;; image canvas.
(defmagick* MagickTextureImage :
  _MagickWand (texture : _MagickWand) -> _MagickWand)

;; MagickThresholdImage changes the value of individual pixels based on the
;; intensity of each pixel compared to threshold.  The result is a
;; high-contrast, two color image.
(defmagick* MagickThresholdImage :
  _MagickWand _double* -> _status)

;; MagickThresholdImageChannel changes the value of individual pixel component
;; based on the intensity of each pixel compared to threshold.  The result is a
;; high-contrast, two color image.
(defmagick* MagickThresholdImageChannel :
  _MagickWand _ChannelType (threshold : _double*) -> _status)

;; MagickTintImage applies a color vector to each pixel in the image.  The
;; length of the vector is 0 for black and white and at its maximum for the
;; midtones.  The vector weighting function is
;; f(x)=(1-(4.0*((x-0.5)*(x-0.5)))).
(defmagick* MagickTintImage :
  _MagickWand (tint : _PixelWand) (opacity : _PixelWand) -> _status)

;; MagickTransformImage is a convenience method that behaves like
;; MagickResizeImage or MagickCropImage but accepts scaling and/or cropping
;; information as a region geometry specification.  If the operation fails, the
;; original image handle is returned.
(defmagick* MagickTransformImage :
  _MagickWand (crop-geometry : _string) (image-geometry : _string)
  -> _MagickWand)

;; MagickTransparentImage changes any pixel that matches color with the color
;; defined by fill.
(defmagick* MagickTransparentImage :
  _MagickWand (target : _PixelWand) (opacity : _Quantum) (fuzz : _double*)
  -> _status)

;; MagickTrimImage remove edges that are the background color from the image.
(defmagick* MagickTrimImage :
  _MagickWand (fuzz : _double*) -> _status)

;; MagickUnsharpMaskImage sharpens an image.  We convolve the image with a
;; Gaussian operator of the given radius and standard deviation (sigma).  For
;; reasonable results, radius should be larger than sigma.  Use a radius of 0
;; and UnsharpMaskImage selects a suitable radius for you.
(defmagick* MagickUnsharpMaskImage :
  _MagickWand (radius : _double*) (sigma : _double*)
  (amount-precentage : _double*) (threshold : _double*)
  -> _status)

;; MagickWaveImage creates a "ripple" effect in the image by shifting the
;; pixels vertically along a sine wave whose amplitude and wavelength is
;; specified by the given parameters.
(defmagick* MagickWaveImage :
  _MagickWand (amplitude : _double*) (wave-length : _double*) -> _status)

;; MagickWhiteThresholdImage is like ThresholdImage but forces all pixels above
;; the threshold into white while leaving all pixels below the threshold
;; unchanged.
(defmagick* MagickWhiteThresholdImage :
  _MagickWand _PixelWand -> _status)

;; MagickWriteImage writes an image.
(defmagick* MagickWriteImage :
  _MagickWand _file -> _status)

;; MagickGetImageBlob implements direct to memory image formats.  It returns
;; the image as a blob and its length.  The magick member of the Image
;; structure determines the format of the returned blob (GIF, JPEG, PNG, etc.)
(defmagick* MagickGetImageBlob :
  _MagickWand (len : (_ptr o _ulong)) -> (_bytes o len))

;; MagickWriteImageFile writes an image to an open file descriptor.
(defmagick* MagickWriteImageFile :
  _MagickWand (FILE* : _pointer) -> _status)

;; MagickWriteImages writes an image or image sequence.
(defmagick* MagickWriteImages :
  _MagickWand _file (adjoin? : _bool) -> _status)

;; NewMagickWand returns a wand required for all other methods in the API.
(defmagick* NewMagickWand :
  -> _MagickWand)

;; ===== PixelIterator API ====================================================

;; DestroyPixelIterator deallocates resources associated with a PixelIterator.
(defmagick DestroyPixelIterator :
  _PixelIterator -> _void)

;; NewPixelIterator returns a new pixel iterator.
(defmagick* NewPixelIterator :
  (w : _MagickWand) -> (pi : _PixelIterator)
  -> (begin (set-PixelIterator-width! pi (MagickGetImageWidth w)) pi))

;; NewPixelRegionIterator returns a new pixel iterator.
(defmagick* NewPixelRegionIterator :
  _MagickWand (x : _long) (y : _long) (width : _ulong) (height : _ulong)
  -> (pi : _PixelIterator)
  -> (begin (set-PixelIterator-width! pi width) pi))

;; PixelIteratorGetException returns the severity, reason, and description of
;; any error that occurs when using other methods in this API.
(defmagick* PixelIteratorGetException :
  _PixelIterator (severity : (_ptr o _MagickExceptionType))
  -> (message : _string)
  -> (unless (eq? severity 'UndefinedException)
       (error 'PixelIterator "(~a) ~a" severity message)))

;; PixelGetNextRow returns the next row from the pixel iterator.
(defmagick* PixelGetNextRow :
  (pi : _PixelIterator) -> (_list o _PixelWand (PixelIterator-width pi)))

;; PixelResetIterator resets the pixel iterator.  Use it in conjunction with
;; PixelGetNextPixel to iterate over all the pixels in a pixel container.
(defmagick* PixelResetIterator :
  _PixelIterator -> _void)

;; PixelSetIteratorRow set the pixel iterator row.
(defmagick* PixelSetIteratorRow :
  (pi : _PixelIterator) (row : _long) -> _status)

;; PixelSyncIterator syncs the pixel iterator.
(defmagick* PixelSyncIterator :
  _PixelIterator -> _status)

;; ===== PixelWand API ========================================================

;; PixelGetException returns the severity, reason, and description of any error
;; that occurs when using other methods in this API (as an exception).
(defmagick* PixelGetException :
  _MagickWand (severity : (_ptr o _MagickExceptionType)) -> (message : _string)
  -> (unless (eq? severity 'UndefinedException)
       (error 'PixelWand "(~a) ~a" severity message)))

;; DestroyPixelWand deallocates resources associated with a PixelWand.
;; Intended for internal use only, must be defined after the above.
(defmagick DestroyPixelWand :
  _PixelWand -> _void)

;; DestroyPixelWands deallocates resources associated with an array of
;; pixel wands.
;; * There is no need for this

;; NewPixelWand returns a new pixel wand.
;; Extended: can get a color name to use for the new pixel wand, or an RGB list
;; (integers or floats determine the method to use for setting the values), or
;; a CMYK list.  (See also the _PixelWand type definition).
(defmagick* NewPixelWand :
  init-color ::
  -> (p : _PixelWand)
  -> (let ([color (and (pair? init-color) (car init-color))])
       (define (err) (error 'NewPixelWand "bad initial color: ~e" color))
       (cond [(null? init-color)]
             [(string? color) (PixelSetColor p color)]
             [(list? color)
              (let ([len (length color)]
                    [ints? (andmap integer? color)]
                    [flts? (andmap inexact? color)])
                (if (or ints? flts?)
                  (let ([len (if flts? (- len) len)])
                    (case len
                      [(3)  (PixelSetRedQuantum     p (car color))
                            (PixelSetGreenQuantum   p (cadr color))
                            (PixelSetBlueQuantum    p (caddr color))]
                      [(-3) (PixelSetRed            p (car color))
                            (PixelSetGreen          p (cadr color))
                            (PixelSetBlue           p (caddr color))]
                      [(4)  (PixelSetCyanQuantum    p (car color))
                            (PixelSetMagentaQuantum p (cadr color))
                            (PixelSetYellowQuantum  p (caddr color))
                            (PixelSetBlackQuantum   p (cadddr color))]
                      [(-4) (PixelSetCyan           p (car color))
                            (PixelSetMagenta        p (cadr color))
                            (PixelSetYellow         p (caddr color))
                            (PixelSetBlack          p (cadddr color))]
                      [else (err)]))
                  (err)))]
             [else (err)])
            p))

;; NewPixelWands returns an array of pixel wands.
;; * There is no need for this

;; PixelGetBlack returns the normalized black color of the pixel wand.
(defmagick* PixelGetBlack :
  _PixelWand -> _double*)

;; PixelGetBlackQuantum returns the black color of the pixel wand.  The color
;; is in the range of [0..MaxRGB].
(defmagick* PixelGetBlackQuantum :
  _PixelWand -> _Quantum)

;; PixelGetBlue returns the normalized blue color of the pixel wand.
(defmagick* PixelGetBlue :
  _PixelWand -> _double*)

;; PixelGetBlueQuantum returns the blue color of the pixel wand.  The color is
;; in the range of [0..MaxRGB].
(defmagick* PixelGetBlueQuantum :
  _PixelWand -> _Quantum)

;; PixelGetColorAsString gets the color of the pixel wand.
(defmagick* PixelGetColorAsString :
  _PixelWand -> _string)

;; PixelGetColorCount returns the color count associated with this color.
(defmagick* PixelGetColorCount :
  _PixelWand -> _ulong)

;; PixelGetCyan returns the normalized cyan color of the pixel wand.
(defmagick* PixelGetCyan :
  _PixelWand -> _double*)

;; PixelGetCyanQuantum returns the cyan color of the pixel wand.  The color is
;; in the range of [0..MaxRGB].
(defmagick* PixelGetCyanQuantum :
  _PixelWand -> _Quantum)

;; PixelGetGreen returns the normalized green color of the pixel wand.
(defmagick* PixelGetGreen :
  _PixelWand -> _double*)

;; PixelGetGreenQuantum returns the green color of the pixel wand.  The color
;; is in the range of [0..MaxRGB].
(defmagick* PixelGetGreenQuantum :
  _PixelWand -> _Quantum)

;; PixelGetIndex returns the colormap index from the pixel wand.
(defmagick* PixelGetIndex :
  _PixelWand -> _IndexPacket)

;; PixelGetMagenta returns the normalized magenta color of the pixel wand.
(defmagick* PixelGetMagenta :
  _PixelWand -> _double*)

;; PixelGetMagentaQuantum returns the magenta color of the pixel wand.  The
;; color is in the range of [0..MaxRGB].
(defmagick* PixelGetMagentaQuantum :
  _PixelWand -> _Quantum)

;; PixelGetOpacity returns the normalized opacity color of the pixel wand.
(defmagick* PixelGetOpacity :
  _PixelWand -> _double*)

;; PixelGetOpacityQuantum returns the opacity color of the pixel wand.  The
;; color is in the range of [0..MaxRGB].
(defmagick* PixelGetOpacityQuantum :
  _PixelWand -> _Quantum)

;; PixelGetQuantumColor gets the color of the pixel wand.
(defmagick* PixelGetQuantumColor :
  _PixelWand (color : _PixelPacket = (NewPixelPacket)) -> _void
  -> color)

;; PixelGetRed returns the normalized red color of the pixel wand.
(defmagick* PixelGetRed :
  _PixelWand -> _double*)

;; PixelGetRedQuantum returns the red color of the pixel wand.  The color is in
;; the range of [0..MaxRGB].
(defmagick* PixelGetRedQuantum :
  _PixelWand -> _Quantum)

;; PixelGetYellow returns the normalized yellow color of the pixel wand.
(defmagick* PixelGetYellow :
  _PixelWand -> _double*)

;; PixelGetYellowQuantum returns the yellow color of the pixel wand.  The color
;; is in the range of [0..MaxRGB].
(defmagick* PixelGetYellowQuantum :
  _PixelWand -> _Quantum)

;; PixelSetBlack sets the normalized black color of the pixel wand.
(defmagick* PixelSetBlack :
  _PixelWand (black : _double*) -> _void)

;; PixelSetBlackQuantum sets the black color of the pixel wand.  The color must
;; be in the range of [0..MaxRGB].
(defmagick* PixelSetBlackQuantum :
  _PixelWand (black : _Quantum) -> _void)

;; PixelSetBlue sets the normalized blue color of the pixel wand.
(defmagick* PixelSetBlue :
  _PixelWand (blue : _double*) -> _void)

;; PixelSetBlueQuantum sets the blue color of the pixel wand.  The color must
;; be in the range of [0..MaxRGB].
(defmagick* PixelSetBlueQuantum :
  _PixelWand (blue : _Quantum) -> _void)

;; PixelSetColor sets the color of the pixel wand with a string (e.g. "blue",
;; "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)", etc.).
(defmagick* PixelSetColor :
  _PixelWand (color : _string) -> _status)

;; PixelSetColorCount sets the color count of the pixel wand.
(defmagick* PixelSetColorCount :
  _PixelWand (count : _ulong) -> _void)

;; PixelSetCyan sets the normalized cyan color of the pixel wand.
(defmagick* PixelSetCyan :
  _PixelWand (cyan : _double*) -> _void)

;; PixelSetCyanQuantum sets the cyan color of the pixel wand.  The color must
;; be in the range of [0..MaxRGB].
(defmagick* PixelSetCyanQuantum :
  _PixelWand (cyan : _Quantum) -> _void)

;; PixelSetGreen sets the normalized green color of the pixel wand.
(defmagick* PixelSetGreen :
  _PixelWand (green : _double*) -> _void)

;; PixelSetGreenQuantum sets the green color of the pixel wand.  The color must
;; be in the range of [0..MaxRGB].
(defmagick* PixelSetGreenQuantum :
  _PixelWand (green : _Quantum) -> _void)

;; PixelSetIndex sets the colormap index of the pixel wand.
(defmagick* PixelSetIndex :
  _PixelWand _IndexPacket -> _void)

;; PixelSetMagenta sets the normalized magenta color of the pixel wand.
(defmagick* PixelSetMagenta :
  _PixelWand (magenta : _double*) -> _void)

;; PixelSetMagentaQuantum sets the magenta color of the pixel wand.  The color
;; must be in the range of [0..MaxRGB].
(defmagick* PixelSetMagentaQuantum :
  _PixelWand (magenta : _Quantum) -> _void)

;; PixelSetOpacity sets the normalized opacity color of the pixel wand.
(defmagick* PixelSetOpacity :
  _PixelWand (opacity : _double*) -> _void)

;; PixelSetOpacityQuantum sets the opacity color of the pixel wand.  The color
;; must be in the range of [0..MaxRGB].
(defmagick* PixelSetOpacityQuantum :
  _PixelWand (opacity : _Quantum) -> _void)

;; PixelSetQuantumColor sets the color of the pixel wand.
(defmagick* PixelSetQuantumColor :
  _PixelWand _PixelPacket -> _void)

;; PixelSetRed sets the normalized red color of the pixel wand.
(defmagick* PixelSetRed :
  _PixelWand (red : _double*) -> _void)

;; PixelSetRedQuantum sets the red color of the pixel wand.  The color must be
;; in the range of [0..MaxRGB].
(defmagick* PixelSetRedQuantum :
  _PixelWand (red : _Quantum) -> _void)

;; PixelSetYellow sets the normalized yellow color of the pixel wand.
(defmagick* PixelSetYellow :
  _PixelWand (yellow : _double*) -> _void)

;; PixelSetYellowQuantum sets the yellow color of the pixel wand.  The color
;; must be in the range of [0..MaxRGB].
(defmagick* PixelSetYellowQuantum :
  _PixelWand (yellow : _Quantum) -> _void)

;; ===== DrawingWand API ======================================================

;; DrawGetException returns the severity, reason, and description of any error
;; that occurs when using other methods in this API.
(defmagick* DrawGetException :
  _DrawingWand (severity : (_ptr o _MagickExceptionType))
  -> (message : _string)
  -> (unless (eq? severity 'UndefinedException)
       (error 'DrawingWand "(~a) ~a" severity message)))

;; DestroyDrawingWand frees all resources associated with the drawing wand.
;; Once the drawing wand has been freed, it should not be used any further
;; unless it re-allocated.
(defmagick DestroyDrawingWand :
  _DrawingWand -> _void)

;; DrawAnnotation draws text on the image.
(defmagick* DrawAnnotation :
  _DrawingWand (x : _double*) (y : _double*) (text : _string) -> _void)

;; DrawAffine adjusts the current affine transformation matrix with the
;; specified affine transformation matrix.  Note that the current affine
;; transform is adjusted rather than replaced.
(defmagick* DrawAffine :
  _DrawingWand _AffineMatrix -> _void)

;; DrawAllocateWand allocates an initial drawing wand which is an opaque handle
;; required by the remaining drawing methods.
(defmagick* DrawAllocateWand :
  _DrawInfo _Image -> _DrawingWand)

;; DrawArc draws an arc falling within a specified bounding rectangle on the
;; image.
(defmagick* DrawArc :
  _DrawingWand
  (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  (deg1 : _double*) (deg2 : _double*)
  -> _void)

;; DrawBezier draws a bezier curve through a set of points on the image.
(defmagick* DrawBezier :
  (d points) ::
  (d : _DrawingWand) (_ulong = (length points)) (points : _Points) -> _void)

;; DrawCircle draws a circle on the image.
(defmagick* DrawCircle :
  _DrawingWand
  (x : _double*) (y : _double*)
  (perimeter-x : _double*) (perimeter-y : _double*)
  -> _void)

;; DrawGetClipPath obtains the current clipping path ID.  The value returned
;; must be deallocated by the user when it is no longer needed.
(defmagick* DrawGetClipPath :
  _DrawingWand -> _string)

;; DrawSetClipPath associates a named clipping path with the image.  Only the
;; areas drawn on by the clipping path will be modified as long as it remains
;; in effect.
(defmagick* DrawSetClipPath :
  _DrawingWand (clip_path : _string) -> _void)

;; DrawGetClipRule returns the current polygon fill rule to be used by the
;; clipping path.
(defmagick* DrawGetClipRule :
  _DrawingWand -> _FillRule)

;; DrawSetClipRule set the polygon fill rule to be used by the clipping path.
(defmagick* DrawSetClipRule :
  _DrawingWand _FillRule -> _void)

;; DrawGetClipUnits returns the interpretation of clip path units.
(defmagick* DrawGetClipUnits :
  _DrawingWand -> _ClipPathUnits)

;; DrawSetClipUnits sets the interpretation of clip path units.
(defmagick* DrawSetClipUnits :
  _DrawingWand _ClipPathUnits -> _void)

;; DrawColor draws color on image using the current fill color, starting at
;; specified position, and using specified paint method.  The available paint
;; methods are:
;;   PointMethod: Recolors the target pixel
;;   ReplaceMethod: Recolor any pixel that matches the target pixel.
;;   FloodfillMethod: Recolors target pixels and matching neighbors.
;;   FillToBorderMethod: Recolor target pixels and neighbors not matching
;;     border color.
;;   ResetMethod: Recolor all pixels.
(defmagick* DrawColor :
  _DrawingWand (x : _double*) (y : _double*) _PaintMethod -> _void)

;; DrawComment adds a comment to a vector output stream.
(defmagick* DrawComment :
  _DrawingWand (comment : _string) -> _void)

;; DrawEllipse draws an ellipse on the image.
(defmagick* DrawEllipse :
  _DrawingWand
  (x : _double*) (y : _double*)
  (radius-x : _double*) (radius-y : _double*)
  (start-deg : _double*) (end-deg : _double*)
  -> _void)

;; DrawGetFillColor returns the fill color used for drawing filled objects.
(defmagick* DrawGetFillColor :
  _DrawingWand (c : _PixelWand = (NewPixelWand)) -> _void -> c)

;; DrawSetFillColor sets the fill color to be used for drawing filled objects.
(defmagick* DrawSetFillColor :
  _DrawingWand _PixelWand -> _void)

;; DrawSetFillPatternURL sets the URL to use as a fill pattern for filling
;; objects.  Only local URLs ("#identifier") are supported at this time.  These
;; local URLs are normally created by defining a named fill pattern with
;; DrawPushPattern/DrawPopPattern.
(defmagick* DrawSetFillPatternURL :
  _DrawingWand (fill-url : _string) -> _void)

;; DrawGetFillOpacity returns the opacity used when drawing using the fill
;; color or fill texture.  Fully opaque is 1.0.
(defmagick* DrawGetFillOpacity :
  _DrawingWand -> _double*)

;; DrawSetFillOpacity sets the opacity to use when drawing using the fill color
;; or fill texture.  Fully opaque is 1.0.
(defmagick* DrawSetFillOpacity :
  _DrawingWand (fill-opacity : _double*) -> _void)

;; DrawGetFillRule returns the fill rule used while drawing polygons.
(defmagick* DrawGetFillRule :
  _DrawingWand -> _FillRule)

;; DrawSetFillRule sets the fill rule to use while drawing polygons.
(defmagick* DrawSetFillRule :
  _DrawingWand _FillRule -> _void)

;; DrawGetFont returns a null-terminaged string specifying the font used when
;; annotating with text.
(defmagick* DrawGetFont :
  _DrawingWand -> _string)

;; DrawSetFont sets the fully-sepecified font to use when annotating with text.
(defmagick* DrawSetFont :
  _DrawingWand (font-name : _string) -> _void)

;; DrawGetFontFamily returns the font family to use when annotating with text.
(defmagick* DrawGetFontFamily :
  _DrawingWand -> _string)

;; DrawSetFontFamily sets the font family to use when annotating with text.
(defmagick* DrawSetFontFamily :
  _DrawingWand (font-family : _string) -> _void)

;; DrawGetFontSize returns the font pointsize used when annotating with text.
(defmagick* DrawGetFontSize :
  _DrawingWand -> _double*)

;; DrawSetFontSize sets the font pointsize to use when annotating with text.
(defmagick* DrawSetFontSize :
  _DrawingWand (pointsize : _double*) -> _void)

;; DrawGetFontStretch returns the font stretch used when annotating with text.
(defmagick* DrawGetFontStretch :
  _DrawingWand -> _StretchType)

;; DrawSetFontStretch sets the font stretch to use when annotating with text.
;; The AnyStretch enumeration acts as a wild-card "don't care" option.
(defmagick* DrawSetFontStretch :
  _DrawingWand _StretchType -> _void)

;; DrawGetFontStyle returns the font style used when annotating with text.
(defmagick* DrawGetFontStyle :
  _DrawingWand -> _StyleType)

;; DrawSetFontStyle sets the font style to use when annotating with text.  The
;; AnyStyle enumeration acts as a wild-card "don't care" option.
(defmagick* DrawSetFontStyle :
  _DrawingWand _StyleType -> _void)

;; DrawGetFontWeight returns the font weight used when annotating with text.
(defmagick* DrawGetFontWeight :
  _DrawingWand -> _ulong)

;; DrawSetFontWeight sets the font weight to use when annotating with text.
(defmagick* DrawSetFontWeight :
  _DrawingWand (font-weight : _ulong) -> _void)

;; DrawGetGravity returns the text placement gravity used when annotating with
;; text.
(defmagick* DrawGetGravity :
  _DrawingWand -> _GravityType)

;; DrawSetGravity sets the text placement gravity to use when annotating with
;; text.
(defmagick* DrawSetGravity :
  _DrawingWand _GravityType -> _void)

;; DrawComposite composites an image onto the current image, using the
;; specified composition operator, specified position, and at the specified
;; size.
(defmagick* DrawComposite :
  _DrawingWand _CompositeOperator
  (x : _double*) (y : _double*) (width : _double*) (height : _double*) _Image
  -> _void)

;; DrawLine draws a line on the image using the current stroke color, stroke
;; opacity, and stroke width.
(defmagick* DrawLine :
  _DrawingWand (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  -> _void)

;; DrawMatte paints on the image's opacity channel in order to set effected
;; pixels to transparent.  The available paint methods are:
;;   PointMethod: Select the target pixel
;;   ReplaceMethod: Select any pixel that matches the target pixel.
;;   FloodfillMethod: Select the target pixel and matching neighbors.
;;   FillToBorderMethod: Select the target pixel and neighbors not matching
;;     border color.
;;   ResetMethod: Select all pixels.
(defmagick* DrawMatte :
  _DrawingWand (x : _double*) (y : _double*) _PaintMethod -> _void)

;; DrawPathClose adds a path element to the current path which closes the
;; current subpath by drawing a straight line from the current point to the
;; current subpath's most recent starting point (usually, the most recent
;; moveto point).
(defmagick* DrawPathClose :
  _DrawingWand -> _void)

;; DrawPathCurveToAbsolute draws a cubic Bezier curve from the current point to
;; (x,y) using (x1,y1) as the control point at the beginning of the curve and
;; (x2,y2) as the control point at the end of the curve using absolute
;; coordinates.  At the end of the command, the new current point becomes the
;; final (x,y) coordinate pair used in the polybezier.
(defmagick* DrawPathCurveToAbsolute :
  _DrawingWand
  (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  (x : _double*) (y : _double*)
  -> _void)

;; DrawPathCurveToRelative draws a cubic Bezier curve from the current point to
;; (x,y) using (x1,y1) as the control point at the beginning of the curve and
;; (x2,y2) as the control point at the end of the curve using relative
;; coordinates.  At the end of the command, the new current point becomes the
;; final (x,y) coordinate pair used in the polybezier.
(defmagick* DrawPathCurveToRelative :
  _DrawingWand
  (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  (x : _double*) (y : _double*)
  -> _void)

;; DrawPathCurveToQuadraticBezierAbsolute draws a quadratic Bezier curve from
;; the current point to (x,y) using (x1,y1) as the control point using absolute
;; coordinates.  At the end of the command, the new current point becomes the
;; final (x,y) coordinate pair used in the polybezier.
(defmagick* DrawPathCurveToQuadraticBezierAbsolute :
  _DrawingWand (x1 : _double*) (y1 : _double*) (x : _double*) (y : _double*)
  -> _void)

;; DrawPathCurveToQuadraticBezierRelative draws a quadratic Bezier curve from
;; the current point to (x,y) using (x1,y1) as the control point using relative
;; coordinates.  At the end of the command, the new current point becomes the
;; final (x,y) coordinate pair used in the polybezier.
(defmagick* DrawPathCurveToQuadraticBezierRelative :
  _DrawingWand (x1 : _double*) (y1 : _double*) (x : _double*) (y : _double*)
  -> _void)

;; DrawPathCurveToQuadraticBezierSmoothAbsolute draws a quadratic Bezier curve
;; (using absolute coordinates) from the current point to (x,y).  The control
;; point is assumed to be the reflection of the control point on the previous
;; command relative to the current point.  (If there is no previous command or
;; if the previous command was not a DrawPathCurveToQuadraticBezierAbsolute,
;; DrawPathCurveToQuadraticBezierRelative,
;; DrawPathCurveToQuadraticBezierSmoothAbsolut or
;; DrawPathCurveToQuadraticBezierSmoothRelative, assume the control point is
;; coincident with the current point.).  At the end of the command, the new
;; current point becomes the final (x,y) coordinate pair used in the
;; polybezier.
(defmagick* DrawPathCurveToQuadraticBezierSmoothAbsolute :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathCurveToQuadraticBezierSmoothAbsolute draws a quadratic Bezier curve
;; (using relative coordinates) from the current point to (x,y).  The control
;; point is assumed to be the reflection of the control point on the previous
;; command relative to the current point.  (If there is no previous command or
;; if the previous command was not a DrawPathCurveToQuadraticBezierAbsolute,
;; DrawPathCurveToQuadraticBezierRelative,
;; DrawPathCurveToQuadraticBezierSmoothAbsolut or
;; DrawPathCurveToQuadraticBezierSmoothRelative, assume the control point is
;; coincident with the current point.).  At the end of the command, the new
;; current point becomes the final (x,y) coordinate pair used in the
;; polybezier.
(defmagick* DrawPathCurveToQuadraticBezierSmoothRelative :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathCurveToSmoothAbsolute draws a cubic Bezier curve from the current
;; point to (x,y) using absolute coordinates.  The first control point is
;; assumed to be the reflection of the second control point on the previous
;; command relative to the current point.  (If there is no previous command or
;; if the previous command was not an DrawPathCurveToAbsolute,
;; DrawPathCurveToRelative, DrawPathCurveToSmoothAbsolute or
;; DrawPathCurveToSmoothRelative, assume the first control point is coincident
;; with the current point.)  (x2,y2) is the second control point (i.e., the
;; control point at the end of the curve).  At the end of the command, the new
;; current point becomes the final (x,y) coordinate pair used in the
;; polybezier.
(defmagick* DrawPathCurveToSmoothAbsolute :
  _DrawingWand (x2 : _double*) (y2 : _double*) (x : _double*) (y : _double*)
  -> _void)

;; DrawPathCurveToSmoothRelative draws a cubic Bezier curve from the current
;; point to (x,y) using relative coordinates.  The first control point is
;; assumed to be the reflection of the second control point on the previous
;; command relative to the current point.  (If there is no previous command or
;; if the previous command was not an DrawPathCurveToAbsolute,
;; DrawPathCurveToRelative, DrawPathCurveToSmoothAbsolute or
;; DrawPathCurveToSmoothRelative, assume the first control point is coincident
;; with the current point.)  (x2,y2) is the second control point (i.e., the
;; control point at the end of the curve).  At the end of the command, the new
;; current point becomes the final (x,y) coordinate pair used in the
;; polybezier.
(defmagick* DrawPathCurveToSmoothRelative :
  _DrawingWand (x2 : _double*) (y2 : _double*) (x : _double*) (y : _double*)
  -> _void)

;; DrawPathEllipticArcAbsolute draws an elliptical arc from the current point
;; to (x, y) using absolute coordinates.  The size and orientation of the
;; ellipse are defined by two radii (rx, ry) and an xAxisRotation, which
;; indicates how the ellipse as a whole is rotated relative to the current
;; coordinate system.  The center (cx, cy) of the ellipse is calculated
;; automatically to satisfy the constraints imposed by the other parameters.
;; largeArcFlag and sweepFlag contribute to the automatic calculations and help
;; determine how the arc is drawn.  If largeArcFlag is true then draw the
;; larger of the available arcs.  If sweepFlag is true, then draw the arc
;; matching a clock-wise rotation.
(defmagick* DrawPathEllipticArcAbsolute :
  _DrawingWand (rx : _double*) (ry : _double*)
  (x-axis-rotation : _double*) (large-arc-flag? : _bool) (sweep-flag? : _bool)
  (x : _double*) (y : _double*)
  -> _void)

;; DrawPathEllipticArcRelative draws an elliptical arc from the current point
;; to (x, y) using relative coordinates.  The size and orientation of the
;; ellipse are defined by two radii (rx, ry) and an xAxisRotation, which
;; indicates how the ellipse as a whole is rotated relative to the current
;; coordinate system.  The center (cx, cy) of the ellipse is calculated
;; automatically to satisfy the constraints imposed by the other parameters.
;; largeArcFlag and sweepFlag contribute to the automatic calculations and help
;; determine how the arc is drawn.  If largeArcFlag is true then draw the
;; larger of the available arcs.  If sweepFlag is true, then draw the arc
;; matching a clock-wise rotation.
(defmagick* DrawPathEllipticArcRelative :
  _DrawingWand (rx : _double*) (ry : _double*)
  (x-axis-rotation : _double*) (large-arc-flag? : _bool) (sweep-flag? : _bool)
  (x : _double*) (y : _double*)
  -> _void)

;; DrawPathFinish terminates the current path.
(defmagick* DrawPathFinish :
  _DrawingWand -> _void)

;; DrawPathLineToAbsolute draws a line path from the current point to the given
;; coordinate using absolute coordinates.  The coordinate then becomes the new
;; current point.
(defmagick* DrawPathLineToAbsolute :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathLineToRelative draws a line path from the current point to the given
;; coordinate using relative coordinates.  The coordinate then becomes the new
;; current point.
(defmagick* DrawPathLineToRelative :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathLineToHorizontalAbsolute draws a horizontal line path from the
;; current point to the target point using absolute coordinates.  The target
;; point then becomes the new current point.
(defmagick* DrawPathLineToHorizontalAbsolute :
  _DrawingWand (x : _double*) -> _void)

;; DrawPathLineToHorizontalRelative draws a horizontal line path from the
;; current point to the target point using relative coordinates.  The target
;; point then becomes the new current point.
(defmagick* DrawPathLineToHorizontalRelative :
  _DrawingWand (x : _double*) -> _void)

;; DrawPathLineToVerticalAbsolute draws a vertical line path from the current
;; point to the target point using absolute coordinates.  The target point then
;; becomes the new current point.
(defmagick* DrawPathLineToVerticalAbsolute :
  _DrawingWand (y : _double*) -> _void)

;; DrawPathLineToVerticalRelative draws a vertical line path from the current
;; point to the target point using relative coordinates.  The target point then
;; becomes the new current point.
(defmagick* DrawPathLineToVerticalRelative :
  _DrawingWand (y : _double*) -> _void)

;; DrawPathMoveToAbsolute starts a new sub-path at the given coordinate using
;; absolute coordinates.  The current point then becomes the specified
;; coordinate.
(defmagick* DrawPathMoveToAbsolute :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathMoveToRelative starts a new sub-path at the given coordinate using
;; relative coordinates.  The current point then becomes the specified
;; coordinate.
(defmagick* DrawPathMoveToRelative :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPathStart declares the start of a path drawing list which is terminated
;; by a matching DrawPathFinish command.  All other DrawPath commands must be
;; enclosed between a DrawPathStart and a DrawPathFinish command.  This is
;; because path drawing commands are subordinate commands and they do not
;; function by themselves.
(defmagick* DrawPathStart :
  _DrawingWand -> _void)

;; PeekDrawingWand returns the current drawing wand.
(defmagick* PeekDrawingWand :
  _DrawingWand -> _DrawInfo)

;; DrawPoint draws a point using the current stroke color and stroke thickness
;; at the specified coordinates.
(defmagick* DrawPoint :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawPolygon draws a polygon using the current stroke, stroke width, and fill
;; color or texture, using the specified array of coordinates.
(defmagick* DrawPolygon :
  (d points) ::
  (d : _DrawingWand) (_ulong = (length points)) (points : _Points) -> _void)

;; DrawPolyline draws a polyline using the current stroke, stroke width, and
;; fill color or texture, using the specified array of coordinates.
(defmagick* DrawPolyline :
  (d points) ::
  (d : _DrawingWand) (_ulong = (length points)) (points : _Points) -> _void)

;; DrawPopClipPath terminates a clip path definition.
(defmagick* DrawPopClipPath :
  _DrawingWand -> _void)

;; DrawPopDefs terminates a definition list.
(defmagick* DrawPopDefs :
  _DrawingWand -> _void)

;; PopDrawingWand destroys the current drawing wand and returns to the
;; previously pushed drawing wand.  Multiple drawing wands may exist.  It is an
;; error to attempt to pop more drawing wands than have been pushed, and it is
;; proper form to pop all drawing wands which have been pushed.
(defmagick* PopDrawingWand :
  _DrawingWand -> _void)

;; DrawPopPattern terminates a pattern definition.
(defmagick* DrawPopPattern :
  _DrawingWand -> _void)

;; DrawPushClipPath starts a clip path definition which is comprized of any
;; number of drawing commands and terminated by a DrawPopClipPath command.
(defmagick* DrawPushClipPath :
  _DrawingWand (clip-path-id : _string) -> _void)

;; DrawPushDefs indicates that commands up to a terminating DrawPopDefs command
;; create named elements (e.g. clip-paths, textures, etc.) which may safely be
;; processed earlier for the sake of efficiency.
(defmagick* DrawPushDefs :
  _DrawingWand -> _void)

;; PushDrawingWand clones the current drawing wand to create a new
;; drawing wand.  The original drawing drawing wand(s) may be returned to by
;; invoking PopDrawingWand.  The drawing wands are stored on a drawing wand
;; stack.  For every Pop there must have already been an equivalent Push.
(defmagick* PushDrawingWand :
  _DrawingWand -> _void)

;; DrawPushPattern indicates that subsequent commands up to a DrawPopPattern
;; command comprise the definition of a named pattern.  The pattern space is
;; assigned top left corner coordinates, a width and height, and becomes its
;; own drawing space.  Anything which can be drawn may be used in a pattern
;; definition.  Named patterns may be used as stroke or brush definitions.
(defmagick* DrawPushPattern :
  _DrawingWand (pattern-id : _string)
  (x : _double*) (y : _double*) (width : _double*) (height : _double*)
  -> _void)

;; DrawRectangle draws a rectangle given two coordinates and using the current
;; stroke, stroke width, and fill settings.
(defmagick* DrawRectangle :
  _DrawingWand (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  -> _void)

;; DrawRender renders all preceding drawing commands onto the image.
(defmagick* DrawRender :
  _DrawingWand -> _status)

;; DrawRotate applies the specified rotation to the current coordinate space.
(defmagick* DrawRotate :
  _DrawingWand (degrees : _double*) -> _void)

;; DrawRoundRectangle draws a rounted rectangle given two coordinates, x & y
;; corner radiuses and using the current stroke, stroke width, and fill
;; settings.
(defmagick* DrawRoundRectangle :
  _DrawingWand
  (x1 : _double*) (y1 : _double*) (x2 : _double*) (y2 : _double*)
  (rx : _double*) (ry : _double*)
  -> _void)

;; DrawScale adjusts the scaling factor to apply in the horizontal and vertical
;; directions to the current coordinate space.
(defmagick* DrawScale :
  _DrawingWand (horizontal-scale : _double*) (vertical-scale : _double*)
  -> _void)

;; DrawSkewX skews the current coordinate system in the horizontal direction.
(defmagick* DrawSkewX :
  _DrawingWand (degrees : _double*) -> _void)

;; DrawSkewY skews the current coordinate system in the vertical direction.
(defmagick* DrawSkewY :
  _DrawingWand (degrees : _double*) -> _void)

;; DrawGetStrokeColor returns the color used for stroking object outlines.
(defmagick* DrawGetStrokeColor :
  _DrawingWand (c : _PixelWand = (NewPixelWand)) -> _void -> c)

;; DrawSetStrokeColor sets the color used for stroking object outlines.
(defmagick* DrawSetStrokeColor :
  _DrawingWand _PixelWand -> _void)

;; DrawSetStrokePatternURL sets the pattern used for stroking object outlines.
(defmagick* DrawSetStrokePatternURL :
  _DrawingWand (stroke-url : _string) -> _void)

;; DrawGetStrokeAntialias returns the current stroke antialias setting.
;; Stroked outlines are antialiased by default.  When antialiasing is disabled
;; stroked pixels are thresholded to determine if the stroke color or
;; underlying canvas color should be used.
(defmagick* DrawGetStrokeAntialias :
  _DrawingWand -> _bool)

;; DrawSetStrokeAntialias controls whether stroked outlines are antialiased.
;; Stroked outlines are antialiased by default.  When antialiasing is disabled
;; stroked pixels are thresholded to determine if the stroke color or
;; underlying canvas color should be used.
(defmagick* DrawSetStrokeAntialias :
  _DrawingWand (stroke-antialias? : _bool) -> _void)

;; DrawGetStrokeDashArray returns an array representing the pattern of dashes
;; and gaps used to stroke paths (see DrawSetStrokeDashArray).
(defmagick* DrawGetStrokeDashArray :
  _DrawingWand (len : (_ptr o _ulong)) -> (_list o _double* len))

;; DrawSetStrokeDashArray specifies the pattern of dashes and gaps used to
;; stroke paths.  The strokeDashArray represents an array of numbers that
;; specify the lengths of alternating dashes and gaps in pixels.  If an odd
;; number of values is provided, then the list of values is repeated to yield
;; an even number of values.  To remove an existing dash array, pass a zero
;; number_elements argument and null dash_array.  A typical strokeDashArray_
;; array might contain the members 5 3 2.
(defmagick* DrawSetStrokeDashArray :
  (d dash-list) ::
  ;; the function seem to always expect a 0.0-terminated vector
  (dash-list : _? = (append dash-list '(0.0)))
  (d : _DrawingWand)
  (_ulong = (length dash-list)) (dash-list : (_list i _double*))
  -> _void)

;; DrawGetStrokeDashOffset returns the offset into the dash pattern to start
;; the dash.
(defmagick* DrawGetStrokeDashOffset :
  _DrawingWand -> _double*)

;; DrawSetStrokeDashOffset specifies the offset into the dash pattern to start
;; the dash.
(defmagick* DrawSetStrokeDashOffset :
  _DrawingWand (dash-offset : _double*) -> _void)

;; DrawGetStrokeLineCap returns the shape to be used at the end of open
;; subpaths when they are stroked.  Values of LineCap are UndefinedCap,
;; ButtCap, RoundCap, and SquareCap.
(defmagick* DrawGetStrokeLineCap :
  _DrawingWand -> _LineCap)

;; DrawSetStrokeLineCap specifies the shape to be used at the end of open
;; subpaths when they are stroked.  Values of LineCap are UndefinedCap,
;; ButtCap, RoundCap, and SquareCap.
(defmagick* DrawSetStrokeLineCap :
  _DrawingWand _LineCap -> _void)

;; DrawGetStrokeLineJoin returns the shape to be used at the corners of paths
;; (or other vector shapes) when they are stroked.  Values of LineJoin are
;; UndefinedJoin, MiterJoin, RoundJoin, and BevelJoin.
(defmagick* DrawGetStrokeLineJoin :
  _DrawingWand -> _LineJoin)

;; DrawSetStrokeLineJoin specifies the shape to be used at the corners of paths
;; (or other vector shapes) when they are stroked.  Values of LineJoin are
;; UndefinedJoin, MiterJoin, RoundJoin, and BevelJoin.
(defmagick* DrawSetStrokeLineJoin :
  _DrawingWand _LineJoin -> _void)

;; DrawGetStrokeMiterLimit returns the miter limit.  When two line segments
;; meet at a sharp angle and miter joins have been specified for 'lineJoin', it
;; is possible for the miter to extend far beyond the thickness of the line
;; stroking the path.  The miterLimit' imposes a limit on the ratio of the
;; miter length to the 'lineWidth'.
(defmagick* DrawGetStrokeMiterLimit :
  _DrawingWand -> _ulong)

;; DrawSetStrokeMiterLimit specifies the miter limit.  When two line segments
;; meet at a sharp angle and miter joins have been specified for 'lineJoin', it
;; is possible for the miter to extend far beyond the thickness of the line
;; stroking the path.  The miterLimit' imposes a limit on the ratio of the
;; miter length to the 'lineWidth'.
(defmagick* DrawSetStrokeMiterLimit :
  _DrawingWand (miterlimit : _ulong) -> _void)

;; DrawGetStrokeOpacity returns the opacity of stroked object outlines.
(defmagick* DrawGetStrokeOpacity :
  _DrawingWand -> _double*)

;; DrawSetStrokeOpacity specifies the opacity of stroked object outlines.
(defmagick* DrawSetStrokeOpacity :
  _DrawingWand (stroke-opacity : _double*) -> _void)

;; DrawGetStrokeWidth returns the width of the stroke used to draw object
;; outlines.
(defmagick* DrawGetStrokeWidth :
  _DrawingWand -> _double*)

;; DrawSetStrokeWidth sets the width of the stroke used to draw object
;; outlines.
(defmagick* DrawSetStrokeWidth :
  _DrawingWand (stroke-width : _double*) -> _void)

;; DrawGetTextAntialias returns the current text antialias setting, which
;; determines whether text is antialiased.  Text is antialiased by default.
(defmagick* DrawGetTextAntialias :
  _DrawingWand -> _bool)

;; DrawSetTextAntialias controls whether text is antialiased.  Text is
;; antialiased by default.
(defmagick* DrawSetTextAntialias :
  _DrawingWand (text-antialias? : _bool) -> _void)

;; DrawGetTextDecoration returns the decoration applied when annotating with
;; text.
(defmagick* DrawGetTextDecoration :
  _DrawingWand -> _DecorationType)

;; DrawSetTextDecoration specifies a decoration to be applied when annotating
;; with text.
(defmagick* DrawSetTextDecoration :
  _DrawingWand _DecorationType -> _void)

;; DrawGetTextEncoding returns a null-terminated string which specifies the
;; code set used for text annotations.
(defmagick* DrawGetTextEncoding :
  _DrawingWand -> _string)

;; DrawSetTextEncoding specifies specifies the code set to use for text
;; annotations.  The only character encoding which may be specified at this
;; time is "UTF-8" for representing Unicode as a sequence of bytes.  Specify an
;; empty string to set text encoding to the system's default.  Successful text
;; annotation using Unicode may require fonts designed to support Unicode.
(defmagick* DrawSetTextEncoding :
  _DrawingWand (encoding : _string) -> _void)

;; DrawGetTextUnderColor returns the color of a background rectangle to place
;; under text annotations.
(defmagick* DrawGetTextUnderColor :
  _DrawingWand (c : _PixelWand = (NewPixelWand)) -> _void -> c)

;; DrawSetTextUnderColor specifies the color of a background rectangle to place
;; under text annotations.
(defmagick* DrawSetTextUnderColor :
  _DrawingWand _PixelWand -> _void)

;; DrawTranslate applies a translation to the current coordinate system which
;; moves the coordinate system origin to the specified coordinate.
(defmagick* DrawTranslate :
  _DrawingWand (x : _double*) (y : _double*) -> _void)

;; DrawSetViewbox sets the overall canvas size to be recorded with the drawing
;; vector data.  Usually this will be specified using the same size as the
;; canvas image.  When the vector data is saved to SVG or MVG formats, the
;; viewbox is use to specify the size of the canvas image that a viewer will
;; render the vector data on.
(defmagick* DrawSetViewbox :
  _DrawingWand (x1 : _ulong) (y1 : _ulong) (x2 : _ulong) (y2 : _ulong)
  -> _void)

;; NewDrawingWand returns a draw wand required for all other methods in the
;; API.
(defmagick* NewDrawingWand :
  -> _DrawingWand)

;; ===== Misc APIs ============================================================

;; These are not part of the Wand API, but they are used by it so we need
;; destructor functions when collecting them.

;; DestroyImage dereferences an image, deallocating memory associated with the
;; image if the reference count becomes zero.
(defmagick DestroyImage :
  _Image -> _void)

(defmagick DestroyDrawInfo :
  _DrawInfo -> _void)
