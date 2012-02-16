#lang racket/base
(require racket/class
         racket/math
         racket/draw/private/local
         racket/draw/private/dc
         racket/draw/unsafe/cairo
         racket/draw/private/bitmap
         racket/draw/private/bitmap-dc
         racket/draw/private/record-dc
         racket/draw/private/ps-setup
         ffi/unsafe
         ffi/unsafe/objc
         "../../lock.rkt"
         "dc.rkt"
         "frame.rkt"
         "cg.rkt"
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out printer-dc%
              show-print-setup))

(import-class NSPrintOperation NSView NSGraphicsContext
              NSPrintInfo NSDictionary NSPageLayout
              NSNumber)

(define NSPortraitOrientation  0)
(define NSLandscapeOrientation 1)

(define-cocoa NSPrintScalingFactor _id)

(define-objc-class PrinterView NSView
  [wxb]
  [-a _BOOL (knowsPageRange: [_NSRange-pointer rng])
      (set-NSRange-location! rng 1)
      (set-NSRange-length! rng (let ([wx (->wx wxb)])
                                 (if wx
                                     (send wx get-page-count)
                                     0)))
      #t]
  [-a _NSRect (rectForPage: [_NSInteger n])
      (let ([wx (->wx wxb)])
        (if wx
            (send wx get-rect-for-page n)
            (make-NSRect (make-NSPoint 0 0)
                         (make-NSSize 10 10))))]
  [-a _void (beginPageInRect: [_NSRect aRect] atPlacement: [_NSPoint location])
      (let ([wx (->wx wxb)])
        (when wx
          (send wx start-page-at aRect)))
      (super-tell #:type _void beginPageInRect: #:type _NSRect aRect atPlacement:  #:type _NSPoint location)]
  [-a _void (drawPageBorderWithSize: [_NSSize size])
      (let ([wx (->wx wxb)])
        (when wx
          (send wx draw-print-page self size)))])

(define (make-print-info [prev #f])
  (as-objc-allocation-with-retain
   (tell (tell NSPrintInfo alloc)
         initWithDictionary:
         (if prev
             (tell prev dictionary)
             (tell NSDictionary dictionary)))))

(define (get-scaling-factor print-info)
  ;; 10.6 only:
  #;
  (tell #:type _CGFloat print-info scalingFactor)
  (atomically
   (with-autorelease
    (tell #:type _double
          (tell (tell print-info dictionary) 
                objectForKey: NSPrintScalingFactor)
          doubleValue))))

(define (install-pss-to-print-info pss print-info)
  (tellv print-info setOrientation: #:type _int (if (eq? (send pss get-orientation) 'landscape)
                                                    NSLandscapeOrientation
                                                    NSPortraitOrientation))
  (let ([scale (let ([x (box 0)]
                     [y (box 0)])
                 (send pss get-scaling x y)
                 (unbox y))])
    ;; 10.6 only:
    #;
    (tellv print-info setScalingFactor: #:type _CGFloat scale)
    (atomically
     (with-autorelease
      (tellv (tell print-info dictionary) 
             setObject: (tell NSNumber numberWithDouble: #:type _double scale)
             forKey: NSPrintScalingFactor)))))

(define NSOkButton 1)

(define (show-print-setup parent)
  (let* ([pss (current-ps-setup)]
         [print-info (let ([pi (send pss get-native)])
                       (or pi
                           (let ([pi (make-print-info)])
                             (send pss set-native pi make-print-info)
                             pi)))])
    (install-pss-to-print-info pss print-info)
    (if (atomically
         (let ([front (get-front)])
           (begin0
            (= (tell #:type _NSInteger (tell NSPageLayout pageLayout) runModalWithPrintInfo: print-info)
               NSOkButton)
            (when front
              (tellv (send front get-cocoa-window) makeKeyAndOrderFront: #f)))))
        (begin
          (let ([o (tell #:type _int print-info orientation)])
            (send pss set-orientation (if (= o NSLandscapeOrientation)
                                          'landscape
                                          'portrait)))
          (let ([s (get-scaling-factor print-info)])
            (send pss set-scaling s s))
          #t)
        #f)))

(define printer-dc%
  (class (record-dc-mixin (dc-mixin bitmap-dc-backend%))
    (init [parent #f])

    (super-make-object (make-object quartz-bitmap% 1 1))

    (inherit get-recorded-command
             reset-recording)

    (define pages null)
    (define/override (end-page)
      (set! pages (cons (get-recorded-command) pages))
      (reset-recording))

    (define print-info (or (let-values ([(pi copier)
                                         (send (current-ps-setup)
                                               get-native-copy)])
                             pi)
                           (make-print-info)))
      
    (install-pss-to-print-info (current-ps-setup) print-info)

    (define-values (page-width page-height page-scaling)
      (let ([s (NSRect-size (tell #:type _NSRect print-info imageablePageBounds))]
            [scaling (get-scaling-factor print-info)])
        (values (NSSize-width s)
                (NSSize-height s)
                scaling)))

    (define/override (get-size)
      (values (/ page-width page-scaling) (/ page-height page-scaling)))

    (define/override (get-device-scale)
      (values page-scaling page-scaling))

    (define current-page 0)

    (define/public (get-page-count) (length pages))
    (define/public (get-rect-for-page i)
      (make-NSRect (make-NSPoint 0 (* (sub1 i) page-height))
                   (make-NSSize page-width page-height)))
    (define/public (start-page-at r)
      (set! current-page (inexact->exact (round (/ (NSPoint-y (NSRect-origin r)) page-height)))))
    (define/public (draw-print-page view-cocoa s)
      (let ([f (tell #:type _NSRect view-cocoa frame)])
        (tellv view-cocoa lockFocus)

        (let ([cg (tell #:type _CGContextRef (tell NSGraphicsContext currentContext) graphicsPort)]
              [s (tell #:type _NSSize print-info paperSize)]
              [b (tell #:type _NSRect print-info imageablePageBounds)])
          (CGContextTranslateCTM cg 0 (/ (NSSize-height s) page-scaling))
          (CGContextScaleCTM cg 1 -1)
          (CGContextTranslateCTM cg 
                                 (/ (NSPoint-x (NSRect-origin b)) page-scaling)
                                 (/ (- (NSSize-height s)
                                       (+ (NSPoint-y (NSRect-origin b))
                                          (NSSize-height (NSRect-size b))))
                                    page-scaling))
          (let* ([surface (cairo_quartz_surface_create_for_cg_context 
                           cg 
                           (inexact->exact (ceiling (/ page-width page-scaling)))
                           (inexact->exact (ceiling (/ page-height page-scaling))))]
                 [cr (cairo_create surface)])
            (cairo_surface_destroy surface)
            (let ([dc (make-object (dc-mixin
                                    (class default-dc-backend%
                                      (define/override (get-cr) cr)
                                      (super-new))))])
              (let ([proc (list-ref (reverse pages) current-page)])
                (proc dc)))
            (cairo_destroy cr)))

        (tellv view-cocoa unlockFocus)))

    (define/override (end-doc)
      (define view-cocoa (as-objc-allocation-with-retain
                          (tell (tell PrinterView alloc) 
                                initWithFrame: #:type _NSRect (make-NSRect
                                                               (make-NSPoint 0 0)
                                                               (make-NSSize 10 10)))))
      (define op-cocoa (as-objc-allocation-with-retain
                        (tell NSPrintOperation printOperationWithView: view-cocoa
                              printInfo: print-info)))

      (set-ivar! view-cocoa wxb (->wxb this))

      (atomically
       (let ([front (get-front)])
         (tellv op-cocoa runOperation)
         (when front
           (tellv (send front get-cocoa-window) makeKeyAndOrderFront: #f)))))))
