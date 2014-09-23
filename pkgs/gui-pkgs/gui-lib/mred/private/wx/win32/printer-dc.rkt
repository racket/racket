#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw/private/dc
         racket/draw/private/local
         racket/draw/unsafe/cairo
         racket/draw/private/record-dc
         racket/draw/private/bitmap-dc
         racket/draw/private/ps-setup
         "../../lock.rkt"
         "dc.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt")

(provide
 (protect-out printer-dc%
              show-print-setup))

(define _HGLOBAL _pointer)

(define-cstruct _PAGESETUPDLG
  ([lStructSize _DWORD]
   [hwndOwner _HWND]
   [hDevMode _HGLOBAL]
   [hDevNames _HGLOBAL]
   [Flags _DWORD]
   [ptPaperSize _POINT]
   [rtMinMargin _RECT]
   [rtMargin _RECT]
   [hInstance _HINSTANCE]
   [lCustData _LPARAM]
   [lpfnPageSetupHook _fpointer]
   [lpfnPagePaintHook _fpointer]
   [lpPageSetupTemplateName _pointer]
   [hPageSetupTemplate _HGLOBAL]))

(define-cstruct _PRINTDLG
  ([lStructSize _DWORD]
   [hwndOwner _HWND]
   [hDevMode _HGLOBAL]
   [hDevNames _HGLOBAL]
   [hDC _HDC]
   [Flags _DWORD]
   [nFromPage _WORD]
   [nToPage _WORD]
   [nMinPage _WORD]
   [nMaxPage _WORD]
   [nCopies _WORD]
   [hInstance _HINSTANCE]
   [lCustData _LPARAM]
   [lpfnPrintHook _fpointer]
   [lpfnSetupHook _fpointer]
   [lpPrintTemplateName _pointer]
   [lpSetupTemplateName _pointer]
   [hPrintTemplate _HGLOBAL]
   [hSetupTemplate _HGLOBAL])
  #:alignment (if is-win64? #f 2))

(define-cstruct _DOCINFO
  ([cbSize _int]
   [lpszDocName _permanent-string/utf-16]
   [lpszOutput _pointer]
   [lpszDatatype _pointer]
   [fwType _DWORD]))

(define PD_RETURNDC #x00000100)

(define PSD_INTHOUSANDTHSOFINCHES         #x00000004)
(define PSD_INHUNDREDTHSOFMILLIMETERS     #x00000008)

(define-comdlg32 PageSetupDlgW (_wfun _PAGESETUPDLG-pointer -> _BOOL))
(define-comdlg32 PrintDlgW (_wfun _PRINTDLG-pointer -> _BOOL))

(define-gdi32 StartDocW (_wfun _HDC _DOCINFO-pointer -> _int))
(define-gdi32 StartPage (_wfun _HDC -> (r : _int) -> (unless (positive? r) (failed 'StartPage))))
(define-gdi32 EndPage (_wfun _HDC -> (r : _int) -> (unless (positive? r) (failed 'EndPage))))
(define-gdi32 EndDoc (_wfun _HDC -> (r : _int) -> (unless (positive? r) (failed 'EndDoc))))

(define PHYSICALOFFSETX 112)
(define PHYSICALOFFSETY 113)

(define needs-delete ((allocator DeleteDC) values))

(define (clone-page-setup p)
  (let ([new-p (malloc 1 _PAGESETUPDLG)])
    (set-cpointer-tag! new-p PAGESETUPDLG-tag)
    (memcpy new-p 0 p 1 _PAGESETUPDLG)
    new-p))

(define PSD_RETURNDEFAULT #x00000400)
(define PSD_MARGINS                       #x00000002)

(define (show-print-setup parent [just-create? #f])
  (let* ([pss (current-ps-setup)]
         [ps (send pss get-native)])
    (atomically
     (let ([p (malloc 'raw 1 _PAGESETUPDLG)])
       (set-cpointer-tag! p PAGESETUPDLG-tag)
       (if ps
           (memcpy p 0 ps 1 _PAGESETUPDLG)
           (begin
             (memset p 0 1 _PAGESETUPDLG)
             (set-PAGESETUPDLG-lStructSize! p (ctype-sizeof _PAGESETUPDLG))))
       (set-PAGESETUPDLG-Flags! p (bitwise-ior 
				   (if just-create?
				       PSD_RETURNDEFAULT
				       0)
				   PSD_MARGINS))
       (set-PAGESETUPDLG-hwndOwner! p (and parent
					   (send parent is-shown?)
					   (send parent get-hwnd)))
       (let ([r (PageSetupDlgW p)])
         (when r
           (let ([new-p (clone-page-setup p)])
             (send pss set-native new-p values)))
         (free p)
         ;; FIXME: `r' leaks handles through
         ;; the hDevModes and hDevNames fields
         r)))))

;; Make on drawing unit in a printing context match the relative drawing
;; unit for the screen, where the conceptual drawing unit is always 96dpi
(define SCREEN-DPI 96)

(define printer-dc%
  (class (record-dc-mixin (dc-mixin bitmap-dc-backend%))
    (init [parent #f])

    (define parent-frame parent)

    (super-make-object (make-object win32-bitmap% 1 1 #f))

    (inherit get-recorded-command
             reset-recording)

    (define pages null)
    (define/override (end-page)
      (set! pages (cons (get-recorded-command) pages))
      (reset-recording))

    (define page-setup (or (send (current-ps-setup) get-native)
                           (begin
                             (show-print-setup #f #t)
                             (send (current-ps-setup) get-native))))

    (define-values (page-width page-height margin-left margin-top)
      ;; We would like to make the size of the DC match the
      ;; printable area of the device. Unfortunately, we can't get
      ;; the printable area until after we get a DC, which is too
      ;; late for determining a page count (that can depend on the page
      ;; size). So, we treat the area within the user's chosen margins
      ;; are the printable area; starting out with PSD_MARGINS with 0 margins
      ;; and PSD_RETURNDEFAULT seems to fill in the minimum margins,
      ;; which is probably as good an approximation as any to the value
      ;; that we want. See also `set-point-scale' below, which has to bridge
      ;; the printable-area coordinate system and the within-paper-margins
      ;; coordinate system.
      (let ([scale (if (zero? (bitwise-and (PAGESETUPDLG-Flags page-setup)
                                           PSD_INTHOUSANDTHSOFINCHES))
                       ;; 100ths of mm
                       (/ SCREEN-DPI (* 1000.0 2.54))
                       ;; 1000ths of in
                       (/ SCREEN-DPI 1000.0))]
	    [r (PAGESETUPDLG-rtMargin page-setup)]
	    [p (PAGESETUPDLG-ptPaperSize page-setup)])
	(values
	 (* scale (- (POINT-x p) (RECT-left r) (RECT-right r)))
	 (* scale (- (POINT-y p) (RECT-top r) (RECT-bottom r)))
	 (* scale (RECT-left r))
	 (* scale (RECT-top r)))))

    (define/override (get-size) (values page-width page-height))

    (define start-doc-message #f)
    (define/override (start-doc s)
      (super start-doc s)
      (set! start-doc-message (and s (string->immutable-string s))))
    
    (define/override (end-doc)
      (let-values ([(hdc from-page to-page)
                    (atomically
                     (let ([p (malloc 'raw 1 _PRINTDLG)])
                       (set-cpointer-tag! p PRINTDLG-tag)
                       (memset p 0 1 _PRINTDLG)
                       (set-PRINTDLG-lStructSize! p (ctype-sizeof _PRINTDLG))
		       (set-PRINTDLG-hwndOwner! p (and parent-frame
						       (send parent-frame is-shown?)
						       (send parent-frame get-hwnd)))
                       (set-PRINTDLG-hDevMode! p (PAGESETUPDLG-hDevMode page-setup))
                       (set-PRINTDLG-hDevNames! p (PAGESETUPDLG-hDevNames page-setup))
                       (set-PRINTDLG-Flags! p (bitwise-ior PD_RETURNDC))
                       (set-PRINTDLG-nFromPage! p 1)
                       (set-PRINTDLG-nToPage! p (length pages))
                       (set-PRINTDLG-nMinPage! p 1)
                       (set-PRINTDLG-nMaxPage! p (length pages))
                       (set-PRINTDLG-nCopies! p 1)
                       (let ([r (PrintDlgW p)])
                         (begin0
                          (if r 
                              (values (needs-delete (PRINTDLG-hDC p))
                                      (PRINTDLG-nFromPage p)
                                      (PRINTDLG-nToPage p))
                              (values #f #f #f))
                          (free p)))))])
        (when hdc
          (atomically
           (let ([job
                  (let ([di (make-DOCINFO (ctype-sizeof _DOCINFO)
                                          start-doc-message
                                          #f
                                          #f
                                          0)])
                    (begin0
                     (StartDocW hdc di)
                     (when start-doc-message
                       (free (DOCINFO-lpszDocName di)))))])
             (when (positive? job)
               (for ([proc (in-list (reverse pages))]
                     [page-no (in-naturals 1)])
                 (when (<= from-page page-no to-page)
                   (StartPage hdc)
                   (let* ([s (cairo_win32_printing_surface_create hdc)]
                          [cr (cairo_create s)])
                     (set-point-scale hdc cr margin-left margin-top)
                     (proc
                      (make-object
                       (class (dc-mixin default-dc-backend%)
                         (super-new)
                         (define/override (init-cr-matrix cr)
                           (set-point-scale hdc cr margin-left margin-top))
                         (define/override (get-cr) cr))))
                     (cairo_destroy cr)
                     (cairo_surface_destroy s))
                   (EndPage hdc)))
               (EndDoc hdc))
             (DeleteDC hdc))))))))

(define (set-point-scale hdc cr margin-left margin-top)
  (let* ([lpx (GetDeviceCaps hdc LOGPIXELSX)]
         [lpy (GetDeviceCaps hdc LOGPIXELSY)]
         [dpx (GetDeviceCaps hdc PHYSICALOFFSETX)]
         [dpy (GetDeviceCaps hdc PHYSICALOFFSETY)]
         [lx (/ (if (zero? lpx) 300 lpx) SCREEN-DPI)]
         [ly (/ (if (zero? lpy) 300 lpy) SCREEN-DPI)])
    ;; We declared the size of the page based on paper,
    ;; while DC reflcts just the printable area for the device,
    ;; so compensate by shifting (and assume that margins
    ;; are use appropriately)
    (cairo_translate cr (- dpx) (- dpy))
    ;; Scale to make printer scale simulate screen resolution:
    (cairo_scale cr lx ly)
    ;; Shift again to skip over th emargin, which we subtracted
    ;; from the paper size:
    (cairo_translate cr margin-left margin-top)))
