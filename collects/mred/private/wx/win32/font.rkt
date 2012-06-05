#lang racket/base
(require racket/class
	 racket/draw
         ffi/unsafe
         racket/draw/private/local
         racket/draw/unsafe/pango
         "types.rkt"
         "utils.rkt")

(provide 
 (protect-out font->hfont
              logfont->pango-family
              (struct-out LOGFONTW) _LOGFONTW _LOGFONTW-pointer))


(define display-font-map
  (pango_win32_font_map_for_display))

(define display-context
  (pango_font_map_create_context display-font-map))

(define font-cache (pango_win32_font_cache_new))

(define (font->hfont f)
  (let* ([pfont (or (pango_font_map_load_font display-font-map
					      display-context
					      (send f get-pango))
		    ;; font load failed, so fall back to default
		    ;; font with the same size and style:
		    (pango_font_map_load_font display-font-map
					      display-context
					      (send (make-font 
						     #:size (send f get-point-size)
						     #:style (send f get-style)
						     #:weight (send f get-weight)
						     #:size-in-pixels? (send f get-size-in-pixels))
						    get-pango)))]
         [logfont (and pfont
		       (pango_win32_font_logfont pfont))])
    (and logfont
	 (begin0
	  (pango_win32_font_cache_load font-cache logfont)
	  (g_free logfont)))))

;; ----------------------------------------

(define-cstruct _LOGFONTW
  ([lfHeight  _LONG]
   [lfWidth  _LONG]
   [lfEscapement  _LONG]
   [lfOrientation  _LONG]
   [lfWeight  _LONG]
   [lfItalic  _BYTE]
   [lfUnderline  _BYTE]
   [lfStrikeOut  _BYTE]
   [lfCharSet  _BYTE]
   [lfOutPrecision  _BYTE]
   [lfClipPrecision  _BYTE]
   [lfQuality  _BYTE]
   [lfPitchAndFamily  _BYTE]
   [lfFaceName (_array _uint16 32)]))

(define (logfont->pango-family logfontw)
  (define logfont (malloc _LOGFONTW))
  (memcpy logfont logfontw (ctype-sizeof _LOGFONTW)) ; bit enough for _LOGFONTA, too
  (WideCharToMultiByte 0 0 
                       (array-ptr (LOGFONTW-lfFaceName logfontw)) 0
                       (array-ptr (LOGFONTW-lfFaceName logfontw)) 32
                       #f #f)
  (define desc
    (pango_win32_font_description_from_logfont logfont))
  (if desc
      (pango_font_description_get_family desc)
      ;; random fallback:
      "Arial"))
