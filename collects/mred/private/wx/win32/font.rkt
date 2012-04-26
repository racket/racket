#lang racket/base
(require racket/class
	 racket/draw
         racket/draw/private/local
         racket/draw/unsafe/pango)

(provide 
 (protect-out font->hfont))

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
