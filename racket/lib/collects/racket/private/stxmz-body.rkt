;;----------------------------------------------------------------------
;; mzscheme's `#%module-begin'

(module stxmz-body '#%kernel
  (#%require "define.rkt"
             (for-syntax '#%kernel "stx.rkt"))

  ;; So that expansions print the way the Racket programmer expects:
  (#%require (rename '#%kernel #%plain-module-begin #%module-begin))

  (define-syntax mzscheme-in-stx-module-begin
    (lambda (stx)
      (if (stx-pair? stx)
	  (datum->syntax
	   (quote-syntax here)
	   (list* (quote-syntax #%plain-module-begin)
                  (datum->syntax
		   stx
                   (list (quote-syntax #%require) '(for-syntax scheme/mzscheme)))
		  (stx-cdr stx))
	   stx)
	  (raise-syntax-error #f "bad syntax" stx))))

  (define-syntax #%top-interaction
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (datum->syntax stx (stx-cdr stx) stx stx)))

  (#%provide mzscheme-in-stx-module-begin
             #%top-interaction))
