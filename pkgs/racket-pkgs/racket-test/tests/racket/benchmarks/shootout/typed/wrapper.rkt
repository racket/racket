#lang racket/base
(provide (rename-out (module-begin #%module-begin)))
(require (prefix-in ts: typed/scheme/base)
	 (for-syntax racket/base (prefix-in r: typed-racket/typed-reader))
	 racket/include typed/scheme/base)

(define-syntax (module-begin stx)
  (let* ([name (symbol->string (syntax-property stx 'enclosing-module-name))]
	 [non-opt-re #rx"-non-optimizing"]
	 [opt-re #rx"-optimizing"]
	 [opt? (not (regexp-match non-opt-re name))]
	 [base-name (substring name 0
			       (caar (regexp-match-positions
				      (if opt? opt-re non-opt-re)
				      name)))]
	 [option (if opt? '() (list #'#:no-optimize))]
	 [fname (format "~a.rktl" base-name)])
    #`(ts:#%module-begin #,@option 
                         (define OPTIMIZED? #,opt?)
                         (include/reader #,fname r:read-syntax))))
