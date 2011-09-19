#lang racket/base

(require (for-syntax racket/base))

(define-syntax (this-name-stx stx)
  (let* ([p (syntax-source stx)]
         [dir  (and (path? p) (let-values ([(b _1 _2) (split-path p)]) b))]
         [name (and (path? dir)
                    (bytes->string/locale
                     (path-element->bytes
                      (let-values ([(_1 p _2) (split-path dir)]) p))))])
    ;; check that we are installed as a top-level collection (this is needed
    ;; because there are some code bits (that depend on bindings from this
    ;; file) that expect this to be true)
    (with-handlers
        ([void (lambda (e)
                 (raise
                  (make-exn:fail
                   "*** Error: this collection must be a top-level collection"
                   (exn-continuation-marks e))))])
      (collection-path name))
    (datum->syntax stx name stx)))

(provide this-collection-name)
(define this-collection-name this-name-stx)

(define this-collection-path (collection-path this-collection-name))
(provide in-this-collection)
(define (in-this-collection . paths)
  (apply build-path this-collection-path paths))

(provide make-my-key)
(define (make-my-key sym)
  (string->symbol (format "handin:~a:~a" this-collection-name sym)))
