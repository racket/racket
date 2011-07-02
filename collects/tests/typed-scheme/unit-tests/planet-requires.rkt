#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform)
         scheme/require-syntax)

(define-for-syntax (splice-requires specs)
  (define subs (map (compose cons expand-import) specs))
  (values (apply append (map car subs)) (apply append (map cdr subs))))

(define-syntax define-module
  (syntax-rules ()
    [(_ nm spec ...)
     (define-syntax nm
       (make-require-transformer
        (lambda (stx)
          (splice-requires (list (syntax-local-introduce (quote-syntax spec)) ...)))))]))

(define-syntax planet/multiple
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ plt files ...)
        (let ([mk (lambda (spc)
                    (syntax-case spc (prefix-in)
                      [e
                       (string? (syntax-e #'e))
                       (datum->syntax spc `(planet ,#'e ,#'plt) spc)]
                      [(prefix-in p e)
                       (datum->syntax spc `(prefix-in ,#'p (planet ,#'e ,#'plt)) spc)]))])
          (splice-requires (map mk (syntax->list #'(files ...)))))]))))


(provide rackunit)
;; why is this neccessary?
(provide planet/multiple)

(define-module rackunit
  (planet/multiple ("schematics" "rackunit.plt" 2 11)
                   "test.rkt"
                   ;"graphical-ui.rkt"
                   "text-ui.rkt"
                   "util.rkt")
  ;; disabled until Carl updates to v4
  #;
  (planet/multiple ("cce" "fasttest.plt" 1 2)
                   "random.rkt"
                   "rackunit.rkt"))
