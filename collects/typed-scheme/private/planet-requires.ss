#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform))

(define-for-syntax (splice-requires specs)  
  (define subs (map (compose cons expand-import) specs))
  (values (apply append (map car subs)) (apply append (map cdr subs))))

(define-syntax define-module
  (lambda (stx)
    (syntax-case stx ()
      [(_ nm spec ...)
       (syntax/loc stx
         (define-syntax nm
           (make-require-transformer
            (lambda (stx)
              (splice-requires (list (syntax-local-introduce (quote-syntax spec)) ...))))))])))

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


(provide galore schemeunit)
;; why is this neccessary?
(provide planet/multiple)

(define-module galore
  (prefix-in table: "tables.ss"))

(define-module schemeunit 
  (planet/multiple ("schematics" "schemeunit.plt" 2 3)
                   "test.ss"
                   "graphical-ui.ss"
                   "text-ui.ss"
                   "util.ss")
  (planet/multiple ("cce" "fasttest.plt" 1 2)
                   "random.ss"
                   "schemeunit.ss"))