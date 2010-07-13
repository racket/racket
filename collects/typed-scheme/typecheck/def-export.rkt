#lang racket/base

(require racket/require
         (for-syntax syntax/parse racket/base 
                     (path-up "utils/tc-utils.rkt" "private/typed-renaming.rkt" "env/type-name-env.rkt")))
(provide def-export)


(define-for-syntax (renamer id #:alt [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))

(define-syntax (def-export stx)
  (syntax-parse stx
    [(def-export export-id:identifier id:identifier cnt-id:identifier)
     #'(define-syntax export-id
         (if (unbox typed-context?)
             (renamer #'id #:alt #'cnt-id)
             (renamer #'cnt-id)))]
    [(def-export export-id:identifier id:identifier cnt-id:identifier #:alias)
     #'(define-syntax export-id
         (if (unbox typed-context?)
             (begin                           
               (add-alias #'export-id #'id)
               (renamer #'id #:alt #'cnt-id))
             (renamer #'cnt-id)))]))