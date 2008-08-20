#lang scheme/base
(require (prefix-in scribble: scribble/reader))

(provide (rename-out [*read read])
         (rename-out [*read-syntax read-syntax]))

; Adapted from syntax/module-reader and scribble/reader

(define (*read in modpath line col pos)
  (wrap in (scribble:read-inside in) modpath #f line col pos))

(define (*read-syntax src in modpath line col pos)
  (wrap in (scribble:read-syntax-inside src in) modpath src line col pos))

(define (wrap port body modpath src line col pos)
  (let* ([p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol
                    (path->string (path-replace-suffix name #""))))
                 'page)]
         [tag-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f v
                                     (vector src line col pos
                                             (- (or (syntax-position modpath)
                                                    (add1 pos))
                                                pos)))
                      v))]
         [lib 'web-server/template/lang]
         [lib-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f lib modpath modpath)
                      v))])
    `(,(tag-src 'module) ,(tag-src name) ,(lib-src lib)
                         template . ,body)))