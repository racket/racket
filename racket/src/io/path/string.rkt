#lang racket/base
(require "../locale/string.rkt"
         (rename-in "path.rkt"
                    [string->path raw:string->path])
         "check.rkt")

(provide string->path
         path->string)

(define/who (string->path s)
  (check who string? s)
  (check-path-string who s)
  (raw:string->path s))

(define/who (path->string p)
  (check who is-path? #:contract "path?" p)
  (bytes->string/locale (path-bytes p) #\?))
