#lang racket/base

(require racket/list)

(provide location-source
         location-line
         location-column
         location-position
         location-span
         syntax->location
         location->string)

;; type location = (list any number/#f number/#f number/#f number/#f)
;; location : source line column position span

(define location-source first)
(define location-line second)
(define location-column third)
(define location-position fourth)
(define location-span fifth)

;; syntax->location : syntax -> location
(define (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

;; location->string : (list-of string) -> string
(define (location->string location)
  (string-append (source->string (location-source location))
                 ":"
                 (maybe-number->string (location-line location))
                 ":"
                 (maybe-number->string (location-column location))))

(define (source->string source)
  (cond
   ((string? source) source)
   ((path? source) (path->string source))
   ((not source) "unknown")
   (else (format "~a" source))))

(define (maybe-number->string number)
  (if (number? number)
      (number->string number)
      "?"))

