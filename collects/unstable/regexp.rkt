#lang racket/base

(require racket/list racket/contract)

;; Ryan: These functions should also allow regexp objects, use object-name to get strings.
;;  And use string-join instead of add-between.

;; regexp-or : String ... -> String
;; Produces the regexp disjunction of several regexp-strings.
(define (regexp-or . strings)
  (apply string-append (add-between strings "|")))

;; regexp-maybe : String ... -> String
;; Matches the sequence of regexps, or nothing.
(define (regexp-maybe . strings)
  (format "(?:~a)?" (apply regexp-sequence strings)))

;; regexp-star : String ... -> String
;; Matches zero or more occurrences of the sequence of regexps.
(define (regexp-star . strings)
  (format "(?:~a)*" (apply regexp-sequence strings)))

;; regexp-plus : String ... -> String
;; Matches one or more occurrences of the sequence of regexps.
(define (regexp-plus . strings)
  (format "(?:~a)+" (apply regexp-sequence strings)))

;; regexp-save : String ... -> String
;; Matches and records the matched text of the sequence of regexps.
(define (regexp-save . strings)
  (format "(~a)" (apply regexp-sequence strings)))

(define (regexp-group string)
  (format "(?:~a)" string))

;; regexp-sequence
;; : String ... [#:start String #:end String #:between String] -> String
(define (regexp-sequence #:start [start ""]
                         #:end [end ""]
                         #:between [between ""]
                         . strings)
  (apply string-append
         (append (list start)
                 (add-between (map regexp-group strings) between)
                 (list end))))

;; regexp-multi : String ... -> String
;; Match a sequence of regexps in multi-line mode.
(define (regexp-multi . strings)
  (format "(?m:~a)" (apply regexp-sequence strings)))

(provide/contract
 [regexp-sequence
  (->* [] [#:start string? #:end string? #:between string?]
       #:rest (listof string?)
       string?)]
 [regexp-or (->* [string?] [] #:rest (listof string?) string?)]
 [regexp-maybe (->* [string?] [] #:rest (listof string?) string?)]
 [regexp-star (->* [string?] [] #:rest (listof string?) string?)]
 [regexp-plus (->* [string?] [] #:rest (listof string?) string?)]
 [regexp-save (->* [string?] [] #:rest (listof string?) string?)]
 [regexp-multi (->* [string?] [] #:rest (listof string?) string?)])
