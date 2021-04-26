#lang racket
(require racket/file
         racket/rerequire)

(define dir (make-temporary-file "rereq-~a" 'directory))

(display-to-file
 (string-append
  "#lang racket/base\n"
  "(module inner racket/base\n"
  "  (require \"two.rkt\")\n"
  "  (provide proc))\n"
  "(module more-inner racket/base\n"
  "  (require (submod \"..\" inner))\n"
  "  (provide proc))\n"
  "(require 'more-inner)\n"
  "(provide proc)")
 (build-path dir "one.rkt")
 #:exists 'replace)

(define (change-required-file path proc-value)
  (display-to-file
   (string-append
    "#lang racket/base\n"
    "(module other racket/base)\n"
    "(provide proc)\n"
    (format "(define (proc) ~v)" proc-value))
   (build-path dir path)
   #:exists 'replace)
  (sleep 1) ; to make sure mod time changes so rerequire notices it
  (file-or-directory-modify-seconds
   (build-path dir path)
   (current-seconds)))

;; create "two.rkt" with `proc` function that evaluates to "foo"
(change-required-file "two.rkt" "foo")
;; even though "two.rkt" is inside a submodule, rerequire will transitively load it the first time
(unless (member (build-path dir "two.rkt")
                (dynamic-rerequire (build-path dir "one.rkt")))
  (error "failed on round"))
;; change "two.rkt"
(change-required-file "two.rkt" "zam")
;; this will error: rerequire should transitively reload "two.rkt", but doesn't
(when (empty? (dynamic-rerequire (build-path dir "one.rkt")
                                 #:verbosity 'none))
  (error "failed on second round"))
