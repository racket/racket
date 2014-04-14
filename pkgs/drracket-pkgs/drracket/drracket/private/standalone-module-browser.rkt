#lang racket/base
(require racket/cmdline racket/runtime-path)

(define file (command-line #:args (file) file))

(define-runtime-path module-browser.rkt "module-browser.rkt")
(define standalone-module-overview/file 
  (dynamic-require module-browser.rkt 'standalone-module-overview/file))

(standalone-module-overview/file file)

;; so 'raco test' doesn't try to run the module browser
(module test racket/base)