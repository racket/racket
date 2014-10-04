#lang racket/base
(require "private/standalone-module-browser.rkt")
(provide module-browser)
(define (module-browser file)
  (standalone-module-overview/file file))