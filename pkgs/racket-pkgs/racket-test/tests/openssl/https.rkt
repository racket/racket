#lang racket/base
(require net/url)

;; try a basic HTTPS connection:
(unless (input-port? (get-pure-port (string->url "https://api.github.com/")))
  (error "failed for https://api.github.com/"))

