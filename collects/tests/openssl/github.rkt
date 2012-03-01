#lang racket/base
(require net/url)

(unless (input-port? (get-pure-port (string->url "https://api.github.com/")))
  (error "failed for https://api.github.com/"))

