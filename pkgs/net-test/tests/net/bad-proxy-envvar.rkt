#lang racket/base
(parameterize ([current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))]
               [current-namespace (make-base-namespace)]
               [current-logger (make-logger)])
  ;; There are not a lot of strings that `url->string` rejects, but
  ;; this one of them:
  (putenv "http_proxy" "http://example.com:8080 ")
  ;; Dynamic load so that we get to change the logger first
  ((dynamic-require 'net/url 'current-proxy-servers)))
