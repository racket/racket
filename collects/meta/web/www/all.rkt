#lang racket/base

(require "index.rkt" "download.rkt" "community.rkt" "learning.rkt" "help.rkt"
         "new-name.rkt" "web-copyright.rkt")
(provide (rename-out [index main]) download community learning help)
