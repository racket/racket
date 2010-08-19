#lang at-exp s-exp "shared.rkt"

(require "index.rkt" "download.rkt" "community.rkt" "learning.rkt" "help.rkt"
         "new-name.rkt" "guidelines.rkt")
(provide (rename-out [index main]) download community learning help)
