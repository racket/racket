#lang at-exp s-exp "common.rkt"

(require "www/main.rkt" "download/main.rkt" "minis/main.rkt" "stubs/main.rkt")
(set-navbar! (list main download -docs planet community learning)
             help)
