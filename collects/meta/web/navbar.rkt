#lang meta/web

(require "www/main.rkt" "download/main.rkt" "minis/main.rkt" "stubs/main.rkt")
(set-navbar! (list main download -docs planet community learning)
             help)
