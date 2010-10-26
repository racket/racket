#lang meta/web

(require "www/all.rkt" "download/all.rkt" "minis/all.rkt" "stubs/all.rkt")
(set-navbar! (list main download -docs planet community learning)
             help)
