#lang meta/web

(require "www/all.rkt" "download/all.rkt" "minis/all.rkt" "stubs/all.rkt")
(set-navbar! `((main . ,main) (download . ,download) (documentation . ,documentation)) 
             ;; pkgs should be here too, needs a stub
             main help)
