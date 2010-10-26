#lang meta/web

(require "resources.rkt" "data.rkt")

(define version.txt
  (plain (format "~s" `((recent ,current-version) (stable ,current-version)))))
