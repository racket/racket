#lang meta/web

(require "resources.rkt" "data.rkt")

(define version.txt
  (let ([v (release-version current-release)])
    (plain (format "~s" `((recent ,v) (stable ,v))))))
