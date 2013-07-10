#lang meta/web

(require "resources.rkt" "data.rkt")

(define version.txt
  (plain (lazy (let ([v (release-version current-release)])
                 (format "~s" `((recent ,v) (stable ,v)))))))
