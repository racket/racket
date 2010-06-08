#lang at-exp s-exp "shared.rkt"

(require "data.rkt")

(define version.txt
  (plain (format "~s" `((recent ,current-version) (stable ,current-version)))))
