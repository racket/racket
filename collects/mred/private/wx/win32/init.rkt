#lang racket/base

;; Registers the window class:
(require "wndclass.rkt"
         "queue.rkt")

(define pump-thread (win32-start-event-pump))
