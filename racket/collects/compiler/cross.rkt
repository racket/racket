#lang racket/base

(require "private/cm-minimal.rkt")

(provide
 ;; Publicly re-provide cross-multi-compile? for tools that need to be
 ;; aware of cross-multi mode (like `raco setup').
 cross-multi-compile?)
