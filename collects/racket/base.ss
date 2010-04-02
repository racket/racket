#lang racket/private
(require "private/define-struct.ss")

(provide (except-out (all-from-out scheme/base)
                     define-struct)
         (rename-out [new-define-struct define-struct]))
