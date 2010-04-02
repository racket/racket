#lang racket/private
(require scheme)

(require "private/define-struct.ss")

(provide (except-out (all-from-out scheme)
                     define-struct)
         (rename-out [new-define-struct define-struct]))
