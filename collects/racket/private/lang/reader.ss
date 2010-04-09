#lang s-exp syntax/module-reader
scheme/base

#:info get-info
#:module-info '#(racket/private/get-info get-info #f)

(require racket/private/get-info)
