#lang typed/racket

((values file->string) "/dev/null" #:mode 'binary)
(file->string "/dev/null" #:mode 'text)

file->value file->bytes

(file->lines #:mode 'text #:line-mode 'linefeed "/dev/null")
