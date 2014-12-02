#lang racket/kernel

(printf "This is 22.\n")

(module configure-runtime racket/kernel
  (printf "Configure!\n"))
