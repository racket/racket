#lang racket

(module test-stdout racket/base
  (printf "1\n"))

(module test-stderr racket/base
  (eprintf "1\n"))
