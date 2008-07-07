#lang typed-scheme

(require "private/extra-procs.ss")


(call-with-values (lambda () (values 1 2)) (lambda: ([x : Number] [y : Number]) (+ x y)))

(#{call-with-values* @ Integer Integer Integer} (lambda () (values 1 2)) (lambda: ([x : Integer] [y : Integer]) (+ x y)))


(call-with-values* (lambda () (values 1 2)) (lambda: ([x : Integer] [y : Integer]) (+ x y)))
