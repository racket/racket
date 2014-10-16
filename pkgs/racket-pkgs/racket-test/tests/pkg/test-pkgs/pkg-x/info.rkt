#lang info

(define collection 'multi)

(define deps '("pkg-z"))
(define build-deps '("pkg-y"))

(define implies '("pkg-z"))
(define update-implies '("pkg-y"))

(define binary-omit-files '("nobin-top.txt"))
(define binary-lib-omit-files '("nobinlib-top.txt"))
(define source-omit-files '("nosrc-top.txt"))
