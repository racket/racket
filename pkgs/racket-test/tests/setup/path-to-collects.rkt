#lang racket/base
(require setup/collects
         rackunit
         setup/dirs
         racket/path)

(check-equal? '(lib "racket/base.rkt")
              (path->module-path (build-path (find-collects-dir) "racket" "base.rkt")))
(check-equal? '(collects #"racket" #"base.rkt")
              (path->collects-relative (build-path (find-collects-dir) "racket" "base.rkt")))

(check-equal? '(lib "planet/private/nonesuch.rkt")
              (path->module-path (build-path (find-collects-dir) "planet" "private" "nonesuch.rkt")))
(check-equal? '(collects #"planet" #"private" #"nonesuch.rkt")
              (path->collects-relative (build-path (find-collects-dir) "planet" "private" "nonesuch.rkt")))

(check-equal? (build-path (find-collects-dir) "planet" "private" "none!such.rkt")
              (path->module-path (build-path (find-collects-dir) "planet" "private" "none!such.rkt")))
(check-equal? '(collects #"planet" #"private" #"none!such.rkt")
              (path->collects-relative (build-path (find-collects-dir) "planet" "private" "none!such.rkt")))

(define here (collection-file-path "path-to-collects.rkt" "tests/setup"))
(check-equal? (build-path here "bad%coll" "m.rkt")
              (path->module-path (build-path here "bad%coll" "m.rkt")))
(check-equal? (build-path here "bad%coll" "m.rkt")
              (path->collects-relative (build-path here "bad%coll" "m.rkt")))
