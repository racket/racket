#lang racket/base
(require racket/runtime-path
         racket/file)

(define root (getenv "PC_ROOT"))

(define mins (build-path root "mins"))
(make-directory* mins)
(define orig-pkg (build-path root "orig-pkg"))
(make-directory* orig-pkg)
(define orig (build-path root "orig"))
(make-directory* orig)
(define work (build-path root "work"))
(make-directory* work)
(define pkg-depo (build-path root "cache"))
(make-directory* pkg-depo)
(define pkg-depo-dir "static")
(make-directory* (build-path pkg-depo pkg-depo-dir))

(define cache-dir pkg-depo)
(make-directory* cache-dir)

(provide (all-defined-out))
