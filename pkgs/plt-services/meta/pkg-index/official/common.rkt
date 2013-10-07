#lang racket/base
(require racket/file
         racket/runtime-path
         pkg/util
         racket/string
         web-server/http/id-cookie)

(define-runtime-path src ".")

(define-runtime-path root "root")
(make-directory* root)
(define secret-key
  (make-secret-salt/file
   (build-path root "secret.key")))
(define users-path (build-path root "users"))
(make-directory* users-path)
(define users.new-path (build-path root "users.new"))
(make-directory* users.new-path)

(github-client_id (file->string (build-path root "client_id")))
(github-client_secret (file->string (build-path root "client_secret")))

(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

(define static-path (build-path src "static"))

(define (author->list as)
  (string-split as))

(provide (all-defined-out))
