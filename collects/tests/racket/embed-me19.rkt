#lang racket/base
(require racket/runtime-path)

(define-runtime-module-path plai plai)
(define-runtime-module-path plai-reader plai/lang/reader)
(define-runtime-module-path runtime racket/runtime-config)

(parameterize ([read-accept-reader #t])
  (namespace-require 'racket/base)
  (eval (read (open-input-string "#lang plai 10"))))

(with-output-to-file "stdout"
  (lambda () (printf "This is 19.\n"))
  #:exists 'append)
