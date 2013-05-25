#lang racket/signature

;; -- exceptions raised --
(struct qp-error () #:omit-constructor)
(struct qp-wrong-input () #:omit-constructor)
(struct qp-wrong-line-size (size) #:omit-constructor)

;; -- qp methods --
qp-encode
qp-decode
qp-encode-stream
qp-decode-stream
