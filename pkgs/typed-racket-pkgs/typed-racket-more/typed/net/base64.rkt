#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/base64
  [base64-encode-stream (case-lambda (Input-Port Output-Port -> Void)
                                     (Input-Port Output-Port Bytes -> Void))]
  [base64-decode-stream (Input-Port Output-Port -> Void)]
  [base64-encode (Bytes -> Bytes)]
  [base64-decode (Bytes -> Bytes)])

(provide base64-encode-stream base64-decode-stream base64-encode base64-decode)
