#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/qp
  [qp-encode ( String -> String )]
  [qp-decode ( String -> String )]
  [qp-encode-stream (case-lambda (Input-Port Output-Port -> Void) (Input-Port Output-Port String -> Void) )]
  [qp-decode-stream ( Input-Port Output-Port -> Void )])
