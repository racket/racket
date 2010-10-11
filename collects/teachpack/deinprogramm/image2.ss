#lang racket/base
(provide (all-from-out teachpack/2htdp/image)
	 image)

(require teachpack/2htdp/image)
(require deinprogramm/signature/signature)
(require deinprogramm/signature/signature-syntax)
(require deinprogramm/signature/signature-german)

(define image
  (signature (predicate image?)))


