#lang racket/base

(require pkg/gui)

(void (make-pkg-gui))

;; For test mode, check that we can at least start,
;; but exit right away:
(module+ test 
  (require racket/gui/base)
  (queue-callback (lambda () (exit)) #f))
