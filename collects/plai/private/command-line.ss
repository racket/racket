#lang scheme

(provide disable-tests provide abridged-test-output gc-disable-import-gc?
         alternate-collector set-alternate-collector!)


;;; HACK!!!
(define (alternate-collector)
  (getenv "PLAI_ALTERNATE_COLLECTOR"))
(define (set-alternate-collector! path)
  (putenv "PLAI_ALTERNATE_COLLECTOR"
          (if (path? path) (path->string path) path)))


(define gc-disable-import-gc? (make-parameter false))
(define disable-tests (make-parameter false))
(define abridged-test-output (make-parameter false))
