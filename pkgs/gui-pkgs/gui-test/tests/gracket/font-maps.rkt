#lang racket
(require racket/draw)

;; Check for pollution of font metrics from differently
;; scaled contexts.

(define font (make-font #:face "Times"))

;; Running `go` might affect the result of `go2`
(define (go)
  (define bm (make-bitmap 1 1))
  (send (send bm make-dc) get-text-extent
	"Extra regexp"
	font
	#t))

;; `go2` is like `go`, but for a different scale
(define (go2)
  (define bm2 (make-platform-bitmap 1 1))
  (define dc (send bm2 make-dc))
  (send dc scale 1.25 1.25)
  (send dc get-text-extent
	"Extra regexp"
	font
	#t))

;; Running `go2` again in a separate place might produce
;; results unaffected by `go`:
(define (go2/p)
  (place pch (place-channel-put pch (call-with-values go2 list))))

(module+ test
  (call-with-values go void)
  (define l1 (call-with-values go2 list))
  (define l2 (sync (go2/p)))
  (unless (equal? l1 l2)
    (error 'different "~s ~s" l1 l2)))
