#! /usr/bin/env racket

#lang racket/base

(require (prefix-in xmms- "xmmsctrl.rkt"))

(printf "version: ~s\n" (xmms-get-version))
(printf "skin: ~s\n" (xmms-get-skin))
(printf "volume: ~s\n" (xmms-get-volume))
(printf "balance: ~s\n" (xmms-get-balance))
(printf "number of tracks: ~s\n" (xmms-get-playlist-length))
(printf "Track #10 file  = ~s\n" (xmms-get-playlist-file 10))
(printf "Track #10 title = ~s\n" (xmms-get-playlist-title 10))
(printf "Track #10 time  = ~s\n" (xmms-get-playlist-time 10))

;; (define all-files
;;   (let loop ((i (sub1 (xmms-get-playlist-length))) (files '()))
;;     (if (< i 0)
;;       files (loop (sub1 i) (cons (xmms-get-playlist-file i) files)))))
;; (printf "Number of files: ~s\n" (length all-files))
;; (sleep 1)
;; (xmms-playlist (list (car all-files) (caddr all-files) (cadddr all-files)) #f)
;; (sleep 1)
;; (xmms-playlist all-files #f)
;; (sleep 1)
;; (xmms-stop)

;; (let ([eq (xmms-get-eq)])
;;   (xmms-set-eq (list 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
;;   (sleep 1)
;;   (xmms-set-eq eq)
;;   (sleep 1))

(xmms-set-playlist-pos 10)
(printf "playing? -> ~s\n" (xmms-is-playing?))
(xmms-play)

(define t
  (thread (lambda ()
            (let loop ()
              (printf ">>> ~s\n" (xmms-get-output-time)) (sleep .1) (loop)))))
(define (back-sec)
  (let ([t (- (xmms-get-output-time) 1000)])
    (printf "Jumping to ~s\n" t)
    (xmms-jump-to-time t)))
(sleep 3) (back-sec) (sleep 3) (back-sec) (sleep 3) (back-sec) (sleep 3)
(kill-thread t)

(printf "playing? -> ~s\n" (xmms-is-playing?))
(printf "pos -> ~s\n" (xmms-get-playlist-pos))
(printf "info -> ~s\n" (xmms-get-info))
(xmms-playlist-next)
(sleep 1)
(printf "pos -> ~s\n" (xmms-get-playlist-pos))
(xmms-stop)
