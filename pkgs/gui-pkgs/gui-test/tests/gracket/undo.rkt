#lang racket/gui

;; Tests for Emacs-style undo in a `text%`

(define (mk)
  (define t (new text%))
  (send t set-max-undo-history 1)
  (send t set-undo-preserves-all-history #t)
  t)

(define (deep t)
  (define (touch)
    (send t begin-edit-sequence #f)
    (send t change-style (make-object style-delta% 'change-bold) 0 (send t last-position))
    (send t end-edit-sequence))

  (define (try n)
    (unless (= n 1000)
      (send t insert "hello\n")
      (send t undo)
      (send t begin-edit-sequence)
      (send t insert "mo")
      (send t insert "re")
      (send t end-edit-sequence)
      (touch)
      (send t undo)
      (send t undo) ; hello is back
      (send t undo)
      (try (add1 n))))

  (try 0)
  (unless (equal? "" (send t get-text))
    (error "failed"))
  t)

(define (bounce t)
  (define (undo s)
    (send t undo)
    (define got (send t get-text))
    (unless (equal? s got)
      (error 'undo "fail: ~s vs. ~s" s got)))

  (send t insert "hello")
  (undo "")
  (send t insert "goodbye")
  (undo "")
  (undo "hello")
  (undo "")
  (send t insert "more")
  (undo "")
  (undo "hello")
  (undo "")
  (undo "goodbye")
  (undo ""))

(void (deep (mk)))
(bounce (mk))
(bounce (deep (mk)))
