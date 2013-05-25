#lang racket/base
(require rackunit/docs-complete)

(define (private-module s) #true)
(define (legacy-module s) #true)

(define hidden?
  (lambda (h)
    ;; These identifiers are useful in some tests. They are like
    ;; 'friend' in C++ classes. If this weren't a legacy module, I'd
    ;; revise the architecture. -- Matthias
    (define *hidden
      '(draw begin-draw-sequence end-draw-sequence get-@VP get-mouse-event start-and-export))
    (memq h *hidden)))

(check-docs (quote htdp/world))
(check-docs (quote htdp/testing))
(check-docs (quote htdp/show-queen))
(check-docs (quote htdp/servlet2) #:skip legacy-module)
(check-docs (quote htdp/servlet) #:skip legacy-module)
(check-docs (quote htdp/matrix))
(check-docs (quote htdp/matrix-unit) #:skip private-module)
(check-docs (quote htdp/matrix-sig) #:skip private-module)
(check-docs (quote htdp/matrix-render-sig) #:skip private-module)
(check-docs (quote htdp/matrix-invisible) #:skip private-module)
(check-docs (quote htdp/master))
(check-docs (quote htdp/master-play))
(check-docs (quote htdp/lkup-gui))
(check-docs (quote htdp/image))
(check-docs (quote htdp/hangman) #:skip hidden?)
(check-docs (quote htdp/hangman-play))
(check-docs (quote htdp/gui))
(check-docs (quote htdp/guess))
(check-docs (quote htdp/guess-gui))
(check-docs (quote htdp/graphing) #:skip hidden?)
(check-docs (quote htdp/error))
(check-docs (quote htdp/elevator))
(check-docs (quote htdp/draw) #:skip hidden?)
(check-docs (quote htdp/draw-sig) #:skip private-module)
(check-docs (quote htdp/docs))
(check-docs (quote htdp/dir))
(check-docs (quote htdp/convert))
(check-docs (quote htdp/big-draw) #:skip private-module)
(check-docs (quote htdp/arrow))
(check-docs (quote htdp/arrow-gui))
