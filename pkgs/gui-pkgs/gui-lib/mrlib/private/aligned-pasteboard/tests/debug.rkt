#lang racket/gui

(provide debug-snip debug-pasteboard debug-canvas)

;; debug-snip: -> (void)
;; get the relevant info about the snip that contains the two others
;; pasteboards
(define (debug-snip snip)
  (printf "--- aligned-editor-snip% --\n")
  (let ([l (box 0)] [t (box 0)] [r (box 0)] [b (box 0)])
    (send snip get-inset l t r b)
    (printf "get-inset: ~sX~s  ~sX~s\n"
            (unbox l) (unbox r) (unbox t) (unbox b)))
  (let ([l (box 0)] [t (box 0)] [r (box 0)] [b (box 0)])
    (send snip get-margin l t r b)
    (printf "get-margin: ~sX~s  ~sX~s\n"
            (unbox l) (unbox r) (unbox t) (unbox b)))
  (printf "get-max-height: ~s\n" (send snip get-max-height))
  (printf "get-max-width:  ~s\n" (send snip get-max-width))
  (printf "get-min-height: ~s\n" (send snip get-min-height))
  (printf "get-min-width:  ~s\n" (send snip get-min-width))
  ;; (printf "snip-width:     ~s\n" (send pasteboard snip-width snip))
  ;; (printf "snip-height:    ~s\n" (send pasteboard snip-height snip))
  )

;; debug-pasteboard: -> (void)
;; displays to the repl the sizes i'm interested in
(define (debug-pasteboard pasteboard)
  (printf "--- aligned-pasteboard% ---\n")
  (let ([t1 (box 0)] [t2 (box 0)])
    (send pasteboard get-extent t1 t2)
    (printf "get-extent: ~sX~s\n" (unbox t1) (unbox t2)))
  (printf "get-max-height: ~s\n" (send pasteboard get-max-height))
  (let ([t (call-with-values (λ () (send pasteboard get-max-view-size)) cons)])
    (printf "get-max-view-size: ~sX~s\n" (car t) (cdr t)))
  (printf "get-max-width: ~s\n"  (send pasteboard get-max-width))
  (printf "get-min-height: ~s\n" (send pasteboard get-min-height))
  (printf "get-min-width: ~s\n"  (send pasteboard get-min-width))
  (let ([t1 (box 0)] [t2 (box 0)])
    (send pasteboard get-view-size t1 t2)
    (printf "get-view-size: ~sX~s\n" (unbox t1) (unbox t2))))

;; debug-canvas: -> (void)
;; just some help counting pixels
(define (debug-canvas canvas)
  (printf "--- aligned-editor-canvas% ---\n")
  ;; values
  (define-syntax-rule (show* what ...)
    (begin (let ([t (call-with-values (λ () (send canvas what)) cons)])
             (printf "~a: ~sX~s\n" 'what (car t) (cdr t)))
           ...))
  (show* get-client-size get-graphical-min-size get-size)
  ;; 1 value
  (define-syntax-rule (show1 what ...)
    (begin (printf "~a: ~s\n" 'what (send canvas what)) ...))
  (show1 get-height get-width horiz-margin min-client-height min-client-width
         min-height min-width vert-margin))
