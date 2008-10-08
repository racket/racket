#lang scribble/doc
@(require "common.ss")
@(require scheme/runtime-path (for-syntax scheme/port scheme/base scheme/path))
@(define-runtime-path cn "../chat-noir/chat-noir.ss")

@gametitle["Chat Noir" "chat-noir" "Puzzle Game"]

The goal of the game is to stop the cat from escaping the board. Each
turn you click on a circle, which prevents the cat from stepping on
that space, and the cat responds by taking a step. If the cat is
completely boxed in and thus unable reach the border, you win. If the
cat does reach the border, you lose.

The game was inspired by this one the one at
@link["http://www.gamedesign.jp/flash/chatnoir/chatnoir.html"]{Game Design}
and has essentailly the same rules.

This game is written in the
@link["http://www.htdp.org/"]{How to Design Programs}
Intermediate language. It is a model solution to the final project for
the introductory programming course at the University of Chicago in
the fall of 2008, as below.

@(define-syntax (m stx)
   (call-with-input-file
       (build-path (path-only (syntax-source stx))
                   'up "chat-noir" "chat-noir.ss")
     (lambda (port)
       (port-count-lines! port)
       #`(schemeblock
          #,@
          (let loop ()
            (let* ([p (peeking-input-port port)]
                   [l (read-line p)])
              (cond
                [(eof-object? l) '()]
                [(regexp-match #rx"^[ \t]*$" l)
                 (read-line port)
                 (loop)]
                [(regexp-match #rx"^ *;+" l)
                 =>
                 (lambda  (m)
                   (let-values ([(line col pos) (port-next-location port)])
                     (read-line port)
                     (let-values ([(line2 col2 pos2) (port-next-location port)])
                       (cons (datum->syntax 
                              #f
                              `(code:comment ,(regexp-replace* #rx" " l "\u00a0"))
                              (list "chat-noir.ss" line col pos (- pos2 pos)))
                             (loop)))))]
                [else
                 (cons (read-syntax "chat-noir.ss" port)
                       (loop))])))))
     #:mode 'text))

@m[]
