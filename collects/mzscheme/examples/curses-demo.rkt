; Uses the curses.so extension. Run with
;    racket -r curses-demo.rkt

; To get append-extension-suffix, which adds .so or .dll as
; approrpiate for the current platform:
(require dynext/file)

; Load the curses extension
(load-extension (append-extension-suffix "curses"))

; Screen is initialize. Let's go!
(move 8 10)
(put "Hello, World!")
(put #\newline)
(put "Hit any key to continue.")
(refresh)

(get)
