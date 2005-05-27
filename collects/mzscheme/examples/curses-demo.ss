
; Uses the curses.so extension. Run with
;    mzscheme -r curses-demo.ss

; To get append-extension-suffix, which adds .so or .ddl as
; approrpiate for the current platform:
(require (lib "file.ss" "dynext"))

; Load the curses extension
(load-extension (append-extension-suffix "curses"))

; Screen is initialize. Let's go!
(move 8 10)
(put "Hello, World!")
(put #\newline)
(put "Hit any key to continue.")
(refresh)

(get)




