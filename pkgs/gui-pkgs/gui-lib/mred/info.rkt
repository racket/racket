#lang info

(define version '(400))
(define post-install-collection "installer.rkt")
(define copy-man-pages '("mred.1"))

(define release-note-files
  '(("GRacket and racket/gui" "HISTORY.txt"
     0
     (("Porting from v5.0.x to v5.1" "Draw_and_GUI_5_1.txt")
      ("Porting to v1xxx" "MrEd_100.txt")
      ("Porting to v1xxx" "MrEd_100_Framework.txt")))))
