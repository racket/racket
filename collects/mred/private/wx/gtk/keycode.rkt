#lang racket/base

(provide map-key-code)

(define (map-key-code v)
  (hash-ref
   #hash((#xff08 . #\backspace)
         (#xffff . #\rubout)
         (#xff09 . #\tab)
         (#xff0a . #\newline)
         (#xff0d . #\return)
         (#xff1b . escape) ; escape
         (#xff50 . home)
         (#xff51 . left)
         (#xff52 . up)
         (#xff53 . right)
         (#xff54 . down)
         (#xff55 . prior)
         (#xff56 . next)
         (#xff57 . end)
         (#xff63 . insert)
         (#xff80 . #\space) ; keypad
         (#xff89 . #\tab) ; keypad
         (#xff8d . #\u3)  ; enter
         (#xff91 . f1)
         (#xff92 . f2)
         (#xff93 . f3)
         (#xff94 . f4)
         (#xff95 . home) ; keypad
         (#xff96 . left) ; keypd
         (#xff97 . up) ; keypad
         (#xff98 . right) ; keypad
         (#xff99 . down) ; keypad
         (#xff9a . prior) ; keypad
         (#xff9b . next) ; keypad
         (#xff9c . end) ; keypad
         (#xff9e . insert) ; keypad
         (#xff9f . #\rubout) ; keypad
         (#xffaa . multiply)
         (#xffab . add)
         (#xffad . subtract)
         (#xffaf . divide)
         (#xffb0 . numpad0)
         (#xffb1 . numpad1)
         (#xffb2 . numpad2)
         (#xffb3 . numpad3)
         (#xffb4 . numpad4)
         (#xffb5 . numpad5)
         (#xffb6 . numpad6)
         (#xffb7 . numpad7)
         (#xffb8 . numpad8)
         (#xffb9 . numpad9)
         (#xffbe . f1)
         (#xffbf . f2)
         (#xffc0 . f3)
         (#xffc1 . f4)
         (#xffc2 . f5)
         (#xffc3 . f6)
         (#xffc4 . f7)
         (#xffc5 . f8)
         (#xffc6 . f9)
         (#xffc7 . f10)
         (#xffc8 . f11)
         (#xffc9 . f12)
         (#xffca . f13)
         (#xffcb . f14)
         (#xffcc . f15))
   v
   #f))
