#lang racket/base

(provide QS_ALLINPUT)

(define QS_KEY              #x0001)
(define QS_MOUSEMOVE        #x0002)
(define QS_MOUSEBUTTON      #x0004)
(define QS_POSTMESSAGE      #x0008)
(define QS_TIMER            #x0010)
(define QS_PAINT            #x0020)
(define QS_SENDMESSAGE      #x0040)
(define QS_HOTKEY           #x0080)
(define QS_ALLPOSTMESSAGE   #x0100)
(define QS_RAWINPUT         #x0400)
(define QS_MOUSE           (bitwise-ior QS_MOUSEMOVE
                                        QS_MOUSEBUTTON))

(define QS_INPUT           (bitwise-ior QS_MOUSE
                                        QS_KEY
                                        QS_RAWINPUT))
(define QS_ALLEVENTS       (bitwise-ior QS_INPUT
                                        QS_POSTMESSAGE
                                        QS_TIMER
                                        QS_PAINT
                                        QS_HOTKEY))

(define QS_ALLINPUT        (bitwise-ior QS_INPUT
                                        QS_POSTMESSAGE
                                        QS_TIMER
                                        QS_PAINT
                                        QS_HOTKEY
                                        QS_SENDMESSAGE))
