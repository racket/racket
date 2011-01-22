#lang racket/base

(provide map-key-code
         key-symbol-to-menu-key)

(define (map-key-code v)
  (hash-ref
   #hash((122 . f1)
         (120 . f2)
         (99 . f3)
         (118 . f4)
         (96 . f5)
         (97 . f6)
         (98 . f7)
         (100 . f8)
         (101 . f9)
         (109 . f10)
         (103 . f11)
         (111 . f12)
         (105 . f13)
         (107 . f14)
         (113 . f15)
         (#x35 . escape)
         (#x7e . up)
         (#x7d . down)
         (#x3d . down)
         (#x7b . left)
         (#x3b . left)
         (#x7c . right)
         (#x3c . right)
         (#x24 . #\return)
         (#x30 . #\tab)
         (#x33 . #\backspace)
         (#x75 . #\rubout)
         (#x73 . home)
         (#x77 . end)
         (#x74 . prior)
         (#x79 . next)
         (#x45 . add)
         (78 . subtract)
         (#x43 . multiply)
         (#x4b . divide)
         (71 . separator)
         (65 . decimal)
         (76 . #\u3) ; numpad enter
         (82 . numpad0)
         (83 . numpad1)
         (84 . numpad2)
         (85 . numpad3)
         (86 . numpad4)
         (87 . numpad5)
         (88 . numpad6)
         (89 . numpad7)
         (91 . numpad8)
         (92 . numpad9))
   v
   #f))

(define (key-symbol-to-menu-key k)
  (hash-ref keysyms k #f))

(define keysyms
  (let ()
    (define NSUpArrowFunctionKey #xF700)
    (define NSDownArrowFunctionKey #xF701)
    (define NSLeftArrowFunctionKey #xF702)
    (define NSRightArrowFunctionKey #xF703)
    (define NSF1FunctionKey #xF704)
    (define NSF2FunctionKey #xF705)
    (define NSF3FunctionKey #xF706)
    (define NSF4FunctionKey #xF707)
    (define NSF5FunctionKey #xF708)
    (define NSF6FunctionKey #xF709)
    (define NSF7FunctionKey #xF70A)
    (define NSF8FunctionKey #xF70B)
    (define NSF9FunctionKey #xF70C)
    (define NSF10FunctionKey #xF70D)
    (define NSF11FunctionKey #xF70E)
    (define NSF12FunctionKey #xF70F)
    (define NSF13FunctionKey #xF710)
    (define NSF14FunctionKey #xF711)
    (define NSF15FunctionKey #xF712)
    (define NSF16FunctionKey #xF713)
    (define NSF17FunctionKey #xF714)
    (define NSF18FunctionKey #xF715)
    (define NSF19FunctionKey #xF716)
    (define NSF20FunctionKey #xF717)
    (define NSF21FunctionKey #xF718)
    (define NSF22FunctionKey #xF719)
    (define NSF23FunctionKey #xF71A)
    (define NSF24FunctionKey #xF71B)
    (define NSF25FunctionKey #xF71C)
    (define NSF26FunctionKey #xF71D)
    (define NSF27FunctionKey #xF71E)
    (define NSF28FunctionKey #xF71F)
    (define NSF29FunctionKey #xF720)
    (define NSF30FunctionKey #xF721)
    (define NSF31FunctionKey #xF722)
    (define NSF32FunctionKey #xF723)
    (define NSF33FunctionKey #xF724)
    (define NSF34FunctionKey #xF725)
    (define NSF35FunctionKey #xF726)
    (define NSInsertFunctionKey #xF727)
    (define NSDeleteFunctionKey #xF728)
    (define NSHomeFunctionKey #xF729)
    (define NSBeginFunctionKey #xF72A)
    (define NSEndFunctionKey #xF72B)
    (define NSPageUpFunctionKey #xF72C)
    (define NSPageDownFunctionKey #xF72D)
    (define NSPrintScreenFunctionKey #xF72E)
    (define NSScrollLockFunctionKey #xF72F)
    (define NSPauseFunctionKey #xF730)
    (define NSSysReqFunctionKey #xF731)
    (define NSBreakFunctionKey #xF732)
    (define NSResetFunctionKey #xF733)
    (define NSStopFunctionKey #xF734)
    (define NSMenuFunctionKey #xF735)
    (define NSUserFunctionKey #xF736)
    (define NSSystemFunctionKey #xF737)
    (define NSPrintFunctionKey #xF738)
    (define NSClearLineFunctionKey #xF739)
    (define NSClearDisplayFunctionKey #xF73A)
    (define NSInsertLineFunctionKey #xF73B)
    (define NSDeleteLineFunctionKey #xF73C)
    (define NSInsertCharFunctionKey #xF73D)
    (define NSDeleteCharFunctionKey #xF73E)
    (define NSPrevFunctionKey #xF73F)
    (define NSNextFunctionKey #xF740)
    (define NSSelectFunctionKey #xF741)
    (define NSExecuteFunctionKey #xF742)
    (define NSUndoFunctionKey #xF743)
    (define NSRedoFunctionKey #xF744)
    (define NSFindFunctionKey #xF745)
    (define NSHelpFunctionKey #xF746)
    (define NSModeSwitchFunctionKey #xF747)

    (hasheq
     'start NSResetFunctionKey
     'cancel NSStopFunctionKey
     'clear NSClearDisplayFunctionKey
     'menu NSMenuFunctionKey
     'pause NSPauseFunctionKey
     'prior NSPrevFunctionKey
     'next NSNextFunctionKey
     'end NSEndFunctionKey
     'home NSHomeFunctionKey
     'left NSLeftArrowFunctionKey
     'up NSUpArrowFunctionKey
     'right NSRightArrowFunctionKey
     'down NSDownArrowFunctionKey
     'escape 0
     'select NSSelectFunctionKey
     'print NSPrintFunctionKey
     'execute NSExecuteFunctionKey
     'snapshot 0
     'insert NSInsertFunctionKey
     'help NSHelpFunctionKey
     'numpad0 0
     'numpad1 0
     'numpad2 0
     'numpad3 0
     'numpad4 0
     'numpad5 0
     'numpad6 0
     'numpad7 0
     'numpad8 0
     'numpad9 0
     'numpad-enter 0
     'multiply 0
     'add 0
     'separator 0
     'subtract 0
     'decimal 0
     'divide 0
     'f1 NSF1FunctionKey
     'f2 NSF2FunctionKey
     'f3 NSF3FunctionKey
     'f4 NSF4FunctionKey
     'f5 NSF5FunctionKey
     'f6 NSF6FunctionKey
     'f7 NSF7FunctionKey
     'f8 NSF8FunctionKey
     'f9 NSF9FunctionKey
     'f10 NSF10FunctionKey
     'f11 NSF11FunctionKey
     'f12 NSF12FunctionKey
     'f13 NSF13FunctionKey
     'f14 NSF14FunctionKey
     'f15 NSF15FunctionKey
     'f16 NSF16FunctionKey
     'f17 NSF17FunctionKey
     'f18 NSF18FunctionKey
     'f19 NSF19FunctionKey
     'f20 NSF20FunctionKey
     'f21 NSF21FunctionKey
     'f22 NSF22FunctionKey
     'f23 NSF23FunctionKey
     'f24 NSF24FunctionKey
     'scroll NSScrollLockFunctionKey)))
