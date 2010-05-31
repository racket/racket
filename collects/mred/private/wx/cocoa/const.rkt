#lang scheme/base

(provide (except-out (all-defined-out) <<))

(define NSTitledWindowMask 1)
(define NSBorderlessWindowMask 0)
(define NSClosableWindowMask 2)
(define NSMiniaturizableWindowMask 4)
(define NSResizableWindowMask 8)
(define NSTexturedBackgroundWindowMask 256)

(define NSBackingStoreBuffered 2)
(define NSRoundedBezelStyle 1)
(define NSRegularSquareBezelStyle 2)

(define NSAnyEventMask #xffffffff)

(define (<< a b) (arithmetic-shift a b))

(define NSAlphaShiftKeyMask (1 . << . 16))
(define NSShiftKeyMask (1 . << . 17))
(define NSControlKeyMask (1 . << . 18))
(define NSAlternateKeyMask (1 . << . 19))
(define NSCommandKeyMask (1 . << . 20))
(define NSNumericPadKeyMask (1 . << . 21))
(define NSHelpKeyMask (1 . << . 22))
(define NSFunctionKeyMask (1 . << . 23))

(define NSScrollerNoPart 0)
(define NSScrollerDecrementPage 1)
(define NSScrollerKnob 2)
(define NSScrollerIncrementPage 3)
(define NSScrollerDecrementLine 4)
(define NSScrollerIncrementLine 5)
(define NSScrollerKnobSlot 6)

(define NSMomentaryLightButton 0)
(define NSPushOnPushOffButton 1)
(define NSToggleButton 2)
(define NSSwitchButton 3)
(define NSRadioButton 4)
(define NSMomentaryChangeButton 5)
(define NSOnOffButton 6)
(define NSMomentaryPushInButton 7)
(define NSMomentaryPushButton 0)
(define NSMomentaryLight 7)

(define NSFocusRingTypeDefault 0)
(define NSFocusRingTypeNone 1)
(define NSFocusRingTypeExterior 2)

(define kCGBitmapAlphaInfoMask #x1F)
(define kCGBitmapFloatComponents (1 . << . 8))
(define kCGBitmapByteOrderMask #x7000)
(define kCGBitmapByteOrderDefault (0 . << . 12))
(define kCGBitmapByteOrder16Little (1 . << . 12))
(define kCGBitmapByteOrder32Little (2 . << . 12))
(define kCGBitmapByteOrder16Big (3 . << . 12))
(define kCGBitmapByteOrder32Big (4 . << . 12))

(define kCGImageAlphaNone 0)
(define kCGImageAlphaPremultipliedLast 1)
(define kCGImageAlphaPremultipliedFirst 2)
(define kCGImageAlphaLast 3)
(define kCGImageAlphaFirst 4)
(define kCGImageAlphaNoneSkipLast 5)
(define kCGImageAlphaNoneSkipFirst 6)


