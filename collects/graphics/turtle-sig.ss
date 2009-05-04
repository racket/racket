#lang scheme/signature

turtles
clear home
turn turn/radians
move move-offset
draw draw-offset
erase erase-offset

save-turtle-bitmap

splitfn split*fn tpromptfn
turtle-window-size

display-lines-in-drawing

(define-syntaxes (split)
  (lambda (x)
    (syntax-case x ()
      ((_ args ...)
       (syntax (splitfn (lambda () args ...)))))))

(define-syntaxes (split*)
  (syntax-rules ()
    [(_ e0 e ...)
     (split*fn (list (lambda () e0) (lambda () e) ...))]))

(define-syntaxes (tprompt)
  (lambda (x)
    (syntax-case x ()
      ((_ e1 ...)
       (syntax (tpromptfn (lambda () e1 ...)))))))
