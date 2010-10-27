#lang racket/base
(require racket/class
          "../../syntax.rkt"
          "../common/queue.rkt"
          "../common/dialog.rkt"
          "../../lock.rkt"
         "frame.rkt")

(provide 
 (protect-out dialog%))

(define dialog% 
  (class (dialog-mixin frame%)
    (super-new [is-dialog? #t])

    ;; #t result avoids children sheets
    (define/override (get-sheet) #t)))
