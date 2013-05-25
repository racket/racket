#lang racket/base
;; owner: ryanc
(require racket/class
         racket/gui/base
         "../private/notify.rkt")
(provide (all-from-out "../private/notify.rkt")
         menu-option/notify-box
         menu-group/notify-box
         check-box/notify-box
         choice/notify-box)

;; GUI elements tied to notify-boxes
;; See unstable/private/notify.rkt for the non-gui parts of notify-boxes.

(define (menu-option/notify-box parent label nb)
  (define menu-item
    (new checkable-menu-item%
         (label label)
         (parent parent)
         (demand-callback
          (lambda (i)
            (send i check (send nb get))))
         (callback
          (lambda _ 
            #;(send nb set (send menu-item is-checked?))
            (send nb set (not (send nb get)))))))
  menu-item)

(define (check-box/notify-box parent label nb)
  (define checkbox
    (new check-box%
         (label label)
         (parent parent)
         (value (send nb get))
         (callback
          (lambda (c e) (send nb set (send c get-value))))))
  (send nb listen (lambda (value) (send checkbox set-value value)))
  checkbox)

(define (choice/notify-box parent label choices nb)
  (define choice
    (new choice%
         (label label)
         (parent parent)
         (style '(horizontal-label))
         (choices choices)
         (callback (lambda (c e) (send nb set (send c get-string-selection))))))
  (send choice set-string-selection (send nb get))
  (send nb listen (lambda (value) (send choice set-string-selection value)))
  choice)

(define (menu-group/notify-box parent labels nb)
  (map (lambda (option)
         (define label (if (pair? option) (car option) option))
         (define menu-item
           (new checkable-menu-item%
                (label label)
                (parent parent)
                (checked (eq? (send nb get) option))
                (callback
                 (lambda _ (send nb set option)))))
         (send nb listen
               (lambda (value) (send menu-item check (eq? value option))))
         menu-item)
       labels))
