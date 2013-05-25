#lang racket/base
;; owner: ryanc
(require (for-syntax racket/base syntax/parse racket/syntax)
         racket/class)
(provide define-notify
         notify-box%
         notify-box/pref)

;; Non-gui parts of notify-boxes
;; Worth splitting into two libraries?
;; Probably not, very few non-gui uses of classes.

(define-for-syntax (mk-init name)
  (format-id name "init-~a" (syntax-e name)))
(define-for-syntax (mk-get name)
  (format-id name "get-~a" (syntax-e name)))
(define-for-syntax (mk-set name)
  (format-id name "set-~a" (syntax-e name)))
(define-for-syntax (mk-listen name)
  (format-id name "listen-~a" (syntax-e name)))

(define-syntax (define-notify stx)
  (syntax-parse stx
    [(define-notify name:id
       (~optional value:expr
                  #:defaults ([value #'(new notify-box% (value #f))]))
       (~optional (~and #:init-method init-method)))
     (with-syntax ([init-name (mk-init #'name)]
                   [get-name (mk-get #'name)]
                   [set-name (mk-set #'name)]
                   [listen-name (mk-listen #'name)])
       (with-syntax ([(init-expr init-method-decl)
                      (if (attribute init-method)
                          (list #'(init-name)
                                #'(define/public (init-name) value))
                          (list #'value
                                #'(begin)))])
         (quasisyntax/loc stx
           (begin (field [name init-expr])
                  init-method-decl
                  (define/public-final (get-name)
                    (send name get))
                  (define/public-final (set-name new-value)
                    (send name set new-value))
                  (define/public-final (listen-name listener)
                    (send name listen listener))))))]))

(define notify-box%
  (class object%
    (init value)
    (define v value)
    (define listeners null)

    ;; get : -> value
    ;; Fetch current value
    (define/public (get)
      v)

    ;; set : value -> void
    ;; Update value and notify listeners
    (define/public (set nv)
      (set! v nv)
      (for-each (lambda (p) (p nv)) listeners))

    ;; listen : (value -> void) -> void
    ;; Add a listener
    (define/public (listen p)
      (set! listeners (cons p listeners)))

    ;; remove-listener : (value -> void) -> void
    (define/public (remove-listener p)
      (set! listeners (remq p listeners)))

    ;; remove-all-listeners : -> void
    (define/public (remove-all-listeners)
      (set! listeners null))

    (super-new)))


(define (notify-box/pref pref #:readonly? [readonly? #f])
  (define nb (new notify-box% (value (pref))))
  (send nb listen pref)
  nb)
