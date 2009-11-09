
#lang scheme/base
(require (for-syntax scheme/base unstable/syntax)
         scheme/list
         scheme/class
         macro-debugger/util/class-iop
         scheme/gui)
(provide define/listen
         field/notify
         notify-methods
         connect-to-pref
         connect-to-pref/readonly
         override/return-false
         notify-box%
         notify-box/pref
         menu-option/notify-box
         menu-group/notify-box
         check-box/notify-box
         choice/notify-box)

(define-syntax override/return-false
  (syntax-rules ()
    [(override/return-false m ...)
     (begin (define/override (m) #f) ...)]))

(define-for-syntax (mk-init name)
  (format-id name "init-~a" (syntax-e name)))
(define-for-syntax (mk-get name)
  (format-id name "get-~a" (syntax-e name)))
(define-for-syntax (mk-set name)
  (format-id name "set-~a" (syntax-e name)))
(define-for-syntax (mk-listen name)
  (format-id name "listen-~a" (syntax-e name)))

(define-syntax (field/notify stx)
  (syntax-case stx ()
    [(field/notify name value)
     (with-syntax ([init-name (mk-init #'name)]
                   [get-name (mk-get #'name)]
                   [set-name (mk-set #'name)]
                   [listen-name (mk-listen #'name)])
       #'(begin (field [name (init-name)])
                (define/public (init-name) value)
                (define/public-final (get-name)
                  (send name get))
                (define/public-final (set-name new-value)
                  (send name set new-value))
                (define/public-final (listen-name listener)
                  (send name listen listener))))]))

(define-syntax (notify-methods stx)
  (syntax-case stx ()
    [(notify-methods name)
     (with-syntax ([init-name (mk-init #'name)]
                   [get-name (mk-get #'name)]
                   [set-name (mk-set #'name)]
                   [listen-name (mk-listen #'name)])
       #'(begin (field [name (init-name)])
                (define/public (init-name)
                  (new notify-box% (value #f)))
                (define/public-final (get-name)
                  (send name get))
                (define/public-final (set-name new-value)
                  (send name set new-value))
                (define/public-final (listen-name listener)
                  (send name listen listener))))]))

(define-syntax (connect-to-pref stx)
  (syntax-case stx ()
    [(connect-to-pref name pref)
     (with-syntax ([init-name (mk-init #'name)])
       #'(define/override (init-name) (notify-box/pref pref)))]))

(define-syntax (connect-to-pref/readonly stx)
  (syntax-case stx ()
    [(connect-to-pref/readonly name pref)
     (with-syntax ([init-name (mk-init #'name)])
       #'(define/override (init-name) (notify-box/pref/readonly pref)))]))

(define-syntax (define/listen stx)
  (syntax-case stx ()
    [(define/listen name value)
     (unless (identifier? #'name)
       (raise-syntax-error 'define/listen "expected identifier" #'name))
     (with-syntax ([get-name (mk-get #'name)]
                   [set-name (mk-set #'name)]
                   [listen-name (mk-listen #'name)])
       #'(begin
           (define name value)
           (define listeners null)
           (define/public-final (get-name) name)
           (define/public-final (set-name new-value)
             (set! name new-value)
             (for-each (lambda (listener) (listener new-value)) listeners))
           (define/public-final (listen-name listener)
             (set! listeners (cons listener listeners)))))]))

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

(define (notify-box/pref pref)
  (define nb (new notify-box% (value (pref))))
  (send nb listen pref)
  nb)

(define (notify-box/pref/readonly pref)
  (new notify-box% (value (pref))))

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
