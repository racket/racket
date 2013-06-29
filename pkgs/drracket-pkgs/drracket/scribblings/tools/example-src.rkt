#lang racket/base
(provide (struct-out src-wrap)
         files
         sexp-files)

(require (for-syntax racket/base))

(define-struct src-wrap (obj srcloc)
  #:transparent)

(define-syntax (retain-src stx)
  
  (define (to-src-wrap stx)
    (cond
      [(syntax? stx)
       #`(src-wrap #,(to-src-wrap (syntax-e stx))
                   (vector '#,(syntax-source stx)
                           #,(syntax-line stx)
                           #,(syntax-column stx)
                           #,(syntax-position stx)
                           #,(syntax-span stx)))]
      [(pair? stx)
       #`(cons #,(to-src-wrap (car stx))
               #,(to-src-wrap (cdr stx)))]
      [else 
       #`'#,stx]))
                   
  (syntax-case stx ()
    [(_ args ...)
     (with-syntax ([(args ...) 
                    (map to-src-wrap
                         (syntax->list #'(args ...)))])
       #'(list args ...))]))

(define files
  (list (list "info.rkt"
              (retain-src
               setup/infotab
               (define drracket-tools (list (list "tool.rkt")))))
        (list "tool.rkt"
              (retain-src
               racket/base
               (require drracket/tool
                        racket/class
                        racket/gui/base
                        racket/unit
                        mrlib/switchable-button)
               (provide tool@)
               
               (define secret-key "egg")
               (define to-insert "easter ")
               
               (define tool@
                 (unit
                   (import drracket:tool^)
                   (export drracket:tool-exports^)
                   
                   (define easter-egg-mixin
                     (mixin ((class->interface text%)) ()
                       
                       (inherit begin-edit-sequence
                                end-edit-sequence
                                insert
                                get-text)
                       
                       (define/augment (on-insert start len)
                         (begin-edit-sequence))
                       (define/augment (after-insert start len)
                         (check-range (max 0 (- start (string-length secret-key)))
                                      (+ start len))
                         (end-edit-sequence))
                       
                       (define/augment (on-delete start len)
                         (begin-edit-sequence))
                       (define/augment (after-delete start len)
                         (check-range (max 0 (- start (string-length secret-key)))
                                      start)
                         (end-edit-sequence))
                       
                       (define/private (check-range start stop)
                         (let/ec k
                           (for ([x (in-range start stop)])
                             (define after-x 
                               (get-text x (+ x (string-length secret-key))))
                             (when (string=? after-x secret-key)
                               (define before-x 
                                 (get-text (max 0 (- x (string-length to-insert))) x))
                               (unless (string=? before-x to-insert)
                                 (insert to-insert x x)
                                 (k (void)))))))
                       
                       (super-new)))
                   
                   (define reverse-button-mixin
                     (mixin (drracket:unit:frame<%>) ()
                       (super-new)
                       (inherit get-button-panel
                                get-definitions-text)
                       (inherit register-toolbar-button)
                       
                       (let ([btn
                              (new switchable-button%
                                   [label "Reverse Definitions"]
                                   [callback (λ (button) 
                                               (reverse-content
                                                (get-definitions-text)))]
                                   [parent (get-button-panel)]
                                   [bitmap reverse-content-bitmap])])
                         (register-toolbar-button btn #:number 11)
                         (send (get-button-panel) change-children
                               (λ (l)
                                 (cons btn (remq btn l)))))))
                   
                   (define reverse-content-bitmap
                     (let* ([bmp (make-bitmap 16 16)]
                            [bdc (make-object bitmap-dc% bmp)])
                       (send bdc erase)
                       (send bdc set-smoothing 'smoothed)
                       (send bdc set-pen "black" 1 'transparent)
                       (send bdc set-brush "blue" 'solid)
                       (send bdc draw-ellipse 2 2 8 8)
                       (send bdc set-brush "red" 'solid)
                       (send bdc draw-ellipse 6 6 8 8)
                       (send bdc set-bitmap #f)
                       bmp))
                   
                   (define (reverse-content text)
                     (for ([x (in-range 1 (send text last-position))])
                       (send text split-snip x))
                     (define snips
                       (let loop ([snip (send text find-first-snip)])
                         (if snip
                             (cons snip (loop (send snip next)))
                             '())))
                     (define released-snips
                       (for/list ([snip (in-list snips)]
                                  #:when (send snip release-from-owner))
                         snip))
                     (for ([x (in-list released-snips)])
                       (send text insert x 0 0)))
                   
                   (define (phase1) (void))
                   (define (phase2) (void))
                   
                   (drracket:get/extend:extend-definitions-text easter-egg-mixin)
                   (drracket:get/extend:extend-unit-frame reverse-button-mixin)))))))


(define (to-sexp x)
  (let loop ([sw x])
    (cond
      [(src-wrap? sw)
       (loop (src-wrap-obj sw))]
      [(pair? sw)
       (cons (loop (car sw)) (loop (cdr sw)))]
      [else sw])))

(define sexp-files
  (for/list ([file (in-list files)])
    (list (list-ref file 0)
          (to-sexp (car (list-ref file 1)))
          (to-sexp (cdr (list-ref file 1))))))
