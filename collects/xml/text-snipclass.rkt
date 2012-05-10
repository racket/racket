#lang racket/base
(require framework
         mzlib/class
         mred)

(provide text-box%
         (rename-out [snipclass snip-class]))

;; chunk-string: (listof any) -> (listof any)
(define (chunk-string s acc)
  (cond
    ((and (null? s) (null? acc)) null)
    ((null? s) (list (list->string (reverse acc))))
    ((char? (car s)) (chunk-string (cdr s) (cons (car s) acc)))
    ((null? acc) (cons (car s) (chunk-string (cdr s) null)))
    (else (cons (list->string (reverse acc)) (cons (car s) (chunk-string (cdr s) null))))))

(define get-icon
  (let ([icon #f])
    (λ ()
      (unless icon
        (let ()
          (define str "“”")
          (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1)))
          (define font (send the-font-list find-or-create-font 24 'default 'normal 'normal))
          (define-values (w h _1 _2) (send bdc get-text-extent str font))
          (define bmp (make-object bitmap% (floor (inexact->exact w)) (floor (inexact->exact h))))
          (send bdc set-bitmap bmp)
          (send bdc set-smoothing 'aligned)
          (send bdc set-font font)
          (send bdc clear)
          (send bdc draw-text str 0 0)
          (send bdc set-bitmap #f)
          (set! icon bmp)))
      icon)))


;; marshall: writable -> string
(define (marshall s)
  (let ((os (open-output-string)))
    (with-handlers ((exn:fail? (lambda (x) "")))
      (write s os)
      (get-output-string os))))

(define snipclass-text-box%
  (class decorated-editor-snipclass%
    (define/override (make-snip stream-in) (new text-box%))
    (super-instantiate ())))

(define old-snipclass (new snipclass-text-box%))
(send old-snipclass set-version 1)
(send old-snipclass set-classname "text-box%")
(send (get-the-snip-class-list) add old-snipclass)

(define snipclass (new snipclass-text-box%))
(send snipclass set-version 1)
(send snipclass set-classname (format "~s" '(lib "text-snipclass.ss" "xml")))
(send (get-the-snip-class-list) add snipclass)

(define text-box%
  (class* decorated-editor-snip% (readable-snip<%>)
    (define/override (make-editor) (let ([e (new text:keymap%)])
                                     (send e set-max-undo-history 'forever)
                                     e))
    (define/override (make-snip) (make-object text-box%))
    (inherit get-editor get-admin)


    (define/override (get-corner-bitmap)
      (get-icon))

    (define/override (get-menu)
      (let ([menu (new popup-menu%)])
        (new menu-item%
             (label "Convert to string")
             (parent menu)
             (callback
              (lambda (x y)
                (let ([to-ed (find-containing-editor)])
                  (when to-ed
                    (let ([this-pos (find-this-position)])
                      (when this-pos
                        (let ([from-ed (get-editor)])
                          (send to-ed begin-edit-sequence)
                          (send from-ed begin-edit-sequence)
                          (send to-ed delete this-pos (+ this-pos 1))
                          (let* ((p (open-input-text-editor from-ed 0 'end
                                                            (lambda (s)
                                                              (values (box s) 1))))
                                 (contents
                                  (let loop ((next (read-char-or-special p)))
                                    (cond
                                      ((eof-object? next) null)
                                      (else
                                       (cons next (loop (read-char-or-special p)))))))
                                 (repaired-contents
                                  (map (lambda (x)
                                         (if (string? x)
                                           (marshall x)
                                           (send (unbox x) copy)))
                                       (chunk-string contents null))))
                            (for-each
                             (lambda (x)
                               (send to-ed insert x this-pos))
                             (reverse repaired-contents)))
                          (send to-ed end-edit-sequence)
                          (send from-ed end-edit-sequence)))))))))
        menu))

    ;; find-containing-editor : -> (union #f editor)
    (define/private (find-containing-editor)
      (let ([admin (get-admin)])
        (and admin
             (send admin get-editor))))

    ;; find-this-position : -> (union #f number)
    (define/private (find-this-position)
      (let ([ed (find-containing-editor)])
        (and ed
             (send ed get-snip-position this))))

    ;; input-port -> (union (listof char) char eof-object? syntax-object)
    (define/private (get-next port)
      (let ([v (read-char-or-special port)])
        (if (special-comment? v)
          (get-next port)
          v)))

    (define/public (read-special source line column position)
      (let* ((ed (get-editor))
             (port (open-input-text-editor ed))
             (str (let loop ((next (get-next port)))
                    (cond
                      ((eof-object? next) null)
                      ((char? next)
                       (cons next (loop (get-next port))))
                      (else (cons #`(marshall #,next) (loop (get-next port))))))))
        #`(let ((marshall
                 (lambda (s)
                   (let ((os (open-output-string)))
                     (with-handlers ((exn:fail? (lambda (x) "")))
                       (display s os)
                       (get-output-string os))))))
            (string-append #,@(chunk-string str null)))))

    (super-instantiate ())
    (inherit set-snipclass)
    (set-snipclass snipclass)))
