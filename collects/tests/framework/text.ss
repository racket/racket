#lang scheme

(require "test-suite-utils.ss")

(define dummy-frame-title "dummy to avoid quitting")
(send-sexp-to-mred `(send (make-object frame:basic% ,dummy-frame-title) show #t))

(define (test-creation frame% class name)
  (test
   name
   (lambda (x) 
     (equal? x (list dummy-frame-title))) ;; ensure no frames left
   (lambda ()
     (let ([label
            (send-sexp-to-mred
             `(let ([f (instantiate (class ,frame%
                                      (override get-editor%)
                                      [define (get-editor%) ,class]
                                      (super-instantiate ()))
                         ())])
                (send (send f get-editor) set-max-undo-history 10)
                (send f show #t)
                (send f get-label)))])
       (wait-for-frame label)
       (send-sexp-to-mred `(test:keystroke #\a))
       (wait-for `(string=? "a" (send (send (get-top-level-focus-window) get-editor) get-text)))
       (send-sexp-to-mred
        `(begin 
           ;; remove the `a' to avoid save dialog boxes (and test them, I suppose)
           (send (send (get-top-level-focus-window) get-editor) undo) 
           (send (send (get-top-level-focus-window) get-editor) undo)
           
           (send (send (get-top-level-focus-window) get-editor) lock #t)
           (send (send (get-top-level-focus-window) get-editor) lock #f)))
       (queue-sexp-to-mred
        `(send (get-top-level-focus-window) close))
       (send-sexp-to-mred `(map (lambda (x) (send x get-label)) (get-top-level-windows)))))))

#|
  (test-creation 'frame:text%
                 '(text:basic-mixin (editor:basic-mixin text%))
                 'text:basic-mixin-creation)
  (test-creation 'frame:text%
                 'text:basic%
                 'text:basic-creation)
  |#
(test-creation 'frame:text%
               '(editor:file-mixin text:keymap%)
               'editor:file-mixin-creation)

(test-creation 'frame:text%
               'text:file%
               'text:file-creation)
(test-creation 'frame:text%
               '(text:clever-file-format-mixin text:file%)
               'text:clever-file-format-mixin-creation)
(test-creation 'frame:text%
               'text:clever-file-format%
               'text:clever-file-format-creation)
(test-creation 'frame:text%
               '(editor:backup-autosave-mixin text:clever-file-format%)
               'editor:backup-autosave-mixin-creation)
(test-creation 'frame:text%
               'text:backup-autosave%
               'text:backup-autosave-creation)
(test-creation 'frame:text%
               '(text:searching-mixin text:backup-autosave%)
               'text:searching-mixin-creation)
(test-creation 'frame:text%
               'text:searching%
               'text:searching-creation)
(test-creation '(frame:searchable-mixin frame:text%)
               '(text:info-mixin (editor:info-mixin text:searching%))
               'text:info-mixin-creation)
(test-creation '(frame:searchable-mixin frame:text%)
               'text:info%
               'text:info-creation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;



(test
 'highlight-range1
 (lambda (x) (equal? x 1))
 (λ ()
   (send-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))

(test
 'highlight-range2
 (lambda (x) (equal? x 0))
 (λ ()
   (send-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       ((send t highlight-range 1 2 "red"))
       (length (send t get-highlighted-ranges))))))


(test
 'highlight-range3
 (lambda (x) (equal? x 0))
 (λ ()
   (send-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))


(test
 'highlight-range4
 (lambda (x) (equal? x 1))
 (λ ()
   (send-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))



(test
 'highlight-range5
 (lambda (x) (equal? x 0))
 (λ ()
   (send-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))
