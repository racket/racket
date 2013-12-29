#lang racket/base

(require racket/file
         "test-suite-utils.rkt")

(module test racket/base)

(define dummy-frame-title "dummy to avoid quitting")
(queue-sexp-to-mred `(send (make-object frame:basic% ,dummy-frame-title) show #t))

(define (test-creation frame% class name)
  (test
   name
   (lambda (x) 
     (equal? x (list dummy-frame-title))) ;; ensure no frames left
   (lambda ()
     (let ([label
            (queue-sexp-to-mred
             `(let ([f (new (class ,frame%
                              (define/override (get-editor%) ,class)
                              (super-new)))])
                (send (send f get-editor) set-max-undo-history 10)
                (send f show #t)
                (send f get-label)))])
       (wait-for-frame label)
       (send-sexp-to-mred `(test:keystroke #\a))
       (wait-for #:queue? #t `(string=? "a" (send (send (get-top-level-focus-window) get-editor) get-text)))
       (queue-sexp-to-mred
        `(begin 
           ;; remove the `a' to avoid save dialog boxes (and test them, I suppose)
           (send (send (get-top-level-focus-window) get-editor) undo) 
           (send (send (get-top-level-focus-window) get-editor) undo)
           
           (send (send (get-top-level-focus-window) get-editor) lock #t)
           (send (send (get-top-level-focus-window) get-editor) lock #f)
           (send (get-top-level-focus-window) close)))
       (queue-sexp-to-mred `(map (lambda (x) (send x get-label)) (get-top-level-windows)))))))

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
   (queue-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))

(test
 'highlight-range2
 (lambda (x) (equal? x 0))
 (λ ()
   (queue-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       ((send t highlight-range 1 2 "red"))
       (length (send t get-highlighted-ranges))))))


(test
 'highlight-range3
 (lambda (x) (equal? x 0))
 (λ ()
   (queue-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))


(test
 'highlight-range4
 (lambda (x) (equal? x 1))
 (λ ()
   (queue-sexp-to-mred
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
   (queue-sexp-to-mred
    `(let ([t (new text:basic%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))

(let ([tmp-file (path->string (make-temporary-file "fwtesttmp~a"))])
  (test
   'highlight-range/revert
   (lambda (x)
     (delete-file tmp-file)
     (equal? x 0))
   (λ ()
     (queue-sexp-to-mred
      `(let ([t (new text:basic%)])
         (send t insert "abc")
         (send t save-file ,tmp-file)
         (send t highlight-range 0 3 "red")
         (call-with-output-file ,tmp-file
           (lambda (port) (display "x\n" port))
           #:exists 'truncate)
         (send t load-file)
         (length (send t get-highlighted-ranges)))))))

(test
 'highlight-range-delegate-1
 (lambda (x) (equal? x 0))
 (λ ()
   (queue-sexp-to-mred
    `(let ([t (new text:delegate%)])
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))

(test
 'highlight-range-delegate-1
 (lambda (x) (equal? x 0))
 (λ ()
   (queue-sexp-to-mred
    `(let ([t (new text:delegate%)])
       (send t set-delegate (new text:basic%))
       (send t insert "abc")
       (send t highlight-range 1 2 "red")
       (send t unhighlight-range 1 2 "red")
       (length (send t get-highlighted-ranges))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing get-pos/text method
;;

(test
  'get-pos/text-1
  (λ (x) x)
  (λ ()
    (queue-sexp-to-mred
      '(let* ([f (new frame% [label "Test frame"])]
              [t (new text:basic%)]
              [c (new editor-canvas% [parent f] [editor t])]
              [snip (make-object string-snip% "Test string")])
          (send t insert snip)
          (define-values (x-box y-box) (values (box 0) (box 0)))
          (send t get-snip-location snip x-box y-box)
          (send t local-to-global x-box y-box)
          (define event (new mouse-event% [event-type 'motion]
                                          [x (add1 (unbox x-box))]
                                          [y (add1 (unbox y-box))]))
          (let-values ([(pos edit) (send t get-pos/text event)])
            (and (real? (car p)) (is-a? (cdr p) text%)))))))

(test
  'get-pos/text-2
  (λ (x) x)
  (λ ()
    (queue-sexp-to-mred
      '(let* ([f (new frame% [label "Test frame"])]
              [t (new text:basic%)]
              [c (new editor-canvas% [parent f] [editor t])]
              [snip (make-object string-snip% "Test string")])
          (send t insert snip)
          (define-values (x-box y-box) (values (box 0) (box 0)))
          (send t get-snip-location snip x-box y-box)
          (send t local-to-global x-box y-box)
          (define event (new mouse-event% [event-type 'motion]
                                          [x (+ 9999 (unbox x-box))]
                                          [y (+ 9999 (unbox y-box))]))
          (let-values ([(pos edit) (send t get-pos/text event)])
            (and (false? pos) (false? edit)))))))

(test
  'get-pos/text-3
  (λ (x) x)
  (λ ()
    (queue-sexp-to-mred
      '(let* ([f (new frame% [label "Test frame"])]
              [t (new text:basic%)]
              [c (new editor-canvas% [parent f] [editor t])]
              [p (new pasteboard%)]
              [s-snip (make-object string-snip% "Test string")]
              [e-snip (new editor-snip% [editor p])])
          (send p insert s-snip)
          (send t insert e-snip)
          (define-values (x-box y-box) (values (box 0) (box 0)))
          (send t get-snip-location e-snip x-box y-box)
          (send t local-to-global x-box y-box)
          (define event (new mouse-event% [event-type 'motion]
                                          [x (add1 (unbox x-box))]
                                          [y (add1 (unbox y-box))]))
          (let-values ([(pos edit) (send t get-pos/text event)])
            (and (false? pos) (is-a? edit pasteboard%)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  print-to-dc
;;

(test
 'print-to-dc
 (λ (x) (equal? x 'no-error))
 (λ ()
   (queue-sexp-to-mred
    '(let* ([t (new text:basic%)]
            [bmp (make-object bitmap% 100 40)]
            [dc (new bitmap-dc% (bitmap bmp))])
       (send t insert "Hello world")
       (send dc clear)
       (send t print-to-dc dc 1)
       'no-error))))


(test
 'print-to-dc2
 (λ (x) (equal? x 'no-error))
 (λ ()
   (queue-sexp-to-mred
    `(let* ([f (new frame% [label ""])]
            [t (new text:basic%)]
            [ec (new editor-canvas% [parent f] [editor t])]
            [bmp (make-object bitmap% 100 40)]
            [dc (new bitmap-dc% (bitmap bmp))])
       (send t insert "Hello world")
       (send t highlight-range 2 5 "orange")
       (send f reflow-container)
       (send dc clear)
       (send t print-to-dc dc 1)
       'no-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  text:ports
;;

;; there is an internal buffer of this size, so writes that are larger and smaller are interesting
(define buffer-size 4096)

(let ([big-str (build-string (* buffer-size 2) (λ (i) (integer->char (+ (modulo i 26) (char->integer #\a)))))]
      [non-ascii-str "λαβ一二三四五"])
  
  (define (do/separate-thread str mtd)
    (queue-sexp-to-mred
     `(let* ([t (new (text:ports-mixin text:wide-snip%))]
             [op (send t ,mtd)]
             [exn #f])
        (yield
         (thread
          (λ () 
            (with-handlers ((exn:fail? (λ (x) (set! exn x))))
              (display ,str op)
              (flush-output op)))))
        (when exn (raise exn))
        (send t get-text 0 (send t last-position)))))
  
  (test 
   'text:ports%.1
   (λ (x) (equal? x "abc"))
   (λ () (do/separate-thread "abc" 'get-out-port)))
  
  (test 
   'text:ports%.2
   (λ (x) (equal? x big-str))
   (λ () (do/separate-thread big-str 'get-out-port)))
  
  (test 
   'text:ports%.3
   (λ (x) (equal? x non-ascii-str))
   (λ () (do/separate-thread non-ascii-str 'get-out-port)))
  
  (test 
   'text:ports%.4
   (λ (x) (equal? x "abc"))
   (λ () (do/separate-thread "abc" 'get-err-port)))
  
  (test 
   'text:ports%.5
   (λ (x) (equal? x big-str))
   (λ () (do/separate-thread big-str 'get-err-port)))
  
  (test 
   'text:ports%.6
   (λ (x) (equal? x non-ascii-str))
   (λ () (do/separate-thread non-ascii-str 'get-err-port)))
  
  
  (test 
   'text:ports%.7
   (λ (x) (equal? x "abc"))
   (λ () (do/separate-thread "abc" 'get-value-port)))
  
  (test 
   'text:ports%.8
   (λ (x) (equal? x big-str))
   (λ () (do/separate-thread big-str 'get-value-port)))
  
  (test 
   'text:ports%.9
   (λ (x) (equal? x non-ascii-str))
   (λ () (do/separate-thread non-ascii-str 'get-value-port)))
  
  ;; display the big string, one char at a time
  (test
   'text:ports%.10
   (λ (x) (equal? x big-str))
   (λ () 
     (queue-sexp-to-mred
      `(let* ([t (new (text:ports-mixin text:wide-snip%))]
              [op (send t get-out-port)]
              [big-str ,big-str]
              [exn #f])
         (yield
          (thread
           (λ () 
             (with-handlers ((exn:fail? (λ (x) (set! exn x))))
               (let loop ([i 0])
                 (when (< i (string-length big-str))
                   (display (string-ref big-str i) op)
                   (loop (+ i 1))))
               (flush-output op)))))
         (when exn (raise exn))
         (send t get-text 0 (send t last-position))))))
     
  ;; the next tests test the interaction when the current
  ;; thread is the same as the handler thread of the eventspace
  ;; where the text was created
  
  (test 
   'text:ports%.thd1
   (λ (x) (equal? x "abc"))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new (text:ports-mixin text:wide-snip%))]
              [op (send t get-out-port)]
              [exn #f])
         (display "abc" op)
         (flush-output op)
         (send t get-text 0 (send t last-position))))))
  
  (test 
   'text:ports%.thd2
   (λ (x) (equal? x big-str))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new (text:ports-mixin text:wide-snip%))]
              [op (send t get-out-port)])
         (display ,big-str op)
         (flush-output op)
         (send t get-text 0 (send t last-position))))))
  
  (test 
   'text:ports%.thd3
   (λ (x) (equal? x non-ascii-str))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new (text:ports-mixin text:wide-snip%))]
              [op (send t get-out-port)])
         (display ,non-ascii-str op)
         (flush-output op)
         (send t get-text 0 (send t last-position))))))
  
  (test 
   'text:ports%.thd4
   (λ (x) (equal? x non-ascii-str))
   (λ ()
     (queue-sexp-to-mred
      `(let* ([t (new (text:ports-mixin text:wide-snip%))]
              [op (send t get-out-port)])
         (display ,non-ascii-str op)
         (flush-output op)
         (send t get-text 0 (send t last-position)))))))
