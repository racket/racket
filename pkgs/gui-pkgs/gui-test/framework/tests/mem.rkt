#lang racket/base
(require "test-suite-utils.rkt")

(module test racket/base)

; mem-boxes : (list-of (list string (list-of (weak-box TST))))
(send-sexp-to-mred '(define mem-boxes null))

(define mem-count 10)

(define (test-allocate tag open close)
  (queue-sexp-to-mred
   `(let ([new-boxes
           (let loop ([n ,mem-count])
             (cond
               [(zero? n) null]
               [else
                (let* ([o (,open)]
                       [b (make-weak-box o)])
                  (,close o)

                  ;; break at least that link.
                  (set! o #f)

                  ;; flush pending events
                  (let ([s (make-semaphore 0)])
                    (queue-callback (lambda () (semaphore-post s)) #f)
                    (yield s))

                  (cons b (loop (- n 1))))]))])
      (sleep/yield 1/10) (collect-garbage)
      (sleep/yield 1/10) (collect-garbage)
      (sleep/yield 1/10) (collect-garbage)
      (set! mem-boxes (cons (list ,tag new-boxes) mem-boxes)))))

(define (done)
  (queue-sexp-to-mred
   `(begin
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (let* ([f (make-object dialog% "Results" #f 300 500)]
             [text (make-object text%)]
             [ec (make-object editor-canvas% f text)]
             [hp (instantiate horizontal-panel% () 
                   (parent f)
                   (stretchable-width #f)
                   (stretchable-height #f))]
             [vp (instantiate vertical-panel% ()
                   (parent hp)
                   (stretchable-width #f)
                   (stretchable-height #f))]
             [gc-canvas (make-object canvas% hp '(border))]
             [anything? #f])

        (define (update-gui)
          (send text erase)
          (let ([anything? #f])
            (send text begin-edit-sequence)
            (for-each
             (lambda (boxl)
               (let* ([tag (car boxl)]
                      [boxes (cadr boxl)]
                      [calc-results
                       (lambda ()
                         (let loop ([boxes boxes]
                                    [n 0])
                           (cond
                             [(null? boxes) n]
                             [else (if (weak-box-value (car boxes))
                                     (loop (cdr boxes) (+ n 1))
                                     (loop (cdr boxes) n))])))])
                 (let ([res (calc-results)])
                   (when (> res 0)
                     (set! anything? #t)
                     (send text insert (format "~a: ~a of ~a\n" tag res ,mem-count))))))
             (reverse mem-boxes))
            (unless anything?
              (send text insert "Nothing!\n"))
            (send text end-edit-sequence)))

        (update-gui)

        (let ([onb (icon:get-gc-on-bitmap)]
              [offb (icon:get-gc-off-bitmap)])
          (when (and (send onb ok?)
                     (send offb ok?))
            (send* gc-canvas
              (min-client-width (max (send gc-canvas min-width) (send onb get-width)))
              (min-client-height (max (send gc-canvas min-height) (send onb get-height)))
              (stretchable-width #f)
              (stretchable-height #f))
            (register-collecting-blit gc-canvas 
                                      0 0
                                      (send onb get-width)
                                      (send onb get-height)
                                      onb offb)))

        (make-object button% "Collect" vp
                     (lambda (x y)
                       (send text erase)
                       (send text insert "Collecting Garbage\n")
                       (collect-garbage)(collect-garbage)(collect-garbage)
                       (collect-garbage)(collect-garbage)(collect-garbage)
                       (collect-garbage)(collect-garbage)(collect-garbage)
                       (update-gui)))
        (make-object button% "Close" vp (lambda (x y) (send f show #f)))
        (send f show #t)))))

(define (test-frame-allocate %)
  (let ([name (format "~s" %)])
    (queue-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #f))
    (test-allocate name
                   `(lambda ()
                      (let ([f (make-object ,% ,name)])
                        (send f show #t)
                        (yield) (yield)
                        f))
                   `(lambda (f)
                      (yield) (yield)
                      (send f close)
                      (when (send f is-shown?)
                        (error 'test-frame-allocate "~a instance didn't close" ',%))
                      (yield) (yield)))
    (queue-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #t))))

(test-allocate "frame%"
               '(lambda ()
                  (let ([f (make-object frame% "test frame")])
                    (send f show #t)
                    f))
               '(lambda (f) (send f show #f)))

(define (test-editor-allocate object-name)
  (test-allocate (symbol->string object-name)
                 `(lambda () (make-object ,object-name))
                 '(lambda (e) (send e on-close))))

(test-editor-allocate 'text:basic%)
(test-editor-allocate 'text:keymap%)
(test-editor-allocate 'text:autowrap%)
(test-editor-allocate 'text:file%)
(test-editor-allocate 'text:clever-file-format%)
(test-editor-allocate 'text:backup-autosave%)
(test-editor-allocate 'text:searching%)
(test-editor-allocate 'text:info%)

(test-editor-allocate 'pasteboard:basic%)
(test-editor-allocate 'pasteboard:keymap%)
(test-editor-allocate 'pasteboard:file%)
(test-editor-allocate 'pasteboard:backup-autosave%)
(test-editor-allocate 'pasteboard:info%)

(test-editor-allocate 'racket:text%)

(test-allocate "text:return%"
               '(lambda () (make-object text:return% void))
               '(lambda (t) (void)))

(test-frame-allocate '(class frame% (inherit show) (define/public (close) (show #f)) (super-new)))
(test-frame-allocate 'frame:basic%)
(test-frame-allocate 'frame:info%)
(test-frame-allocate 'frame:text-info%)
(test-frame-allocate 'frame:pasteboard-info%)
(test-frame-allocate 'frame:standard-menus%)

(test-frame-allocate 'frame:text%)
(test-frame-allocate 'frame:searchable%)

(test-frame-allocate 'frame:pasteboard%)
(done)
