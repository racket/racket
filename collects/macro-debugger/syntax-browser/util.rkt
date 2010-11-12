#lang racket/base
(require racket/class)
(provide with-unlock
         make-text-port)

;; with-unlock SYNTAX (expression)
;; (with-unlock text-expression . body)
(define-syntax with-unlock
  (syntax-rules ()
    [(with-unlock text . body)
     (let* ([t text]
            [locked? (send t is-locked?)])
       (dynamic-wind
         (lambda ()
           (send* t
             (begin-edit-sequence #f)
             (lock #f)))
         (lambda () . body)
         (lambda ()
           (send* t
             (lock locked?)
             (end-edit-sequence)))))]))

;; make-text-port : text (-> number) -> port
;; builds a port from a text object.  
(define (make-text-port text end-position)
  (make-output-port #f
                    always-evt
                    (lambda (s start end flush? enable-break?)
                      (send text insert
                            (bytes->string/utf-8 s #f start end)
                            (end-position))
                      (- end start))
                    void
                    (lambda (special buffer? enable-break?)
                      (send text insert special (end-position))
                      #t)))
