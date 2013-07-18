#lang racket/base

(require scribble/eval
         scribble/core
         scribble/scheme
         racket/class
         racket/file
         racket/runtime-path
         racket/serialize
         "private/gui-eval-exn.rkt"
         racket/system
         racket/sandbox
         (for-syntax racket/base))

(define-syntax define-mr
  (syntax-rules ()
    [(_ mr orig)
     (begin
       (provide mr)
       (define-syntax (mr stx)
         (syntax-case stx ()
           [(_ #:eval+opts the-eval get-predicate? get-render get-get-width get-get-height x (... ...))
            #'(let ([the-eval-x the-eval])
                (parameterize ([scribble-eval-handler (gui-eval-handler the-eval-x
                                                                        get-predicate?
                                                                        get-render
                                                                        get-get-width
                                                                        get-get-height)])
                  (orig #:eval the-eval-x x (... ...))))]
           [(_ x (... ...))
            #'(parameterize ([scribble-eval-handler (gui-eval-handler gui-eval
                                                                      (λ () (gui-eval 'pict?))
                                                                      (λ () (gui-eval 'draw-pict))
                                                                      (λ () (gui-eval 'pict-width))
                                                                      (λ () (gui-eval 'pict-height)))])
                (orig #:eval gui-eval x (... ...)))])))]))

(define gui-eval (make-base-eval #:pretty-print? #f))

(define mred? (getenv "MREVAL"))
(define-namespace-anchor anchor)

(when mred?
  (call-in-sandbox-context gui-eval
                           (lambda ()
                             (namespace-attach-module (namespace-anchor->namespace anchor)
                                                      'racket/class)))
  (gui-eval '(require racket/gui/base))
  (gui-eval '(require slideshow)))

(define-mr gui-interaction interaction)
(define-mr gui-interaction-eval interaction-eval)
(define-mr gui-interaction-eval-show interaction-eval-show)
(define-mr gui-def+int def+int)
(define-mr gui-defs+int defs+int)
(define-mr gui-racketmod+eval racketmod+eval)
(define-mr gui-racketblock+eval racketblock+eval)

(provide (rename-out [gui-racketmod+eval gui-schememod+eval]
                     [gui-racketblock+eval gui-schemeblock+eval]))

;; This one needs to be relative, because it ends up in the
;;  exprs.dat file:
(define img-dir "images") ; relative to src dir

;; This one can be absolute:
(define exprs-dat-file (build-path "images"
                                   "exprs.dat"))

(define gui-eval-handler
  (if mred?
      (let ([eh (scribble-eval-handler)]
            [log-file (open-output-file exprs-dat-file #:exists 'truncate/replace)])
        (λ (gui-eval get-predicate? get-render get-get-width get-get-height)
          (lambda (ev catching-exns? expr)
            (write (serialize (if (syntax? expr) (syntax->datum expr) expr)) log-file)
            (newline log-file)
            (flush-output log-file)
            (let ([result
                   (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (make-gui-exn (exn-message exn)))])
                     ;; put the call to fixup-picts in the handlers
                     ;; so that errors in the user-supplied predicates & 
                     ;; conversion functions show up in the rendered output
                     (fixup-picts (get-predicate?) (get-render) (get-get-width) (get-get-height)
                                  (eh ev catching-exns? expr)))])
              (write (serialize result) log-file)
              (newline log-file)
              (flush-output log-file)
              (if (gui-exn? result)
                  (raise (make-exn:fail
                          (gui-exn-message result)
                          (current-continuation-marks)))
                  result)))))
      (let ([log-file (with-handlers ([exn:fail:filesystem?
                                       (lambda (exn)
                                         (open-input-string ""))])
                        (open-input-file exprs-dat-file))])
        (λ (gui-eval get-predicate? get-render get-get-width get-get-height)
          (lambda (ev catching-exns? expr)
            (with-handlers ([exn:fail? (lambda (exn)
                                         (if catching-exns?
                                             (raise exn)
                                             (void)))])
              (let ([v (read log-file)])
                (if (eof-object? v)
                    (error "expression not in log file")
                    (let ([v (deserialize v)])
                      (if (equal? v (if (syntax? expr)
                                        (syntax->datum expr)
                                        expr))
                          (let ([v (read log-file)])
                            (if (eof-object? v)
                                (error "expression result missing in log file")
                                (let ([v (deserialize v)])
                                  (if (gui-exn? v)
                                      (raise (make-exn:fail
                                              (gui-exn-message v)
                                              (current-continuation-marks)))
                                      v))))
                          (error 'mreval
                                 "expression does not match log file: ~e versus: ~e"
                                 expr
                                 v)))))))))))

(define image-counter 0)

;; This path will be marshaled for use on multiple platforms
(define (build-string-path a b) (string-append a "/" b))

(define (fixup-picts predicate? render get-width get-height v)
  (let loop ([v v])
    (cond
      [(predicate? v)
       (let ([fn (build-string-path img-dir
                                    (format "img~a.png" image-counter))])
         (set! image-counter (add1 image-counter))
         (let ([dc (let ([pss (make-object (gui-eval 'ps-setup%))])
                     (send pss set-mode 'file)
                     (send pss set-file (path-replace-suffix fn #".pdf"))
                     (parameterize ([(gui-eval 'current-ps-setup) pss])
                       (let ([xb (box 0)]
                             [yb (box 0)])
                         (send pss get-scaling xb yb)
                         (new (gui-eval 'pdf-dc%) 
                              [interactive #f]
                              [width (* (unbox xb) (get-width v))]
                              [height (* (unbox yb) (get-height v))]))))])
           (send dc start-doc "Image")
           (send dc start-page)
           (render v dc 0 0)
           (send dc end-page)
           (send dc end-doc))
         (let* ([bm (make-object (gui-eval 'bitmap%)
                      (inexact->exact (ceiling (get-width v)))
                      (inexact->exact (ceiling (get-height v))))]
                [dc (make-object (gui-eval 'bitmap-dc%) bm)])
           (send dc set-smoothing 'aligned)
           (send dc clear)
           (render v dc 0 0)
           (send bm save-file fn 'png)
           (make-image-element
            #f
            (list "[image]")
            ;; Be sure to use a string rather than a path, because
            ;; it gets recorded in "exprs.dat".
            (path->string (path-replace-suffix fn #""))
            '(".pdf" ".png")
            1.0)))]
      [(pair? v) (cons (loop (car v))
                       (loop (cdr v)))]
      [(serializable? v) v]
      [else (make-element #f (list (format "~s" v)))])))
