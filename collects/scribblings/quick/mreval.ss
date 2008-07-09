
(module mreval mzscheme
  (require scribble/eval
           scribble/struct
           scribble/scheme
           mzlib/class
           mzlib/file
           mzlib/runtime-path
           mzlib/serialize
           scribblings/quick/exn)

  (define-syntax define-mr
    (syntax-rules ()
      [(_ mr orig)
       (begin
         (provide mr)
         (define-syntax mr
           (syntax-rules ()
             [(_ x (... ...))
              (parameterize ([scribble-eval-handler mr-eval-handler])
                (orig #:eval mr-eval x (... ...)))])))]))

  (define mr-eval (make-base-eval))
  
  (define-mr mr-interaction interaction)
  (define-mr mr-interaction-eval interaction-eval)
  (define-mr mr-interaction-eval-show interaction-eval-show)
  (define-mr mr-def+int def+int)
  (define-mr mr-defs+int defs+int)
  (define-mr mr-schememod+eval schememod+eval)
  (define-mr mr-schemeblock+eval schemeblock+eval)
  
  (define mred? (getenv "MREVAL"))

  (when mred?
    (mr-eval '(require scheme/gui/base))
    (mr-eval '(require slideshow)))

  ;; This one needs to be relative, because it ends up in the
  ;;  exprs.dat file:
  (define img-dir "images") ; relative to src dir

  ;; This one can be absolute:
  (define exprs-dat-file (build-path "images"
                                     "exprs.dat"))

  (define mr-eval-handler
    (if mred?
        (let ([eh (scribble-eval-handler)]
              [log-file (open-output-file exprs-dat-file 'truncate/replace)])
          (lambda (ev catching-exns? expr)
            (write (serialize (if (syntax? expr) (syntax-object->datum expr) expr)) log-file)
            (newline log-file)
            (flush-output log-file)
            (let ([result
                   (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (make-mr-exn (exn-message exn)))])
                     (eh ev catching-exns? expr))])
              (let ([result (fixup-picts result)])
                (write (serialize result) log-file)
                (newline log-file)
                (flush-output log-file)
                (if (mr-exn? result)
                    (raise (make-exn:fail
                            (mr-exn-message result)
                            (current-continuation-marks)))
                    result)))))
        (let ([log-file (with-handlers ([exn:fail:filesystem?
                                         (lambda (exn)
                                           (open-input-string ""))])
                          (open-input-file exprs-dat-file))])
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
                                        (syntax-object->datum expr)
                                        expr))
                          (let ([v (read log-file)])
                            (if (eof-object? v)
                                (error "expression result missing in log file")
                                (let ([v (deserialize v)])
                                  (if (mr-exn? v)
                                      (raise (make-exn:fail
                                              (mr-exn-message v)
                                              (current-continuation-marks)))
                                      v))))
                          (error 'mreval
                                 "expression does not match log file: ~e versus: ~e"
                                 expr
                                 v))))))))))
  
  (define image-counter 0)

  ;; This path will be marshaled for use on multiple platforms
  (define (build-string-path a b) (string-append a "/" b))

  (define (fixup-picts v)
    (cond
     [((mr-eval 'pict?) v)
      (let ([fn (build-string-path img-dir
                                   (format "img~a.png" image-counter))])
        (set! image-counter (add1 image-counter))
        (let* ([bm (make-object (mr-eval 'bitmap%)
                                (inexact->exact (ceiling ((mr-eval 'pict-width) v)))
                                (inexact->exact (ceiling ((mr-eval 'pict-height) v))))]
               [dc (make-object (mr-eval 'bitmap-dc%) bm)])
          (send dc set-smoothing 'aligned)
          (send dc clear)
          (((mr-eval 'make-pict-drawer) v) dc 0 0)
          (send bm save-file fn 'png)
          (make-element #f (list (make-element (make-image-file fn 1.0) (list "[image]"))))))]
     [(pair? v) (cons (fixup-picts (car v))
                      (fixup-picts (cdr v)))]
     [(serializable? v) v]
     [else (make-element #f (list (format "~s" v)))])))

