
(module mreval mzscheme
  (require (lib "eval.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "scheme.ss" "scribble")
           (lib "class.ss")
           (lib "file.ss")
           (lib "runtime-path.ss")
           (lib "serialize.ss")
           "slideshow-doc.ss"
           "mred-doc.ss"
           (lib "exn.ss" "scribblings" "quick"))

  (define-syntax define-mr
    (syntax-rules ()
      [(_ mr orig)
       (begin
         (provide mr)
         (define-syntax mr
           (syntax-rules ()
             [(_ x (... ...))
              (parameterize ([scribble-eval-handler mr-eval-handler])
                (orig x (... ...)))])))]))
  
  (define-mr mr-interaction interaction)
  (define-mr mr-interaction-eval interaction-eval)
  (define-mr mr-interaction-eval-show interaction-eval-show)
  (define-mr mr-def+int def+int)
  (define-mr mr-defs+int defs+int)
  (define-mr mr-schememod+eval schememod+eval)
  (define-mr mr-schemeblock+eval schemeblock+eval)
  
  (define mred? (getenv "MREVAL"))

  ;; This one needs to be relative, because it ends up in the
  ;;  exprs.dat file:
  (define img-dir "quick/images") ; relative to scribbles dir

  ;; This one can be absolute:
  (define exprs-dat-file (build-path (collection-path "scribblings")
                                     "quick"
                                     "images"
                                     "exprs.dat"))

  (define mr-eval-handler
    (if mred?
        (let ([eh (scribble-eval-handler)]
              [log-file (open-output-file exprs-dat-file 'truncate/replace)])
          (lambda (catching-exns? expr)
            (write (serialize (syntax-object->datum expr)) log-file)
            (newline log-file)
            (flush-output log-file)
            (let ([result
                   (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (make-mr-exn (exn-message exn)))])
                     (eh catching-exns? expr))])
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
          (lambda (catching-exns? expr)
            (with-handlers ([exn:fail? (lambda (exn)
                                         (if catching-exns?
                                             (raise exn)
                                             (void)))])
              (let ([v (read log-file)])
                (if (eof-object? v)
                    (error "expression not in log file")
                    (let ([v (deserialize v)])
                      (if (equal? v (syntax-object->datum expr))
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
  
  (define mr-namespace (current-namespace))
  
  (define image-counter 0)

  (define (fixup-picts v)
    (cond
     [(pict? v)
      (let ([fn (format "~a/img~a.png" img-dir image-counter)])
        (set! image-counter (add1 image-counter))
        (let* ([bm (make-object bitmap%
                                (inexact->exact (ceiling (pict-width v)))
                                (inexact->exact (ceiling (pict-height v))))]
               [dc (make-object bitmap-dc% bm)])
          (send dc set-smoothing 'aligned)
          (send dc clear)
          ((make-pict-drawer (colorize v (make-object color% 0 0 #xAF))) dc 0 0)
          (send bm save-file fn 'png)
          (make-element #f (list (make-element (make-image-file fn) (list "[image]"))))))]
     [(pair? v) (cons (fixup-picts (car v))
                      (fixup-picts (cdr v)))]
     [(serializable? v) v]
     [else (make-element #f (list (format "~s" v)))])))

