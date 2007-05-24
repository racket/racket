
(module mreval mzscheme
  (require (lib "eval.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "config.ss" "scribble")
           (lib "scheme.ss" "scribble")
           (lib "class.ss")
           (lib "file.ss")
           (lib "runtime-path.ss")
           (lib "serialize.ss")
           "exn.ss")

  (define-syntax define-mr
    (syntax-rules ()
      [(_ mr orig)
       (begin
         (provide mr)
         (define-syntax mr
           (syntax-rules ()
             [(_ x (... ...))
              (parameterize ([scribble-eval-handler mr-eval-handler]
                             [current-int-namespace mr-namespace])
                (orig x (... ...)))])))]))
  
  (define-mr mr-interaction interaction)
  (define-mr mr-interaction-eval interaction-eval)
  (define-mr mr-interaction-eval-show interaction-eval-show)
  (define-mr mr-def+int def+int)
  (define-mr mr-defs+int defs+int)
  (define-mr mr-schememod+eval schememod+eval)
  (define-mr mr-schemeblock+eval schemeblock+eval)
  
  (define mred? (getenv "MREVAL"))

  (define-runtime-path img-dir "images")
  (define-runtime-path exn-module "exn.ss")

  (define mr-eval-handler
    (if mred?
        (let ([eh (scribble-eval-handler)]
              [log-file (open-output-file (build-path img-dir "exprs.dat") 'truncate/replace)])
          (lambda (catching-exns? expr)
            (write (serialize expr) log-file)
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
                          (open-input-file (build-path img-dir "exprs.dat")))])
          (lambda (catching-exns? expr)
            (with-handlers ([exn:fail? (lambda (exn)
                                         (if catching-exns?
                                             (raise exn)
                                             (void)))])
              (let ([v (read log-file)])
                (if (eof-object? v)
                    (error "expression not in log file")
                    (let ([v (deserialize v)])
                      (if (equal? v expr)
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
  
  (define mr-namespace
    (if mred?
        (current-int-namespace ((dynamic-require '(lib "mred.ss" "mred") 'make-namespace-with-mred)))
        (let ([ns (make-namespace)])
          (namespace-attach-module (current-namespace)
                                   '(lib "struct.ss" "scribble")
                                   ns)
          (namespace-attach-module (current-namespace)
                                   exn-module
                                   ns)
          ns)))
  
  (define image-counter 0)

  (define (ss:pict?)
    (with-handlers ([exn:fail? (lambda (x) (lambda (x) #f))])
      (eval 'pict? mr-namespace)))
  (define (ss:pict-width)
    (eval 'pict-width mr-namespace))
  (define (ss:pict-height)
    (eval 'pict-height mr-namespace))
  (define (ss:make-pict-drawer)
    (eval 'make-pict-drawer mr-namespace))
  (define (ss:colorize)
    (eval 'colorize mr-namespace))
  (define (mred:canvas%)
    (dynamic-require '(lib "mred.ss" "mred") 'canvas%))
  (define (mred:bitmap%)
    (dynamic-require '(lib "mred.ss" "mred") 'bitmap%))
  (define (mred:bitmap-dc%)
    (dynamic-require '(lib "mred.ss" "mred") 'bitmap-dc%))

  (define (fixup-picts v)
    (cond
     [((ss:pict?) v)
      (let ([fn (build-path img-dir (format "img~a.png" image-counter))])
        (set! image-counter (add1 image-counter))
        (let* ([bm (make-object (mred:bitmap%)
                                (inexact->exact (ceiling ((ss:pict-width) v)))
                                (inexact->exact (ceiling ((ss:pict-height) v))))]
               [dc (make-object (mred:bitmap-dc%) bm)])
          (send dc set-smoothing 'aligned)
          (send dc clear)
          (((ss:make-pict-drawer) ((ss:colorize) v value-color)) dc 0 0)
          (send bm save-file fn 'png)
          (make-element #f (list (make-element (make-image-file fn) (list "[image]"))))))]
     [(is-a? v (mred:canvas%))
      (make-object:canvas%)]
     [(pair? v) (cons (fixup-picts (car v))
                      (fixup-picts (cdr v)))]
     [else v])))
