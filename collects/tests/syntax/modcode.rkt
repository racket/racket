#lang racket
(require syntax/modcode)

(define dir (find-system-path 'temp-dir))
(define file.rkt (build-path dir "file.rkt"))
(define file.ss (build-path dir "file.ss"))

(define compiled-dir (build-path dir "compiled"))
(make-directory* compiled-dir)

(define (test expect proc . args)
  (define val (apply proc args))
  (unless (equal? expect val)
    (error 'fail "at (apply ~s '~s): ~e is not expected: ~e" proc args val expect)))

(define (try file.sfx src? zo? so?)
  (printf "trying ~s\n" (list file.sfx src? zo? so?))
  (define file.zo
    (let-values ([(base name dir?) (split-path file.sfx)])
      (build-path base "compiled" (path-add-suffix name #".zo"))))
  (define file.rkt (path-replace-suffix file.sfx #".rkt"))
  (define file.so (path-replace-suffix file.sfx #".so"))
  (when (file-exists? file.ss) (delete-file file.ss))
  (when (file-exists? file.rkt) (delete-file file.rkt))
  (when (file-exists? file.zo) (delete-file file.zo))
  (define ns (make-base-namespace))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (when src?
       (call-with-output-file*
        file.sfx
        (lambda (o)
          (write '(module file racket/base 
                    10
                    (module inner racket/base 11))
                 o))))
     (when zo?
       (call-with-output-file* 
        file.zo
        (lambda (o)
          (write (parameterize ([current-namespace ns])
                   (compile '(module file racket/base 
                               12
                               (module inner racket/base 13))))
                 o))))
     (define (check-name name code)
       (test name module-compiled-name code))
     (define-values (m-path m-type)
       (get-module-path file.sfx
                        #:choose (lambda (src zo so)
                                   (test src? file-exists? src)
                                   (test zo? file-exists? zo)
                                   (test so? file-exists? so)
                                   #f)))
     (test
       (cond
         [zo? (list file.zo 'zo)]
         [so? (list file.so 'so)]
         [else (list file.sfx 'src)])
       list m-path m-type)
     (check-name
      'file
      (get-module-code file.sfx
                       #:choose (lambda (src zo so)
                                  (test src? file-exists? src)
                                  (test zo? file-exists? zo)
                                  (test so? file-exists? so)
                                  #f)))
     (define-values (sm-path sm-type)
       (get-module-path file.sfx
                        #:submodule? #t
                        #:choose (lambda (src zo so)
                                   (test src? file-exists? src)
                                   (test zo? file-exists? zo)
                                   (test so? file-exists? so)
                                   #f)))
     (test
       (cond
         [zo? (list file.zo 'zo)]
         [else (list file.sfx 'src)])
       list sm-path sm-type)
     (check-name
      '(file inner)
      (get-module-code file.sfx
                       #:submodule-path '(inner)
                       #:choose (lambda (src zo so)
                                  (test src? file-exists? src)
                                  (test zo? file-exists? zo)
                                  (test so? file-exists? so)
                                  #f)))
     (void))
   (lambda ()
     (when src? (delete-file file.sfx))
     (when zo? (delete-file file.zo)))))

(try file.rkt #t #f #f)
(try file.rkt #t #t #f)
(try file.rkt #f #t #f)
(try file.ss #t #f #f)
(try file.ss #t #t #f)
(try file.ss #f #t #f)

(delete-directory/files compiled-dir)
