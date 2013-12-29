(module value-turtles-test mzscheme
  (require "value-turtles-examples.rkt"
           "value-turtles.rkt"
           mred
           mzlib/class)

  (module test racket/base)
  
  (define-syntax (test stx)
    (syntax-case stx ()
      [(_ body)
       (with-syntax ([body-string (format "~s" (syntax-object->datum (syntax body)))])
         (syntax
          (let* ([f (make-object frame% "frame" #f 600 600)]
                 [t (make-object text%)]
                 [mb (make-object menu-bar% f)]
                 [fm (make-object menu% "File" mb)])
            (send t insert body-string)
            (send t change-style (make-object style-delta% 'change-family 'modern)
                  0 (send t last-position))
            (send t insert #\newline)
            (make-object menu-item% "Copy" fm (lambda x (send t copy)) #\c)
            (make-object editor-canvas% f t)
            (send f show #t)
            (with-handlers ([exn:fail?
                             (lambda (x)
                               (send t insert (exn-message x))
                               (send t insert #\newline))])
              (send t insert body)
              (send t insert #\newline)))))]))
  
  (test (regular-poly 5 30 (turtles 150 150)))
  (test (regular-polys 5 30 (turtles 150 150)))
  (test (draw 20 (radial-turtles 5 (turtles 150 150))))
  (test (draw 20 (turn 90 (spaced-turtles 5 (turtles 150 150 10 75 0)))))
  (test (spokes (turtles 200 200)))
  (test (spyro-gyra (turtles 200 200)))
  (test (neato (turtles 400 400))))
