#!/bin/sh
#| -*- scheme -*-
exec mred  "$0" "$@"
|#
#lang scheme
(require plot file/md5 scheme/runtime-path)

(define-runtime-path here "./")

(define (read-file file)
  (with-input-from-file file (lambda () (read-bytes (file-size file)))))

(define-syntax run-test
  (syntax-rules ()
    [(_ description (plot args ...) file-name)
     (let* ([result-file-name
             (build-path here (string-append file-name "-out.png"))]
            [expected-file-name
             (build-path here (string-append file-name ".png"))])
       (plot args ... #:out-file result-file-name)
       ;; WILL COMPARE by MD5 hash.
       (printf "testing \"~a\" ... " description)
       (if (equal? (md5 (read-file result-file-name))
                   (md5 (read-file expected-file-name)))
         (begin (display "passed\n") (delete-file result-file-name))
         (printf "failed! expected results in ~a, plot produced results in ~a\n"
                 expected-file-name
                 result-file-name)))]))

(run-test "Line"
          (plot (line (lambda (x) x) #:color 'red))
          "red-identity")

(run-test "Vector Field"
          (plot (vector-field (gradient (lambda (x y) (* (sin x) (cos y))))
                              #:samples 25)
                #:title "gradient field of F(x,y) = sin(x) * sin(y)")
          "vector-field")

(define (trig x y) (* (sin x) (sin y)))

(run-test "Shading"
          (plot (shade trig)
                #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                #:title "shdade of F(x,y) = sin(x) * sin(y)")
          "shade")

(run-test "Contours"
          (plot (contour trig)
                #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                #:title "contours of F(x,y) = sin(x) * sin(y)")
          "contours")

(run-test "Mix of all three"
          (plot (mix (shade trig)
                     (contour trig)
                     (vector-field (gradient trig) #:samples 25))
                #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                #:title "gradient field & shade & contours")
          "mix")

(run-test "3d mesg"
          (plot3d (mesh3d trig)
                  #:x-min -3.5 #:x-max 3.5
                  #:y-min -3.5 #:y-max 3.5
                  #:z-min -1.0 #:z-max 1.5
                  #:bgcolor '(0 0 0) #:fgcolor '(255 0 0))
          "3d-mesh")

(require plot/plot-extend)

; (number -> number) mumbo-jumbo -> 2d-renderer
(define-plot-type dashed-line
  fun 2dview (x-min x-max) ((samples 100) (segments 20) (color 'red) (width 1))
    (let* ((dash-size (/ (- x-max x-min) segments))
           (x-lists (build-list (/ segments 2)
                                (lambda (index)
                                  (x-values
                                   (/ samples segments)
                                   (+ x-min (* 2 index dash-size))
                                   (+ x-min (* (add1 ( * 2 index)) dash-size)))))))
      (send* 2dview
        (set-line-color color)
        (set-line-width width))
      (for-each (lambda (dash)
                  (send 2dview plot-line 
                        (map (lambda (x) (vector x (fun x))) dash))) 
                x-lists)))

(run-test "Simple plot-extend"
          (plot (dashed-line (lambda (x) x) [color 'red]))
          "dashed-line")

(run-test "canvas sizing"
          (plot (line sin) #:height 100 #:width 100)
          "size")
