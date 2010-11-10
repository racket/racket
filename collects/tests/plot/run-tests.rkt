#!/bin/sh
#| -*- scheme -*-
exec gracket "$0" "$@"
|#
#lang scheme
(require plot file/md5 scheme/runtime-path
         racket/draw)

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
       (printf "testing \"~a\" ... " description)
       (let* ([bm1 (read-bitmap result-file-name)]
              [bm2 (read-bitmap expected-file-name)]
              [w (send bm1 get-width)]
              [h (send bm1 get-height)]
              [s1 (make-bytes (* 4 w h))]
              [s2 (make-bytes (* 4 w h))])
         (send bm1 get-argb-pixels 0 0 w h s1)
         (send bm2 get-argb-pixels 0 0 w h s2)
         (if (and (= (send bm2 get-width) w)
                  (= (send bm2 get-width) h)
                  ;; The generated and target images can be a little different,
                  ;; but not much --- less than 1/255 difference average difference
                  ;; over all RGB components (which is really pretty close)
                  ((/ (for/fold ([diff 0]) ([i (in-range (* w h 4))])
                        (+ diff (abs (- (bytes-ref s1 i) (bytes-ref s2 i)))))
                      (* w h 4))
                   . <= .
                   1.0))
             (begin (display "passed\n") (delete-file result-file-name))
             (begin
               (printf "failed! expected results in ~a, plot produced results in ~a\n"
                       expected-file-name
                       result-file-name)))))]))

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
