(require (lib "graphics.ss" "graphics")
         (lifted mzscheme
                 make-hash-table
                 hash-table-put!
                 hash-table-get))
#|
    "Lifted" is explained in FrTime's own documentation (plt/collects/frtime/doc.txt)
    Quickly put, lifting extends the functions listed above so they can take FrTime time-varying
    values (such as MzTake traces) as arguments. 
|#


(open-graphics)
(define window (open-viewport "Debugger" 600 500))

(debug-process p ("random-Xs.ss" [x-trace 4 6 bind 'x]))

(define x (hold x-trace))

(define valcount (make-hash-table))

((changes x) . ==> . (lambda (x)
                       (hash-table-put! valcount x (add1 (hash-table-get valcount x (lambda () 0))))
                       ((draw-solid-ellipse window) (make-posn (* x 3)
                                                               (- 500 (* 3 (hash-table-get valcount x (lambda () 1)))))
                                                    4 4 "blue")))

(define runtime (process:runtime/milliseconds p))
(printf-b "~a millisecs per event" (truncate (runtime . / . (add1 (count-e (changes x))))))

(printf-b "x: ~a" x)
(printf-b "count: ~a" (count-e (changes x)))

(let ([cnt (count-e (changes x))])
  (when (= 13000 cnt)
    ; pause on next breakpoint
    (pause p)))

(start/resume p)