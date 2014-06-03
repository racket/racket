#lang racket

#|

Shell script to read in the ../problems/raw-*.rkt file and produce
the files in the ../solution-sets subdirectory via John's solver. 

This file must produce code that evaluates to a list of problem
structs. The problem struct should have four fields: a string, a col,
a row and a (union #f solution)

The col and row type specs are in sig.rkt and the solution type is:

  (vectorof (vectorof (union 'on 'off 'unknown)))

|#


(define argv (current-command-line-arguments))

(require racket/gui
         "raw-hattori.rkt"
         (prefix-in solve: "../solve.rkt"))

(if (equal? (vector) argv)
    (printf "pass any command line argument to skip the solver\n\n")
    (printf "skipping the solver\n"))

(define memory-limit (* 1024 1024 400)) ;; in bytes (500 megs)

(define memory-frame%
  (class frame%
    (define/augment (can-close?) #f)
    (super-instantiate ())))

(define memory-frame (parameterize ([current-eventspace (make-eventspace)])
                       (make-object memory-frame% "memory stats frame" #f 500 50)))
(define memory-hp (make-object horizontal-panel% memory-frame))
(define memory-vp (make-object vertical-panel% memory-hp))
(define memory-text (make-object text%))
(define memory-ec (make-object editor-canvas% memory-vp memory-text '(hide-hscroll hide-vscroll)))
(define memory-gauge (make-object gauge% #f 10000 memory-vp))
(define memory-canvas (make-object canvas% memory-hp))
(define memory-on-bitmap (make-object bitmap% (build-path (collection-path "icons") "recycle.png")))
(define memory-off-bitmap (make-object bitmap%
                            (send memory-on-bitmap get-width)
                            (send memory-on-bitmap get-height)))

(let ([memory-off-bitmap-dc (make-object bitmap-dc% memory-off-bitmap)])
  (send memory-off-bitmap-dc clear)
  (send memory-off-bitmap-dc set-bitmap #f))

(register-collecting-blit memory-canvas 0 0
                          (send memory-on-bitmap get-width) (send memory-on-bitmap get-height)
                          memory-on-bitmap memory-off-bitmap)
(send memory-canvas min-width (send memory-on-bitmap get-width))
(send memory-canvas min-height (send memory-on-bitmap get-height))
(send memory-canvas stretchable-width #f)
(send memory-canvas stretchable-height #f)
(send memory-ec set-line-count 1)
(send memory-text hide-caret #t)
(define (format-memory-txt use)
  (format "~a megs (~a bytes)" (bytes->megs use) use))
(define (bytes->megs n) (floor (/ n 1024 1024)))
(define (update-memory-display)
  (let ([use (current-memory-use)])
    (send memory-text lock #f)
    (send memory-text begin-edit-sequence)
    (send memory-text erase)
    (send memory-text insert (format-memory-txt use))
    (send memory-text end-edit-sequence)
    (send memory-text lock #t)
    (send memory-gauge set-value (min 10000 (floor (* 10000 (/ use memory-limit)))))))
(update-memory-display)
(send memory-frame show #t)

(define problems-dir (collection-path "games" "paint-by-numbers"))

(define hattori-sets
  (let* ([set-size 30]
         [hattori-count (length raw-hattori)])
    (let o-loop ([n 0])
      (cond
        [(= n (- hattori-count 1)) null]
        [else 
         (let ([first n]
               [last (if (< (+ n set-size) hattori-count)
                         (+ n set-size)
                         (- hattori-count 1))])
           (let i-loop ([i first]
                        [set null])
             (cond
               [(= i last) (cons 
                            (list (format "Hattori ~a - ~a" (+ first 1) last)
                                  (format "h~a-~a" (+ first 1) last)
                                  (reverse set))
                            (o-loop last))]
               [else (i-loop (+ i 1)
                             (cons (list-ref raw-hattori i)
                                   set))])))]))))

(define (build-set name output-file input-file)
  (list name
        output-file
        (call-with-input-file (build-path problems-dir input-file) (compose eval read))))

(require "raw-problems.rkt")
(define games-set (list "Games Magazine" "games" raw-problems))

(require "raw-misc.rkt")
(define misc-set (list "Misc" "misc" raw-misc))

(require "raw-kajitani.rkt")
(define kajitani-sets raw-kajitani)

(define sets (append (list games-set)
                     (list misc-set)
                     kajitani-sets
                     hattori-sets))

(define (sum-list l) (apply + l))
(define (sum-lists ls) (sum-list (map sum-list ls)))

(define board #f)
(define known 0)
(define solving-progress-output void)

(define (set-entry i j nv)
  (when (and (eq? (get-entry i j) 'unknown)
             (not (eq? nv 'unknown)))
    (solving-progress-output))
  (vector-set! (vector-ref board i) j nv))

(define (get-entry i j)
  (vector-ref (vector-ref board i) j))

(define progress-bar-max 64)
(define guide-string ".......:.......|.......:.......|.......:.......|.......:........")

(define (build-progress-outputer max cleanup)
  (let ([counter 0]
        [dots-printed 0])
    (lambda ()
      (set! counter (+ 1 counter))
      (cond
        [(= counter max)
         
         (cleanup)
         
         ;; dots-printed should always equal progress-bar-max
         (let loop ([n (- progress-bar-max dots-printed)])
           (cond
             [(zero? n) (void)]
             [else (display ".")
                   (loop (- n 1))]))
         (newline)]
        [else
         (let ([dots-to-print (floor (- (* progress-bar-max (/ counter (- max 1))) dots-printed))])
           '(printf "~spercentage: ~a ~a ~a ~a\n"
                     cleanup
                     dots-to-print
                     counter
                     (exact->inexact (/ counter max))
                     (exact->inexact (* progress-bar-max (/ counter max))))
           (set! dots-printed (+ dots-to-print dots-printed))
           (let loop ([n dots-to-print])
             (cond
               [(zero? n) (void)]
               [else
                (display ".")
                (loop (- n 1))]))
           (flush-output))]))))

(define (setup-progress max cleanup)
  (display guide-string)
  (newline)
  (build-progress-outputer max cleanup))

(define (solve name rows cols)
  (cond
    [(equal? argv (vector))
     (printf "Solving ~s; memory limit ~a\n"
              name (format-memory-txt memory-limit))
     (let ([row-count (length rows)]
           [col-count (length cols)])
       (set! board
             (build-vector col-count
                           (lambda (i) (make-vector row-count 'unknown))))
       (set! known 0)
       (set! solving-progress-output (build-progress-outputer
                                      (* row-count col-count)
                                      void)))
     (letrec ([done (make-semaphore 0)]
              [kill (make-semaphore 1)]
              [sucessful? #f]
              [t (thread
                  (lambda ()
                    (with-handlers ([(lambda (x) #t)
                                     (lambda (x)
                                       (semaphore-wait kill)
                                       (set! sucessful? #f)
                                       (kill-thread k)
                                       (eprintf "\nsolver raised an exception\n~a\n"
                                                (if (exn? x) (exn-message x) x))
                                       (semaphore-post done))])
                      (solve:solve rows cols set-entry
                                   (lambda (max)
                                     (setup-progress
                                      max
                                      (lambda ()
                                        (semaphore-wait kill)
                                        (set! sucessful? #t)
                                        (kill-thread k)))))
                      (semaphore-post done))))]
              [k
               (thread
                (lambda ()
                  (let ([check-interval 10]) ;; in seconds
                    (let loop ()
                      (sleep check-interval)
                      (update-memory-display)
                      (if (<= (current-memory-use) memory-limit)
                          (loop)
                          (begin (collect-garbage)(collect-garbage)(collect-garbage)
                                 (update-memory-display)
                                 (if (<= (current-memory-use) memory-limit)
                                     (loop)
                                     (void))))))
                  (semaphore-wait kill)
                  (kill-thread t)
                  (eprintf "\n memory limit expired.\n")
                  (collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
                  (update-memory-display)
                  (semaphore-post done)))])
       (semaphore-wait done)
       (newline)
       (newline)
       (if sucessful?
           board
           #f))]
    [else #f]))

(define (sanity-check problem)
  (let ([name (first problem)]
        [cols (second problem)]
        [rows (third problem)])
    (when (null? cols)
      (error 'build-solution-sets.rkt
             "problem ~a doesn't have any cols" name))
    (when (null? rows)
      (error 'build-solution-sets.rkt
             "problem ~a doesn't have any rows" name))
    (unless (= (sum-lists cols) (sum-lists rows))
      (error 'build-solution-sets.rkt
             "problem ~a: sum of the column lists is not the same as the sum of the row lists"
             name))))


(for-each 
 (lambda (set)
   (let ([set-name (car set)]
         [output-file (build-path (collection-path "games" "paint-by-numbers" "solution-sets")
                                  (cadr set))]
         [problems (caddr set)])
     (for-each sanity-check problems)
     (if (file-exists? output-file)
         (printf "skipping ~s (~a)\n" set-name (normalize-path output-file))
         (call-with-output-file output-file
           (lambda (port)
             (printf "Building ~s\n" set-name)
             (parameterize ([current-output-port port])
               (write
                `(unit 
                   (import paint-by-numbers:problem^)
                   (export paint-by-numbers:problem-set^)
                   (define set-name ,set-name)
                   
                   (define problems
                     (list
                      ,@(map (lambda (x)
                               (let ([name (first x)]
                                     [rows (second x)]
                                     [cols (third x)])
                                 `(make-problem
                                   ,(first x)
                                   ',rows
                                   ',cols
                                   ',(solve name rows cols))))
                             problems)))))))))))
 sets)
