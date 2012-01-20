#lang racket

(require racket/draw
         images/icons/control
         images/icons/arrow
         images/icons/file
         images/icons/symbol
         images/icons/misc
         images/icons/tool
         images/icons/style
         images/private/deep-flomap-render
         images/private/utils
         images/compile-time
         (for-syntax images/icons/stickman
                     images/icons/style))

(default-icon-height 16)
;(default-icon-material glass-icon-material)
#;(default-icon-material
  (deep-flomap-material-value 3.0 1.0 0.0 0.0
                              0.0 0.0 0.0
                              0.0 0.0 0.0
                              0.0))
;(default-icon-material diamond-material)

;; ===================================================================================================
;; Compiled stickman test

(begin-for-syntax
  (define stickman-height 32)
  (define num-running-frames 12))

(compiled-bitmap-list
 (for/list ([t  (in-range 0 1 (/ 1 num-running-frames))])
   (running-stickman-icon t run-icon-color "white" run-icon-color stickman-height)))

;; ===================================================================================================
;; Other icons, various colors

(define icon-procss
  (list (list search-backward-icon rewind-icon continue-backward-icon step-back-icon back-icon
              pause-icon stop-icon record-icon play-icon step-icon continue-forward-icon
              fast-forward-icon search-forward-icon)
        (list right-arrow-icon left-arrow-icon up-arrow-icon down-arrow-icon
              right-over-arrow-icon left-over-arrow-icon right-under-arrow-icon left-under-arrow-icon)
        (list floppy-disk-icon
              (λ (color) (save-icon syntax-icon-color color))
              (λ (color) (load-icon syntax-icon-color color))
              (λ (color) (small-save-icon syntax-icon-color color))
              (λ (color) (small-load-icon syntax-icon-color color)))
        (list x-icon check-icon recycle-icon lambda-icon hash-quote-icon)
        (list octagon-icon stop-sign-icon stop-signs-icon foot-icon
              (λ (color) (magnifying-glass-icon metal-icon-color color))
              (λ (color) (left-magnifying-glass-icon metal-icon-color color))
              (λ (color) (bomb-icon metal-icon-color color))
              (λ (color) (left-bomb-icon metal-icon-color color))
              (λ (color) (stopwatch-icon (default-icon-height) color)))))

(define tool-icon-procs
  (list check-syntax-icon small-check-syntax-icon
        macro-stepper-icon small-macro-stepper-icon
        debugger-icon small-debugger-icon))

(define (icons color)
  (for/list ([fs  icon-procss])
    (for/list ([f   fs])
      (f color))))

(define (colored-icons-test)
  (printf "~v~n" (for/list ([f  tool-icon-procs])
                   (define bm (f))
                   (cons (send bm get-width) bm)))
  (for ([color  (list "red" "blue" "green"
                      "pink" "lightblue" "lightgreen"
                      "darkred" "darkgreen" "darkblue"
                      "white" "black" "tomato"
                      "orange" "cyan" "purple")])
    (define lst (time (icons color)))
    (void)
    (printf "~v~n" lst)
    #;(printf "~v~n" (read-caches))
    ))

(colored-icons-test)
