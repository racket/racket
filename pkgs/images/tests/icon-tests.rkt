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
   (running-stickman-icon t #:height stickman-height)))

;; ===================================================================================================
;; Other icons, various colors

(define icon-procss
  (list (list search-backward-icon rewind-icon continue-backward-icon step-back-icon back-icon
              pause-icon stop-icon record-icon play-icon step-icon continue-forward-icon
              fast-forward-icon search-forward-icon)
        (list right-arrow-icon left-arrow-icon up-arrow-icon down-arrow-icon
              right-over-arrow-icon left-over-arrow-icon right-under-arrow-icon left-under-arrow-icon)
        (list floppy-disk-icon
              (λ (#:color color) (save-icon #:arrow-color syntax-icon-color #:disk-color color))
              (λ (#:color color) (load-icon #:arrow-color syntax-icon-color #:disk-color color))
              (λ (#:color color) (small-save-icon #:arrow-color syntax-icon-color #:disk-color color))
              (λ (#:color color) (small-load-icon #:arrow-color syntax-icon-color #:disk-color color)))
        (list x-icon check-icon recycle-icon lambda-icon hash-quote-icon)
        (list stop-sign-icon stop-signs-icon foot-icon
              (λ (#:color color) (magnifying-glass-icon #:frame-color metal-icon-color
                                                        #:handle-color color))
              (λ (#:color color) (left-magnifying-glass-icon #:frame-color metal-icon-color
                                                             #:handle-color color))
              (λ (#:color color) (bomb-icon #:bomb-color color))
              (λ (#:color color) (left-bomb-icon #:bomb-color color))
              (λ (#:color color) (stopwatch-icon #:height (default-icon-height) #:face-color color)))))

(define tool-icon-procs
  (list check-syntax-icon small-check-syntax-icon
        macro-stepper-icon small-macro-stepper-icon
        debugger-icon small-debugger-icon))

(define (icons color)
  (for/list ([fs  icon-procss])
    (for/list ([f   fs])
      (f #:color color))))

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
