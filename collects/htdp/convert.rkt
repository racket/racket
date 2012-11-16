#lang racket/base

(require racket/gui)

;; ---------------------------------------------------------------------------------------------------
;; functions that demonstrate how one and the same function can be used in three different contexts

(require mzlib/etc lang/prim lang/htdp-langs-save-file-prefix htdp/error )

(provide-higher-order-primitive convert-gui (f2c))
(provide-higher-order-primitive convert-repl (f2c))
(provide-higher-order-primitive convert-file (_ f2c _))

;; ---------------------------------------------------------------------------------------------------

(define IN-ERROR
  "The input must be a number. Given: ~e\n")

(define OUT-ERROR
  "The conversion function must produce a number; but it produced ~e")

(define CONVERT-FILE-MESSAGE
  "It appears as if you created the input file with DrRacket. Please use a different text editor")

;; ---------------------------------------------------------------------------------------------------

(define black-pen  (send the-pen-list find-or-create-pen "BLACK" 2 'solid))
(define red-brush  (send the-brush-list find-or-create-brush "RED" 'solid))
(define white-brush  (send the-brush-list find-or-create-brush "WHITE" 'solid))

;; scale% : (union false (num -> str)) frame% -> scale<%>
;; scale<%> : set-current-x + canvas<%>
(define scale%
  (class canvas%
    (inherit get-dc get-size get-client-size)    
    
    (define value  0)
    
    (define (draw-something)
      (let ([dc (get-dc)])
        (send dc clear)
        (let-values ([(width height) (get-client-size)])
          (send dc set-pen black-pen)
          (send dc set-brush white-brush)
          (send dc draw-rectangle 0 0 width height)
          (send dc set-brush red-brush)
          (send dc draw-rectangle 0 0
                (* width (max 0 (min 1 (/ (- value SLI-MIN) (- SLI-MAX SLI-MIN))))) height)
          (let*-values ([(cw ch) (get-client-size)]
                        [(number) value])
            (when (and (number? number)
                       (exact? number)
                       (real? number))
              (let* ([whole (if (number . < . 0)
                                (ceiling number)
                                (floor number))]
                     [fractional-part (- (abs number) (floor (abs number)))]
                     [num (numerator fractional-part)]
                     [den (denominator fractional-part)]
                     [wholes (if (and (zero? whole) (not (zero? number)))
                                 ""
                                 (number->string whole))]
                     [nums (number->string num)]
                     [dens (number->string den)])
                (let-values ([(ww wh wa wd) (send dc get-text-extent wholes)]
                             [(nw nh na nd) (send dc get-text-extent nums)]
                             [(dw dh da dd) (send dc get-text-extent dens)])
                  (let ([w (if (integer? number) (+ ww (max nw dw)) ww)]
                        [h (if (integer? number)
                               wh
                               (+ nh dh))])
                    (cond
                      [(integer? number) 
                       (send dc draw-text
                             wholes
                             (- (/ cw 2) (/ w 2))
                             (- (/ ch 2) (/ wh 2)))]
                      [else
                       (send dc draw-text
                             wholes
                             (- (/ cw 2) (/ w 2))
                             (- (/ ch 2) (/ wh 2)))
                       (send dc draw-text
                             nums
                             (+ ww (- (/ cw 2) (/ w 2)))
                             (- (/ ch 2) (/ h 2)))
                       (send dc draw-text
                             dens
                             (+ ww (- (/ cw 2) (/ w 2)))
                             (+ nh (- (/ ch 2) (/ h 2))))
                       (send dc draw-line
                             (+ ww (- (/ cw 2) (/ w 2)))
                             (/ ch 2)
                             (+ ww (max nw dw) (- (/ cw 2) (/ w 2)))
                             (/ ch 2))])))))))))
    (override on-paint)
    (define (on-paint) (draw-something))
    (public set-value)
    (define (set-value v) 
      (set! value v)
      (draw-something))
    (inherit min-width min-height)
    (super-instantiate ())
    (let-values ([(w h a d) (send (get-dc) get-text-extent "100100100")])
      (min-width (+ 4 (inexact->exact w)))
      (min-height (+ 4 (inexact->exact (* 2 h)))))))

;; ============================================================================
;; MODEL
;; 2int : num -> int
;; to convert a real number into an exact number 
(define (2int x)
  (if (and (real? x) (number? x))
      (inexact->exact x)
      (error 'convert OUT-ERROR x)))

;; slider-cb : (num -> num) object object -> [slider% event% -> void]
;; to use fahr->cel to perform the conversion 
(define ((slider-cb fahr->cel sliderC sliderF) c s)
  (send sliderC set-value
        ((compose in-slider-range 2int fahr->cel)
         (send sliderF get-value))))

;; in-slider-range : number -> number 
;; to check and to convert the new temperature into an appropriate scale 
(define (in-slider-range x)
  (cond
    [(<= SLI-MIN x SLI-MAX) x]
    [else (error 'convert-gui "result out of range for Celsius display")]))


#| --------------------------------------------------------------------
  
  view  (exports SLI-MIN SLI-MAX) (imports slider-cb)   
  parameter: sliderC sliderF 

  model (imports SLI-MIN SLI-MAX) (exports slider-cb)   
  
  ----------------------------------------------------------------------- |#

;; ============================================================================
;; VIEW

;; convert-gui : (num -> num) -> void
;; to install f as the temperature converter 
;; effect: to create a window with two rulers for converting F to C
(define (convert-gui f)
  (check-proc 'convert-gui f 1 "convert-gui" "one argument")
  ;; only initialize the slider based on the user's program when there aren't any exceptions.
  ;; if there are exceptions, wait for the user to click "convert" to see an error.
  (define-values (sliderC frame) (make-gui f))
  (with-handlers ([exn:fail? (lambda (x) (void))])
    (send sliderC set-value (in-slider-range (f F-SLI-0))))
  (send frame show #t))

;; f2c : num -> num
;; to convert a Fahrenheit temperature into a Celsius temperature 
(define (f2c f) (2int (* 5/9 (- f 32))))

(define F-SLI-MIN -50)
(define F-SLI-MAX 250)
(define F-SLI-0 32)    
(define SLI-MIN (f2c F-SLI-MIN))
(define SLI-MAX (f2c F-SLI-MAX))

(define (make-gui fahr->cel)
  (define frame 
    (parameterize ((current-eventspace (make-eventspace))) 
      (new frame% 
           [label "Fahrenheit to Celsius Conversion"]
           [alignment '(center center)])))
  
  (define main-panel
    (new horizontal-panel% [parent frame] [stretchable-height #f]))
  
  ;; create labels; aligned with sliders 
  (define mpanel (make-object vertical-panel% main-panel))
  (make-object message% "Fahrenheit" mpanel)
  (make-object message% "" mpanel)
  (make-object message% "Celsius" mpanel)
  (send mpanel stretchable-width #f)
  
  (define panel (make-object vertical-panel% main-panel))
  (send panel set-alignment 'center 'center)
  
  ;; sliderF : slider% 
  ;; to display the Fahrenheit temperature 
  (define sliderF (make-object slider% #f F-SLI-MIN F-SLI-MAX panel void F-SLI-0))
  (send sliderF min-width (- F-SLI-MAX F-SLI-MIN))
  
  ;; sliderC : slider% 
  ;; to display the Celsius temperature 
  (define sliderC (make-object scale% panel))
  (define _set-sliderC (send sliderC set-value (in-slider-range (f2c F-SLI-0))))
  
  (define button-panel
    (new vertical-panel% (stretchable-width #f) (stretchable-height #f) (parent main-panel)))
  
  ;; convert : button%
  ;; to convert fahrenheit to celsius 
  (make-object button% "Convert" button-panel (slider-cb fahr->cel sliderC sliderF))
  (make-object button% "Close" button-panel (lambda (x e) (send frame show #f)))
  
  (values sliderC frame))

;; ============================================================================
;; convert-repl : (num -> num) -> void
;; to start a read-eval-print loop that reads numbers [temp in F], applies f, and prints 
;; the result; effects: read and write; 
;; exit on x as input
(define (convert-repl f)
  (check-proc 'convert-repl f 1 "convert-repl" "one argument")
  (let repl ()
    (begin
      (printf "Enter Fahrenheit temperature and press <enter> [to exit, type x]: ")
      (flush-output)
      (let* ([ans (read)])
        (cond
          [(or (eof-object? ans) (eq? ans 'x)) (void)]
          [(not (number? ans))
           (printf "The input must be a number. Given: ~s\n" ans) (repl)]
          [(number? ans) 
           (let ([res (f ans)])
             (if (number? res)
                 (printf "~sF corresponds to ~sC\n" ans res) 
                 (error 'convert OUT-ERROR res))
             (repl))]
          [else (error 'convert "can't happen")])))))

;; ---------------------------------------------------------------------------------------------------

;; convert-file : str (num -> num) str -> void
;; to read a number from file in, to convert it with f, and to write it to out
(define (convert-file in f out)
  (check-arg 'convert-file (path-string? in) "string" "first" in)
  (check-arg 'convert-file (file-exists? in) 
             (format "name of existing file in ~a" (current-directory))
             "first" in)
  (check-proc 'convert-file f 1 "convert-file" "one argument")
  (check-arg 'convert-file (path-string? out) "string" "third" out)
  ;; [ -> Void] -> Void 
  ;; perform the actual conversion on the file after optionally reading a prelude 
  (define (convert-file prefix)
    (with-output-to-file out #:exists 'replace 
      (lambda () (with-input-from-file in (make-reader-for f prefix)))))
  (with-handlers ((exn:fail:read?
                   (lambda (x) 
                     (define message (exn-message x))
                     (define reader-exception? (regexp-match "#reader" message))
                     (cond
                       [reader-exception? 
                        (with-handlers ((exn:fail:read? (lambda (y) (raise x))))
                          (convert-file
                           (lambda ()
                             (unless (htdp-file-prefix? (current-input-port)) (raise x)))))]
                       [else (raise x)]))))
    (convert-file void)))

(define *debug (current-output-port))

;; make-reader-for-f : [Number -> Number] [ -> Void] -> [ -> void]
;; make-reader-for-f creates a function that reads numbers from a file
;; converts them according to f, and prints the results
;; effect: if any of the S-expressions in the file aren't numbers or
;;         if any of f's results aren't numbers,
;;         the function signals an error
(define (make-reader-for f prefix)
  (define (read-until-eof)
    (prefix)
    (let read-until-eof ()
      (define in (read))
      (cond
        [(eof-object? in) (void)]
        [(number? in)
         (define out (f in))
         (if (number? out) (printf "~s\n" out) (error 'convert OUT-ERROR out))
         (read-until-eof)]
        [else (error 'convert IN-ERROR in)])))
  read-until-eof)
