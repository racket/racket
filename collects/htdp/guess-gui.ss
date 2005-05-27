#cs(module guess-gui mzscheme
  (require (lib "error.ss" "htdp")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "prim.ss" "lang"))
  
  (provide
   connect ; (button% event% -> true) -> true 
   control ; N -> DIGIT 
   view    ; X -> true
   )
  
  (define-primitive control control/proc)
  (define-primitive view view/proc)
  (define-higher-order-primitive connect connect/proc (call-back))

  ;; ------------------------------------------------------------------------
  ;; Basic constants: 
  (define TITLE "Number Guessing")
  (define WIDTH 100)
  (define HIGHT 80)
  (define GUESS 3)
  
  ;; DIGIT = (union 0 1 2 3 4 5 6 7 8 9)
  (define DIGITS (build-list 10 (lambda (i) (number->string i))))
  
  ;; ------------------------------------------------------------------------
  ;; GUI LAYOUT 
  (define frame (make-object frame% TITLE #f WIDTH HIGHT))
  (define verti (make-object vertical-panel% frame))
  (define panel (make-object horizontal-panel% verti))
  (send panel set-alignment 'center 'center)
  
  ;; ------------------------------------------------------------------------
  ;; guess : handle CONTROL
  (define guess-panel (make-object horizontal-panel% verti '(border)))
  (define (make-choices n)
    (cons (make-object choice% #f DIGITS guess-panel void)
          (cond
            [(= n 0) empty]
            [else (make-choices (sub1 n))])))
  
  (define guess-choices (make-choices (sub1 GUESS)))
  
  ;; control : N -> DIGIT
  ;; to read out the i-th guess choice, starting with 0
  (define (control/proc i)
    (check-arg 'control
               (and (number? i) (integer? i) (exact? i) (<= 0 i (sub1 GUESS))) 
               (format "number in 0 ...~s" GUESS) "first" i)
    '(if (and (number? i) (integer? i) (exact? i) (<= 0 i (sub1 GUESS)))
         ...  
         (printf "control: improper index, expected 0 ... ~s~n" GUESS))
    (send (list-ref guess-choices (- GUESS i 1)) get-selection))
  
  ;; connect : (button% control-event% -> true) -> true
  ;; effect: to add a check button with call-back to frame and to show frame 
  (define check-button #f)
  (define (connect/proc call-back)
    (check-proc 'connect call-back 2 '1st "2 arguments")
    (if check-button
        (printf "connect: called a second time~n")
        (begin
          (set! check-button 
                (make-object button% "Check" guess-panel call-back '(border)))
          (send frame show #t))))
  
  ;; ------------------------------------------------------------------------
  ;; message : display VIEW 
  (define VIEW-fmt "View the result: ~a")
  (define VIEW0 (format VIEW-fmt (make-string (* 2 GUESS) #\space)))
  (define message-field (make-object message%  VIEW0 verti))
  
  ;; view : X -> void
  ;; effect: to display n in the message panel 
  (define (view/proc n) (send message-field set-label (format VIEW-fmt n)))
  )
