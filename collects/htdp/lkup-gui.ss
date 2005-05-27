#cs(module lkup-gui mzscheme
  (require (lib "error.ss" "htdp")
	   (lib "class.ss")
           (lib "prim.ss" "lang")
	   (lib "mred.ss" "mred"))
  
  (provide control view connect)
  
  (define-primitive control control/proc)
  (define-primitive view view/proc)
  (define-higher-order-primitive connect connect/proc (call-back))
  
  ;; ------------------------------------------------------------------------
  ;; Basic constants: 
  (define TITLE "LOOKUP")
  (define WIDTH 100)
  (define HIGHT 50)
  
  ;; ------------------------------------------------------------------------
  ;; GUI LAYOUT 
  (define frame (make-object frame% TITLE #f WIDTH HIGHT))
  (define panel (make-object horizontal-panel% frame))
  (send panel set-alignment 'left 'top)
  (define vert1 (make-object vertical-panel% panel)) 
  (send vert1 set-alignment 'left 'top)
  (make-object message% "Name:" vert1)
  (make-object message% "Number:" vert1)
  (define vert2 (make-object vertical-panel% panel))
  
  ;; ------------------------------------------------------------------------
  ;; guess : handle CONTROL
  (define query-tf (make-object text-field% "" vert2
                     (lambda (x y) (send result set-label ""))))
  
  ;; control : -> symbol
  ;; to supply the name that a user typed into the query text-field
  (define (control/proc)
    (string->symbol (send query-tf get-value)))
  
  ;; connect : (button% control-event% -> true) -> void
  ;; effect: to add a check button with call-back to frame and to show frame 
  ;; the button is "border" style, so <CR> in query-tf will use call-back
  (define button #f)
  (define (connect/proc call-back)
    (check-proc 'connect call-back 2 '1st "2 arguments")
    (if button
        (printf "connect: called a second time~n")
        (begin 
          (set! button (make-object button% "LookUp" panel call-back '(border)))
          (send query-tf focus)
          (send frame show #t))))
  
  ;; ------------------------------------------------------------------------
  ;; message : display VIEW 
  (define result (make-object message% "ddd.ddd.dddd" vert2))
  
  ;; view : symbol -> void
  ;; effect: to display the phone number n in the message panel 
  (define (view/proc n)
    (check-arg 'view (symbol? n) "symbol" "first" n)
    (send result set-label (symbol->string n)))
  )
