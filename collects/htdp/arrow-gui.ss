#cs(module arrow-gui mzscheme
  
  (require (lib "error.ss" "htdp")
           (lib "big-draw.ss" "htdp")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "prim.ss" "lang"))
  
  (provide
   control ; modelT modelT modelT modelT -> true 
   view    ; X -> true
   connect ; -> Symbol
   )
  
  (define-higher-order-primitive connect connect/proc (left right up down))
  (define-primitive control control/proc)
  (define-primitive view view/proc)
  
  ;; CONSTANTS ---------------------------------------------------------------
  (define MY-ICONS "/home/matthias/icons/")
  (define TITLE "Controller")
  (define COLLECT (collection-path "icons"))
  (define ARR "arrow.blue.~a.gif")
  
  ;; LAYOUT CONSTRUCTION ----------------------------------------------------
  
  
  ;; mk-image-constant : str (button% event% -> true) -> (panel% -> button%)
  ;; to create a panel-parameterized button with a picture and a specific call-back 
  (define (mk-image-constant kind model)
    (local ([define an-item
              (make-object bitmap% (build-path COLLECT (format ARR kind)) 'gif)])
      (lambda (panel)
        (make-object button% an-item panel model))))
  
  ;; make-button-table : 
  ;;   panel% layout -> (listof (listof (union panel% button%)))
  ;; to translate a layout table into a button table 
  ;;   each button is controled by (control a-bitmap)
  (define (make-button-table panel layout)
    (local ((define (make-row a-row)
              (local ((define row-panel (make-object horizontal-panel% panel))
                      (define (make-item an-item)
                        (if an-item (an-item row-panel)
			    (let ([panel (make-object horizontal-panel% row-panel)])
			      (send panel min-width 30)))))
                (map make-item a-row))))
      (map make-row layout)))
  
  (define frame (make-object frame% TITLE #f 10 10))
  (define panel (make-object vertical-panel% frame))
  (define hor (make-object horizontal-panel% panel '(border)))
  (define lab (make-object message% "Going where?" hor))
  (define msg (make-object message% "Nowhere" hor))
  
  ;; X -> true
  ;; to display s in the msg panel 
  (define (view/proc s)
    (send msg set-label (format "~a" s))
    true)
  
  ;; WIRING THINGS UP    ----------------------------------------------------
  ;; -> symbol
  ;; to read out the current state of the msg field 
  (define (control/proc)
    (string->symbol (send msg get-label)))
  
  ;; modelT = (button% event% -> true)
  ;; connect/proc : modelT modelT modelT modelT -> true
  (define (connect/proc left right up down)
    (check-proc 'connect left 2 "'left' argument" "two arguments")
    (check-proc 'connect right 2 "'right' argument" "two arguments")
    (check-proc 'connect up 2 "'up' argument" "two arguments")
    (check-proc 'connect down 2 "'down' argument" "two arguments")  
    (local ((define LEFT-ARROW  (mk-image-constant "left"  left))
            (define RIGHT-ARROW (mk-image-constant "right" right))
            (define UP-ARROW    (mk-image-constant "up"    up))
            (define DOWN-ARROW  (mk-image-constant "down"  down))
            (define FOUR 
              `( (,#f         ,UP-ARROW   ,#f)
                 (,LEFT-ARROW ,#f         ,RIGHT-ARROW)
                 (,#f         ,DOWN-ARROW ,#f) ))
            (define layout (make-button-table frame FOUR)))
      (send frame show true)
      true)))
