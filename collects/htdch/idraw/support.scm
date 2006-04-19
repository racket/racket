(module support mzscheme 
  (require (lib "draw.ss" "htdp")
           (lib "posn.ss" "lang")
           (lib "class.ss")
           (lib "unit.ss") (lib "unitsig.ss")
	   (lib "String.ss" "profj" "libs" "java" "lang"))
  #;(lib "Throwable.ss" "profj" "libs""java""lang")
  #;(lib "RuntimeException.ss" "profj" "libs" "java" "lang")
  
  (provide world-native@ world-native^ support^)
  
  (define-signature world-native^ (endOfTime-native endOfWorld-native bigBangO-double-native))
  (define-signature support^ (world-return void-or-true))
           
  (define world-native@
    (unit/sig world-native^
      (import support^)
      (define (bigBangO-double-native this accs gets privates i)
        (define theCanvas ((hash-table-get accs 'theCanvas) this))
        (define width (with-method ([g (theCanvas Canvas-width-get)]) (g '___)))
        (define height (with-method ([g (theCanvas Canvas-height-get)]) (g '___)))
        ;; call only *after* start 
        (define (on-event world th)
          (begin-draw-sequence)
          (send theCanvas copy)
          (send world erase)
          (th)                
          (send world draw)   
          (end-draw-sequence)
          world)
        (send theCanvas show)
        (big-bang i this)
        (on-tick-event 
         (lambda (world)
           (on-event world (lambda () (send world onTick)))))
        (on-key-event
         (lambda (ke world)
           (define ke* (make-java-string (if (char? ke) (string ke) (symbol->string ke))))
           (on-event world (lambda () (send world onKeyEvent-java.lang.String ke*)))))
        (void-or-true))
      
      (define (endOfTime-native this accs gets privates)
        (define theCanvas ((hash-table-get accs 'theCanvas) this))
        (send theCanvas stop)
        (world-return this))
      
      (define (endOfWorld-native this accs gets privates)
        (define theCanvas ((hash-table-get accs 'theCanvas) this))
        (send theCanvas stop)
        (world-return this))))
  )