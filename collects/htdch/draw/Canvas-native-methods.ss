#cs
(module Canvas-native-methods mzscheme
  (require (lib "draw.ss" "htdp")
           (lib "posn.ss" "lang")
           (lib "class.ss")
	   (lib "String.ss" "profj" "libs" "java" "lang")
           (lib "Throwable.ss" "profj" "libs" "java" "lang")
           (lib "RuntimeException.ss" "profj" "libs" "java" "lang"))
  ;(require "Posn.ss")
  
  ;raises a Java exception with the specified error message
  ;raise-error: String -> void
  (define (raise-error message)
    (raise
     (create-java-exception RuntimeException message 
                            (lambda (exn str)
                              (send exn RuntimeException-constructor-java.lang.String str))
                            (current-continuation-marks))))                                    
  
  (define-syntax (wrap-start-check stx)
    (syntax-case stx ()
      [(_ body)
       #'(with-handlers 
             ((exn:fail? 
               (lambda (e) (raise-error "The method start(int,int) must be called on the canvas before using any drawing methods"))))
           body)]))
  
  (define-syntax (define/provide stx)
    (syntax-case stx ()
      [(_ id val)
       (identifier? #'id)
       #'(begin
	   (define id val)
	   (provide id))]
      [(_ (id . formals) . rest)
       #'(begin
	   (define (id . formals) . rest)
	   (provide id))]))
  
  (define Posn-x-get (dynamic-require '(lib "Posn.ss" "htdch" "draw") 'Posn-x-get))
  (define Posn-y-get (dynamic-require '(lib "Posn.ss" "htdch" "draw") 'Posn-y-get))
  
  (define (build-posn posnO)
    (make-posn (Posn-x-get posnO) (Posn-y-get posnO)))
  (define (color->symbol colorO)
    (string->symbol (to-lower-case (send colorO my-name))))

  ;Raises an error if value less than or equal to 0
  ;check-arg: num string string -> boolean
  (define (check-arg value method argument)
    (or (> value 0)
        (raise-error (format "Method ~a expects an int greater than 0 for ~a argument, given ~a"
                             method argument value))))
  
  (define (to-lower-case s)
    (letrec ((lower 
              (lambda (s)
                (cond
                  ((null? s) s)
                  (else (cons (char-downcase (car s))
                              (lower (cdr s))))))))
      (list->string (lower (string->list s)))))
  
  (define/provide (start-int-int-native this accs gets privates x y)
    (and (check-arg x "start(int,int)" "first")
         (check-arg x "start(int,int)" "second")
         (start x y)))
  
  (define/provide (stop-native this accs gets privates) (stop))
  
  (define/provide (drawCircle-draw.Posn-int-draw.Color-native this accs gets privates posn r c)
    (wrap-start-check
     (and (check-arg r "drawCircle(Posn, int, Color)" "second")
          (draw-circle (build-posn posn) r (color->symbol c)))))
  
  (define/provide (drawDisk-draw.Posn-int-draw.Color-native this accs gets privates posn r c)
    (wrap-start-check
     (and (check-arg r "drawDisk(Posn, int, Color)" "second")
          (draw-solid-disk (build-posn posn) r (color->symbol c)))))
  
  (define/provide (drawRect-draw.Posn-int-int-draw.Color-native this accs gets privates posn w h c)
    (wrap-start-check
     (and (check-arg w "drawRect(Posn, int, int, Color)" "second")
          (check-arg h "drawRect(Posn, int, int, Color)" "third")
          (draw-solid-rect (build-posn posn) w h (color->symbol c)))))

  (define/provide (drawLine-draw.Posn-draw.Posn-draw.Color-native this accs gets privates p0 p1 c)
    (wrap-start-check
     (draw-solid-line (build-posn p0) (build-posn p1) (color->symbol c))))
    
  (define/provide (drawString-draw.Posn-java.lang.String-native this accs gets privates p s)
    (wrap-start-check
     (draw-solid-string (build-posn p) (send s get-mzscheme-string))))

  (define/provide (clearCircle-draw.Posn-int-draw.Color-native this accs gets privates p r c)
    (wrap-start-check
     (and (check-arg r "clearCircle(Posn, int, Color)" "second")
          (clear-circle (build-posn p) r (color->symbol c)))))

  (define/provide (clearDisk-draw.Posn-int-draw.Color-native this accs gets privates p r c)
    (wrap-start-check
     (and (check-arg r "clearDisk(Posn, int, Color)" "second")
          (clear-solid-disk (build-posn p) r (color->symbol c)))))

  (define/provide (clearRect-draw.Posn-int-int-draw.Color-native this accs gets privates p w h c)
    (wrap-start-check
     (and (check-arg w "clearRect(Posn, int, int, Color)" "second")
          (check-arg h "clearRect(Posn, int, int, Color)" "third")
          (clear-solid-rect (build-posn p) w h (color->symbol c)))))

  (define/provide (clearLine-draw.Posn-draw.Posn-draw.Color-native this accs gets privates p0 p1 c)
    (wrap-start-check
     (clear-solid-line (build-posn p0) (build-posn p1) (color->symbol c))))

#|
  (define/provide (sleepForAWhile-int-native this accs gets privates s)
    (sleep-for-a-while s))
  
  (define/provide (bigBang-double-native this accs gets privates i)
    (big-bang i this)
    (on-tick-event
      (lambda (world)
	(set! last-world world)
	(let ([next-world (send world onTick)])
	  (send last-world erase)
	  (send next-world draw)
	  next-world)))
    (on-key-event
      (lambda (ke world)
	(set! last-world world)
	(let ([next-world (send world onKeyEvent-java.lang.String
			   (make-java-string (keyevent->string ke)))])
	  (send last-world erase)
	  (send next-world draw)
	  next-world)))
    #t)

  ;; (union Char Symbol) -> String
  (define (keyevent->string ke)
    (if (char? ke) (string ke) (symbol->string ke)))
  
  (define/provide (draw-native this accs gets privates)
    #t)

  (define/provide (erase-native this accs gets privates)
    #t)

  (define/provide (onTick-native this accs gets privates)
     this)

  (define/provide (onKeyEvent-java.lang.String-native this accs gets privates ke)
     this)

  (define last-world #f)
  
  (define/provide (endOfTime-native this accs gets privates)
    (set! last-world (end-of-time))
    #t)

  (define/provide (endOfWorld-native this accs gets privates)
    (set! last-world (end-of-time))
    last-world)

  (define/provide (lastWorld-native this accs gets privates)
    (if last-world last-world this))
|#
)

