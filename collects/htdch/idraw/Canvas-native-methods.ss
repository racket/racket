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
               (lambda (e)
		 (raise-error
		   (format "The method start(int,int) must be called on the canvas before using any drawing methods")))))
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
  
  (define Posn-x-get (dynamic-require '(lib "Posn.ss" "htdch" "idraw") 'Posn-x-get))
  (define Posn-y-get (dynamic-require '(lib "Posn.ss" "htdch" "idraw") 'Posn-y-get))
  
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
         (start x y)
	 (void)))
  
  (define/provide (stop-native this accs gets privates) (stop) (void))
  
  (define/provide (drawCircle-idraw.Posn-int-idraw.Color-native this accs gets privates posn r c)
    (wrap-start-check
     (and (check-arg r "drawCircle(Posn, int, Color)" "second")
          (draw-circle (build-posn posn) r (color->symbol c))
	  (void))))
  
  (define/provide (drawDisk-idraw.Posn-int-idraw.Color-native this accs gets privates posn r c)
    (wrap-start-check
     (and (check-arg r "drawDisk(Posn, int, Color)" "second")
          (draw-solid-disk (build-posn posn) r (color->symbol c))
	  (void))))
  
  (define/provide (drawRect-idraw.Posn-int-int-idraw.Color-native this accs gets privates posn w h c)
    (wrap-start-check
     (and (check-arg w "drawRect(Posn, int, int, Color)" "second")
          (check-arg h "drawRect(Posn, int, int, Color)" "third")
          (draw-solid-rect (build-posn posn) w h (color->symbol c))
	  (void))))

  (define/provide (drawLine-idraw.Posn-idraw.Posn-idraw.Color-native this accs gets privates p0 p1 c)
    (wrap-start-check
      (and (draw-solid-line (build-posn p0) (build-posn p1) (color->symbol c))
	   (void))))
    
  (define/provide (drawString-idraw.Posn-java.lang.String-native this accs gets privates p s)
    (wrap-start-check
      (and 
	(draw-solid-string (build-posn p) (send s get-mzscheme-string))
	(void))))

  (define/provide (clearCircle-idraw.Posn-int-idraw.Color-native this accs gets privates p r c)
    (wrap-start-check
     (and (check-arg r "clearCircle(Posn, int, Color)" "second")
          (clear-circle (build-posn p) r (color->symbol c))
	  (void))))

  (define/provide (clearDisk-idraw.Posn-int-idraw.Color-native this accs gets privates p r c)
    (wrap-start-check
     (and (check-arg r "clearDisk(Posn, int, Color)" "second")
          (clear-solid-disk (build-posn p) r (color->symbol c))
	  (void))))

  (define/provide (clearRect-idraw.Posn-int-int-idraw.Color-native this accs gets privates p w h c)
    (wrap-start-check
     (and (check-arg w "clearRect(Posn, int, int, Color)" "second")
          (check-arg h "clearRect(Posn, int, int, Color)" "third")
          (clear-solid-rect (build-posn p) w h (color->symbol c))
	  (void))))

  (define/provide (clearLine-idraw.Posn-idraw.Posn-idraw.Color-native this accs gets privates p0 p1 c)
    (wrap-start-check
      (and 
	(clear-solid-line (build-posn p0) (build-posn p1) (color->symbol c))
	(void))))
)

