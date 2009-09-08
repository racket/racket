#lang scheme/gui

(require htdp/draw
         lang/posn
         mzlib/class
         mzlib/unit
         profj/libs/java/lang/String
         profj/libs/java/lang/Throwable
         profj/libs/java/lang/RuntimeException)

(provide world-native@ world-native^ canvas-native@ canvas-native^ support^)

(define-signature world-native^
  (endOfTime-java.lang.String-native
   endOfWorld-java.lang.String-native
   bigBangO-double-native))

(define-signature canvas-native^
  (xshow-native
   xclose-native
   stop-native
   copy-native
   drawCircle-geometry.Posn-int-colors.IColor-native
   drawDisk-geometry.Posn-int-colors.IColor-native
   drawRect-geometry.Posn-int-int-colors.IColor-native
   drawLine-geometry.Posn-geometry.Posn-colors.IColor-native
   drawString-geometry.Posn-java.lang.String-native
   ))

(define-signature support^ (void-or-true imperative))

;Raises an error if string is null
;check-string: string string string -> boolean
(define (check-string value method argument)
  (when (null? value)
    (raise-error (format "Method ~a expects a non-null String for ~a argument, given null" method argument)))
  #t)

;raises a Java exception with the specified error message
;raise-error: String -> void
(define (raise-error message)
  (raise
   (create-java-exception RuntimeException message 
                          (lambda (exn str)
                            (send exn RuntimeException-constructor-java.lang.String str))
                          (current-continuation-marks))))

(define-unit canvas-native@
  (import support^)
  (export canvas-native^)
  
  (define-syntax (wrap-start-check stx)
    (syntax-case stx ()
      [(_ body ...)
       #'(with-handlers 
             ((exn:fail? 
               (lambda (e)
                 (raise-error
                  (format
                   (string-append
                    "The method show() must be called on the canvas"
                    "before using any drawing methods [~s]")
                   (exn-message e))))))
           (begin (begin body ...) void-or-true))]))
  
  (define Posn-x-get (dynamic-require 'htdch/geometry/Posn 'Posn-x-get))
  (define Posn-y-get (dynamic-require 'htdch/geometry/Posn 'Posn-y-get))
  
  (define (build-posn posnO) (make-posn (Posn-x-get posnO) (Posn-y-get posnO)))
  (define (color->symbol colorO) (string->symbol (to-lower-case (send colorO my-name))))
  
  ;Raises an error if value less than or equal to 0
  ;check-arg: num string string -> boolean
  (define (check-arg value method argument)
    (or (> value 0)
        (raise-error
         (format "Method ~a expects an int > 0 for ~a argument, given ~a" method argument value))))
  
  (define (to-lower-case s)
    (letrec ((lower 
              (lambda (s)
                (cond
                  ((null? s) s)
                  (else (cons (char-downcase (car s))
                              (lower (cdr s))))))))
      (list->string (lower (string->list s)))))
  
  (define (xshow-native this accs gets privates)
    ;; Kathy: it looks like I am working around a bug here. 
    ;; I really wanted to write ([hash-table-get privates 'width] this)
    ;; but that didn't work at all. 'width is not a key for privates, 
    ;; even though it is a private field. Then I wanted to write 
    ;; ([hash-table-get privates 'width] this), just like in World-native-methods. 
    ;; That failed, too, with an arity error. 
    (define x (with-method ([g (this Canvas-width-get)]) (g '___)))
    (define y (with-method ([g (this Canvas-height-get)]) (g '___)))
    (start-and-export x y privates)
    void-or-true)
  
  (define (xclose-native this accs gets privates)
    (wrap-start-check ([hash-ref privates '%stop]))
    void-or-true)
  
  (define (stop-native this accs gets privates)
    (wrap-start-check ([hash-ref privates '%end-of-time])))
  
  ;; (copy) restores the viewport and the pixmap so that
  ;; (end-draw-sequence) can copy the pixmap into the viewport.
  ;; It also clears the pixmap from anything that was on there.
  ;; design rationale: the closure is created during the initializtion
  ;; of the world (big-bang) and thus encapsulates access to the actual
  ;; values of pixmap and viewport. big-bang exists once and for
  ;; all and thus can't encapsulate the values.
  ;; Alternative: expose these values as a "token", which big-bang must
  ;; install. I couldn't figure out how to do this at the time. 
  (define (copy-native this accs gets privates)
    (wrap-start-check ([hash-ref privates 'copy])))
  
  (define (drawCircle-geometry.Posn-int-colors.IColor-native this accs gets privates posn r c)
    (wrap-start-check
     (check-arg r "drawCircle(Posn, int, IColor)" "second")
     ([hash-ref privates '%draw-circle] (build-posn posn) r (color->symbol c))))
  
  (define (drawDisk-geometry.Posn-int-colors.IColor-native this accs gets privates posn r c)
    (wrap-start-check
     (check-arg r "drawDisk(Posn, int, IColor)" "second")
     ([hash-ref privates '%draw-solid-disk] (build-posn posn) r (color->symbol c))))
  
  (define (drawRect-geometry.Posn-int-int-colors.IColor-native this accs gets privates posn w h c)
    (wrap-start-check 
     (check-arg w "drawRect(Posn, int, int, IColor)" "second")
     (check-arg h "drawRect(Posn, int, int, IColor)" "third")
     ([hash-ref privates '%draw-solid-rect] (build-posn posn) w h (color->symbol c))))
  
  (define (drawLine-geometry.Posn-geometry.Posn-colors.IColor-native this accs gets privates p0 p1 c)
    (wrap-start-check 
     ([hash-ref privates '%draw-solid-line] (build-posn p0) (build-posn p1) (color->symbol c))))
  
  (define (drawString-geometry.Posn-java.lang.String-native this accs gets privates p s)
    (define _ (check-string s "drawString(Posn, String)" "second"))
    (define s* (send s get-mzscheme-string))
    (wrap-start-check
     ([hash-ref privates '%draw-string] (build-posn p) s*)))
  
  #;
  (define (clearCircle-geometry.Posn-int-colors.IColor-native this accs gets privates p r c)
    (wrap-start-check 
     (check-arg r "clearCircle(Posn, int, IColor)" "second")
     ([hash-table-get privates '%clear-circle] (build-posn p) r (color->symbol c))))
  
  #;
  (define (clearDisk-geometry.Posn-int-colors.IColor-native this accs gets privates p r c)
    (wrap-start-check 
     (check-arg r "clearDisk(Posn, int, IColor)" "second")
     ([hash-table-get privates '%clear-solid-disk] (build-posn p) r (color->symbol c))))
  
  #;
  (define (clearRect-geometry.Posn-int-int-colors.IColor-native this accs gets privates p w h c)
    (wrap-start-check 
     (check-arg w "clearRect(Posn, int, int, IColor)" "second")
     (check-arg h "clearRect(Posn, int, int, IColor)" "third")
     ([hash-table-get privates '%clear-solid-rect] (build-posn p) w h (color->symbol c))))
  
  #;
  (define (clearLine-geometry.Posn-geometry.Posn-colors.IColor-native this accs gets privates p0 p1 c)
    (wrap-start-check 
     ([hash-table-get privates '%clear-solid-line] (build-posn p0) (build-posn p1) (color->symbol c))))
  )


(define-unit world-native@
  (import support^)
  (export world-native^)
  
  (define (bigBangO-double-native this accs gets privates i)
    (define theCanvas ((hash-ref accs 'theCanvas) this))
    (define setCanvas (hash-ref gets 'theCanvas))
    (define width (with-method ([g (theCanvas Canvas-width-get)]) (g '___)))
    (define height (with-method ([g (theCanvas Canvas-height-get)]) (g '___)))
    ;; call only *after* start 
    (define (on-event world0 th)
      (begin-draw-sequence)
      (send theCanvas copy)
      (let ([world (imperative (th) world0)])
        (unless (eq? world0 world)
          (setCanvas world theCanvas))
        (send world draw)   
        (end-draw-sequence)
        world))
    (send theCanvas show)
    (big-bang i this)
    (on-tick-event 
     (lambda (world)
       (on-event world (lambda () (send world onTick)))))
    (on-key-event
     (lambda (ke world)
       (define ke* (make-java-string (if (char? ke) (string ke) (symbol->string ke))))
       (on-event world (lambda () (send world onKeyEvent-java.lang.String ke*)))))
    void-or-true)
  
  (define (endOfTime-java.lang.String-native this accs gets privates s)
    (define _ (check-string s "endOfTime(String)" "first"))	
    (define t (send s get-mzscheme-string))
    (define theCanvas ((hash-ref accs 'theCanvas) this))
    (parameterize ([current-eventspace (make-eventspace)])
      (message-box "end of world" (send s get-mzscheme-string)))
    (unless (null? theCanvas)
      (send theCanvas stop))
    #t)
  
  (define (endOfWorld-java.lang.String-native this accs gets privates s)
    (define theCanvas ((hash-ref accs 'theCanvas) this))
    (define _ (check-string s "endOfWorld(String)" "first"))
    (parameterize ([current-eventspace (make-eventspace)])
      (message-box "end of world" (send s get-mzscheme-string)))
    (unless (null? theCanvas)
      (send theCanvas stop))
    this))

