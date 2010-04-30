#lang scheme/gui

(require htdp/error mzlib/pconvert)

(provide checked-cell%)

(define checked-cell<%>
  (interface ()
    set ;; Symbol Any -> Boolean 
    ;; does the new state differ from the old? 
    ;; effect: if so only, set state
    
    get ;; -> Any (ok?)
    ))

(define checked-cell% 
  (class* object% (checked-cell<%>) 
    (init-field msg    ;; String 
                value0 ;; X
                ok?)   ;; Any -> Boolean : X 
    
    (init [display #f]) ;; (U String #f) ; a string is the name of the state display window
    
    (field 
     [value (coerce "initial value" value0)]
     ;; (U False pasteboard%)
     [pb (if (boolean? display)
             #f
             (let* ([f (new frame% [label display][width 400][height 400])]
                    [p (new pasteboard%)]
                    [e (new editor-canvas% [parent f] [editor p] 
                            [style '(hide-hscroll hide-vscroll)])])
               (send f show #t)
               p))])
    
    (define/private (show-state)
      (define xbox (box #f)) ;; x coordinate (throw away)
      (define ybox (box 0))  ;; y coordinate for next snip
      (define s
        (pretty-format
         (parameterize ([constructor-style-printing #t]
                        [booleans-as-true/false #t]
                        [abbreviate-cons-as-list 
                         #t
                         ;; is this beginner or beginner+quote
                         #;
                         (let ([o (open-output-string)])
                           (print '(1) o)
                           (regexp-match #rx"list" (get-output-string o)))])
           (print-convert value)) 
         40))
      ;; turn s into lines and display them in pb
      (send pb erase)
      (if (is-a? value snip%)
          (send pb insert value 0 0)
          (parameterize ([current-input-port (open-input-string s)])
            (let read-all ()
              (define nxt (read-line))
              (unless (eof-object? nxt)
                (let ([s (make-object string-snip% nxt)])
                  (send pb insert s 0 (unbox ybox))
                  (send pb get-snip-location s xbox ybox #t)
                  (read-all)))))))
    
    ;; Symbol Any -> ok?
    (define/private (coerce tag nw)
      (let ([b (ok? nw)])
        (check-result "check-with predicate" boolean? "Boolean" b)
        (check-result tag (lambda _ b) (format "~a (see check-with)" msg) nw)
        nw))
    
    ;; Symbol Any -> Void 
    ;; effect: set value to v if distinct, also display it if pb exists
    (define/public (set tag v) 
      (define nw  (coerce tag v))
      ;; this is the old Robby "optimization" for not triggering draw
      ;; when the world doesn't change 
      ;if (equal? value nw)
      ;   #t
      (begin
        (set! value nw)
        (when pb (show-state))
        #f))
    
    ;; -> ok?
    (define/public (get) value)
    
    (super-new)
    
    (when pb (show-state))))

; (define c (new checked-cell% [msg "World"] [value0 1] [ok? positive?]))
; (send c set "tick" 10)
