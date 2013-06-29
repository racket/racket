
(module stepper-test mzscheme
  (require mred
           mzlib/class
           "private/drracket-test-util.rkt"
           "private/gui.rkt"
           framework
           mzlib/pretty)
  
  (provide run-test)
  
  (define next-button-label "Step >")
  (define no-more-steps-message "All of the definitions have been successfully evaluated.")
  
  ;; type contents = (listof (union snip string contents))
  ;; type error = (make-error string)
  ;; type step = (make-step contents contents (union error contents))
  (define-struct step (definitions before after) (make-inspector))
  (define-struct err (message) (make-inspector))

  ;; type program-spec = (union string (make-file string))
  (define-struct file (name))
  
  (define current-program-id (make-parameter 'current-program-id-unset))
  (define failure-escape (make-parameter 'failure-escape-unset))
  
  (define sample-solutions-directory "/home/robby/unison/collects/solutions")
  
  (define to-skip-to "average-price.scm")
  
  (define (run-test)
    (run-fully-specified-tests)
    
    #|
    (set-language-level! (list "Beginning Student with List Abbreviations"))
    (run-string-test "(define (f x) (* x 2))\n(+ 1 (f (+ 1 1)))")
    (run-string-test "(sqrt 2)")
    (run-string-test "(car)")
    
    (run-sample-solution-tests)
    |#
    )
 
  (define (run-fully-specified-tests)
    (set-language-level! (list "Beginning Student"))
    (beginner-tests/no-list)
    (test-transcript '(cons 1 (cons 2 (list 3 4 5)))
                     '(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
    
    (set-language-level! (list "Beginning Student with List Abbreviations"))
    (beginner-tests/no-list)
    (test-transcript '(cons 1 (cons 2 (list 3 4 5)))
                     '(cons 1 (list 2 3 4 5))
                     '(list 1 2 3 4 5)))
  
  (define (beginner-tests/no-list)
    (test-transcript '(+ 1 2) 3)
    (test-transcript
     '(cond [(= 1 1) (cond [(= 1 2) 3] [else 4])])
     '(cond [true (cond [(= 1 2) 3] [else 4])])
     '(cond [(= 1 2) 3] [else 4])
     '(cond [false 3] [else 4])
     '(cond [else 4])
     '4)
    (test-transcript
     '(if (= 1 1) (if (= 1 2) 4 (if (= 1 1) 5 6)) 7)
     '(if true (if (= 1 2) 4 (if (= 1 1) 5 6)) 7)
     '(if (= 1 2) 4 (if (= 1 1) 5 6))
     '(if false 4 (if (= 1 1) 5 6))
     '(if (= 1 1) 5 6)
     '(if true 5 6)
     '5)
    (test-transcript
     '(and (or true false)
           (or false false true)
           (and true true)
           false)
     '(and true
           (or false false true)
           (and true true)
           false)
     '(and true
           true
           (and true true)
           false)
     '(and true
           true
           true
           false)
     'false)

    (fully-specified-test
     "(+ #i1.2 1)"
     (make-step '() '("(+ #i1.2 1)") '("#i2.2"))
     (make-step '("#i2.2") '() '()))
    
    (test-transcript/defns
     (list '(define (f x) (+ x 1)))
     '(f 1)
     '(+ 1 1)
     '2)

    (test-transcript
     '(+ 1 (posn-x (make-posn (+ 1 2) (+ 3 4))))
     '(+ 1 (posn-x (make-posn 3 (+ 3 4))))
     '(+ 1 (posn-x (make-posn 3 7)))
     '(+ 1 3)
     '4)
    
    (test-transcript/defns
     (list '(define-struct s (a b)))
     '(if (s? (make-s 'a 'b))
          (s-a (make-s 'c 'd))
          'ack)
     '(if true
          (s-a (make-s 'c 'd))
          'ack)
     '(s-a (make-s 'c 'd))
     ''c)

    (test-transcript/defns
     (list '(define x 1))
     '(+ x x)
     '(+ 1 x)
     '(+ 1 1)
     '2)
    
    (test-transcript/defns
     (list '(define-struct con (hd tl))
           '(define-struct mt ())
           '(define (sum l)
              (cond
                [(mt? l) 0]
                [(con? l) (+ (con-hd l) (sum (con-tl l)))])))
     '(sum (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
     '(cond
        [(mt? (make-con 5 (make-con 6 (make-con 7 (make-mt))))) 0]
        [(con? (make-con 5 (make-con 6 (make-con 7 (make-mt))))) 
         (+ (con-hd (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
            (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))])
     '(cond
        [false 0]
        [(con? (make-con 5 (make-con 6 (make-con 7 (make-mt))))) 
         (+ (con-hd (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
            (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))])
     '(cond
        [(con? (make-con 5 (make-con 6 (make-con 7 (make-mt))))) 
         (+ (con-hd (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
            (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))])
     '(cond
        [true
         (+ (con-hd (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
            (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))])
     '(+ (con-hd (make-con 5 (make-con 6 (make-con 7 (make-mt)))))
         (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))
     '(+ 5
         (sum (con-tl (make-con 5 (make-con 6 (make-con 7 (make-mt)))))))
     '(+ 5
         (sum (make-con 6 (make-con 7 (make-mt)))))
     '(+ 5
         (cond
           [(mt? (make-con 6 (make-con 7 (make-mt)))) 0]
           [(con? (make-con 6 (make-con 7 (make-mt)))) 
            (+ (con-hd (make-con 6 (make-con 7 (make-mt))))
               (sum (con-tl (make-con 6 (make-con 7 (make-mt))))))]))
     '(+ 5
         (cond
           [false 0]
           [(con? (make-con 6 (make-con 7 (make-mt)))) 
            (+ (con-hd (make-con 6 (make-con 7 (make-mt))))
               (sum (con-tl (make-con 6 (make-con 7 (make-mt))))))]))
     '(+ 5
         (cond
           [(con? (make-con 6 (make-con 7 (make-mt)))) 
            (+ (con-hd (make-con 6 (make-con 7 (make-mt))))
               (sum (con-tl (make-con 6 (make-con 7 (make-mt))))))]))
     '(+ 5
         (cond
           [true
            (+ (con-hd (make-con 6 (make-con 7 (make-mt))))
               (sum (con-tl (make-con 6 (make-con 7 (make-mt))))))]))
     '(+ 5
         (+ (con-hd (make-con 6 (make-con 7 (make-mt))))
            (sum (con-tl (make-con 6 (make-con 7 (make-mt)))))))
     '(+ 5
         (+ 6
            (sum (con-tl (make-con 6 (make-con 7 (make-mt)))))))
     '(+ 5
         (+ 6
            (sum (make-con 7 (make-mt)))))
     '(+ 5
         (+ 6
            (cond
              [(mt? (make-con 7 (make-mt))) 0]
              [(con? (make-con 7 (make-mt))) 
               (+ (con-hd (make-con 7 (make-mt))) 
                  (sum (con-tl (make-con 7 (make-mt)))))])))
     '(+ 5
         (+ 6
            (cond
              [false 0]
              [(con? (make-con 7 (make-mt))) 
               (+ (con-hd (make-con 7 (make-mt))) 
                  (sum (con-tl (make-con 7 (make-mt)))))])))
     '(+ 5
         (+ 6
            (cond
              [(con? (make-con 7 (make-mt))) 
               (+ (con-hd (make-con 7 (make-mt))) 
                  (sum (con-tl (make-con 7 (make-mt)))))])))
     '(+ 5
         (+ 6
            (cond
              [true
               (+ (con-hd (make-con 7 (make-mt))) 
                  (sum (con-tl (make-con 7 (make-mt)))))])))
     '(+ 5
         (+ 6
            (+ (con-hd (make-con 7 (make-mt))) 
               (sum (con-tl (make-con 7 (make-mt)))))))
     '(+ 5
         (+ 6
            (+ 7
               (sum (con-tl (make-con 7 (make-mt)))))))
     '(+ 5
         (+ 6
            (+ 7
               (sum (make-mt)))))
     '(+ 5
         (+ 6
            (+ 7
               (cond
                [(mt? (make-mt)) 0]
                [(con? (make-mt)) (+ (con-hd (make-mt)) (sum (con-tl (make-mt))))]))))
     '(+ 5
         (+ 6
            (+ 7
               (cond
                [true 0]
                [(con? (make-mt)) (+ (con-hd (make-mt)) (sum (con-tl (make-mt))))]))))
     '(+ 5 (+ 6 (+ 7 0)))
     '(+ 5 (+ 6 7))
     '(+ 5 13)
     '18))
     
  (define (fully-specified-test init . steps)
    (let ([actual (step-and-extract-program init)])
      (unless (equal? actual steps)
        (printf "FAILED: ~s\nexpected: ~s\n     got: ~s\n" init steps actual))))

  (define (test-transcript/defns defns init . sexp-steps)
    (let* ([str-defns (apply string-append (map (lambda (x) (string-append (to-string x) "\n")) defns))]
           [actual-steps (step-and-extract-program (format "~a~a" str-defns (to-string init)))]
           [failed
            (lambda (msg . args)
              (printf "FAILED: ~a\ndefns: ~s\nexpected: ~s\nactual: ~s\n\n" 
                      (apply format msg args)
                      defns
                      (cons init sexp-steps)
                      actual-steps))])
      (let loop ([steps actual-steps]
                 [last init]
                 [sexps sexp-steps]
                 [n 0])
        (cond
          [(and (null? sexps) 
                (not (null? steps))
                (null? (cdr steps)))
           (let ([step (car steps)])
             (unless (ws-equal? (stringify (step-definitions step))
                                (string-append
                                 str-defns
                                 "\n"
                                 (to-string last)))
               (failed "mismatch at last step ~s and ~s"
                       (step-definitions step)
                       last))
             (unless (and (equal? '() (step-before step))
                          (equal? '() (step-after step)))
               (failed "expected empty before and after steps in the last step")))]
          [(or (null? steps) (null? sexps))
           (failed "different length")]
          [else
           (let ([step (car steps)]
                 [sexp (car sexps)])
             (cond
               [(not (ws-equal? (stringify (step-definitions step)) str-defns))
                (failed "mismatch at ~a defnitions ~s and ~s" 
                        n
                        (stringify (step-definitions step))
                        str-defns)]
               [(not (ws-equal? (stringify (step-before step))
                                (to-string last)))
                (failed "mismatch at ~a before ~s and ~s"
                        n
                        (stringify (step-before step))
                        last)]
               [(not (ws-equal? (stringify (step-after step))
                                (to-string sexp)))
                (failed "mismatch at ~a after ~s and ~s"
                        n
                        (stringify (step-after step))
                        sexp)]
               [else
                (loop (cdr steps)
                      sexp
                      (cdr sexps)
                      (+ n 1))]))]))))

  (define (to-string s)
    (let ([sp (open-output-string)])
      (parameterize ([pretty-print-columns 'infinity]
                     [current-output-port sp])
        (pretty-print s))
      (get-output-string sp)))
  
  (define (stringify x)
    (unless (andmap string? x)
      (error 'stringify "cannot: ~s" x))
    (apply string-append x))
  
  (define (test-transcript init . sexp-steps)
    (apply test-transcript/defns '() init sexp-steps))
  
  (define (ws-equal? s1 s2)
    (equal? (regexp-replace* "[ \t\n]+"
                             (string-append " " s1 " ")
                             " ")
            (regexp-replace* "[ \t\n]+"
                             (string-append " " s2 " ")
                             " ")))
                       
  (define (run-sample-solution-tests)
    (let ([found-it? #f])
      (for-each
       (lambda (file) 
         (when (equal? file to-skip-to)
           (set! found-it? #t))
         (when found-it?
           (run-file-test (build-path sample-solutions-directory file))))
       (directory-list sample-solutions-directory))))
  
  ;; run-file-test : string -> void
  (define (run-file-test filename)
    (let/ec k
      (parameterize ([current-program-id filename]
                     [failure-escape k])
        (check-steps
         (make-file filename)
         (step-and-extract-program (make-file filename))))))
  
  ;; run-string-test : string -> void
  ;; runs the test in the string.
  (define (run-string-test prog)
    (let/ec k
      (parameterize ([current-program-id prog]
                     [failure-escape k])
        (check-steps
         prog
         (step-and-extract-program prog)))))
  
  ;; simple-failure : (union #f step) string any ... -> beta
  ;; indicates that this one test failed, but the stepper may still
  ;; be steppable, so just jumps out of this one test.
  (define (simple-failure step message . args)
    (printf "FAILED TEST ~s:\n" (current-program-id))
    (when step (printf "~s\n" step))
    (printf "~a\n\n" (apply format message args))
    ((failure-escape)))
  
  ;; check-steps : program-spec (listof step) -> void
  ;; executes each of the steps in DrRacket and raises
  ;; an exception if something doesn't match up.
  (define (check-steps program steps)
    (let* ([drs-frame (wait-for-drracket-frame)]
           [defs-text (send drs-frame get-definitions-text)])
      (let loop ([last-results #f]
                 [steps steps])
        (cond
          [(null? steps)
           (clear-definitions drs-frame)
           (type-in-definitions drs-frame ";; full prog\n")
           (set-definitions-to-program drs-frame program)
           (do-execute drs-frame)
           (let ([prog-results (fetch-output drs-frame)])
             
             ;; can only check subset here, since we may have stopped stepping early
             (check-subset-results #f last-results prog-results))]
          [else
           (let ([step (car steps)])
             (clear-definitions drs-frame)
             (dynamic-wind
              (lambda ()
                (send defs-text begin-edit-sequence))
              (lambda ()
                (type-in-definitions drs-frame ";; before\n")
                (insert-into-definitions drs-frame (step-definitions step))
                (type-in-definitions drs-frame "\n")
                (insert-into-definitions drs-frame (step-before step)))
              (lambda ()
                (send defs-text end-edit-sequence)))
             (do-execute drs-frame)
             (let ([before-results (fetch-output drs-frame)])
               (when last-results
                 (check-subset-results step last-results before-results))
               (clear-definitions drs-frame)
               (dynamic-wind
                (lambda ()
                  (send defs-text begin-edit-sequence))
                (lambda ()
                  (type-in-definitions drs-frame ";; after\n")
                  (insert-into-definitions drs-frame (step-definitions step))
                  (type-in-definitions drs-frame "\n")
                  (unless (err? (step-after step))
                    (insert-into-definitions drs-frame (step-after step))))
                (lambda ()
                  (send defs-text end-edit-sequence)))
               (do-execute drs-frame)
               (cond
                 [(err? (step-after step))
                  (let* ([pre-output (fetch-output drs-frame)]
                         [add-newline? (and ((string-length pre-output) . >= . 1)
                                            (not
                                             (char=?
                                              (string-ref pre-output 
                                                          (- (string-length pre-output) 1))
                                              #\newline)))]
                         [after-results 
                          (if add-newline?
                              (string-append pre-output
                                             "\n"
                                             (err-message (step-after step)))
                              (string-append pre-output
                                             (err-message (step-after step))))])
                    (check-same-results step before-results after-results)
                    (unless (null? (cdr steps))
                      (simple-failure #f "expected no more steps after an error, found ~s" (cdr steps)))
                    (loop after-results null))]
                 [else
                  (let ([after-results (fetch-output drs-frame)])
                    (check-same-results step before-results after-results)
                    (loop after-results (cdr steps)))])))]))))

  ;; insert-info-definitions : frame contents -> void
  ;; technically, this function should probably type the
  ;; contents into the definitions window, but that is
  ;; considerably slower than just inserting it directly....
  (define (insert-into-definitions drs-frame orig-contents)
    (let ([defns (send drs-frame get-definitions-text)])
      (let loop ([contents orig-contents])
        (for-each
         (lambda (content)
           (cond
             [(string? content)
              (send defns insert content
                    (send defns last-position)
                    (send defns last-position))]
             [(is-a? content snip%)
              (send defns insert (send content copy)
                    (send defns last-position)
                    (send defns last-position))]
             [(eq? content 'unknown)
              (error 'insert-into-definitions "found unknown snip in ~e" orig-contents)]
             [(list? content) 
              ;; wrong thing. this flattens embedded editors
              (loop content)]))
         contents))))
  
  ;; check-subset-results : step string string -> void
  ;; raises an error if s1 is not the beginning of s2.
  (define (check-subset-results step s1 s2)
    (unless (and (<= (string-length s1)
                     (string-length s2))
                 (string=? (substring s2 0 (string-length s1))
                           s1))
      (simple-failure step "expected\n  ~s\nto be the beginning of\n  ~s" s1 s2)))
  
  ;; check-same-results : step string string -> void
  ;; raises an error if s1 is not s2.
  (define (check-same-results step s1 s2)
    (unless (string=? s1 s2)
      (simple-failure step "expected\n  ~s\nto be the same as\n  ~s" s1 s2)))
  
  ;; step-and-extract-program : program-spec -> (listof step)
  (define (step-and-extract-program program)
    (let ([drs-frame (wait-for-drracket-frame)])
      (clear-definitions drs-frame)
      (set-definitions-to-program drs-frame program)
      (let* ([stepper-frame (start-stepper drs-frame)]
             [steps (get-all-steps stepper-frame)])
        (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Window"))
        (let ([drs-frame1 (wait-for-new-frame stepper-frame)])
          (unless (eq? drs-frame1 drs-frame)
            (error 'step-and-extract "didn't get back to drscheme frame, got: ~e" drs-frame)))
        steps)))

  ;; set-definitions-to-program : program-spec -> void
  (define (set-definitions-to-program drs-frame program)
    (cond
      [(string? program)
       (type-in-definitions drs-frame program)]
      [else
       (let ([definitions-text (send drs-frame get-definitions-text)])
         (send definitions-text load-file (file-name program))
         (send definitions-text set-filename #f))]))

  ;; start-stepper : frame -> frame
  (define (start-stepper drs-frame)
    (test:button-push (send drs-frame get-stepper-button))
    (let ([stepper-frame (wait-for-new-frame drs-frame)])
      stepper-frame))
  
  ;; get-all-steps : frame -> (listof step)
  (define (get-all-steps stepper-frame)
    (let* ([stepper-canvas (find-labelled-window #f editor-canvas% stepper-frame)]
           [stepper-editor (poll-until (lambda () 
                                         (let ([ed (send stepper-canvas get-editor)])
                                           (if (and ed
                                                    (not (send ed refresh-delayed?)))
                                               ed
                                               #f))))])
      (cons (get-step stepper-frame stepper-editor)
            (get-more-steps stepper-frame))))

  ;; get-more-steps : stepper-frame -> (listof step)
  ;; repeatedly push the next button to get out all of the steps
  (define (get-more-steps stepper-frame)
    (let ([next-button (find-labelled-window next-button-label button% stepper-frame)]
          [stepper-canvas (find-labelled-window #f editor-canvas% stepper-frame)])
      
      ;; wait until we are in a "ready" state.
      (poll-until (lambda () (send next-button is-enabled?))
                  2
                  void)
      
      ;; at most 200 steps
      (let loop ([n 200])
        (cond
          [(zero? n) null]
          [(send next-button is-enabled?)
           (let* ([before-editor (poll-until (lambda () (send stepper-canvas get-editor)))]
                  [new-step-available?
                   ;; if there is a new editor and the next button is enabled,
                   ;; we take that to mean that a new step has appeared.
                   (lambda ()
                     (let ([editor (send stepper-canvas get-editor)])
                       (and editor
                            (or (and (not (eq? before-editor editor))
                                     (not (send editor refresh-delayed?))
                                     (send next-button is-enabled?))
                                (editor-has-stepper-done-message editor)))))])
             (test:button-push next-button)
             (poll-until new-step-available? 2 void)
             (if (new-step-available?)
                 (let ([step (get-step stepper-frame (send stepper-canvas get-editor))])
                   (if step
                       (cons step (loop (- n 1)))
                       (loop (- n 1))))
                 null))]
          [else null]))))
  
  ;; editor-has-stepper-done-message : editor->boolean
  (define (editor-has-stepper-done-message editor)
    (string=? (send editor get-text 0 (send editor last-position))
              no-more-steps-message))
  
  ;; get-step : frame editor -> (union step #f)
  ;; extracts a step from the stepper window. Only returns #f for
  ;; the "I'm done stepping" message that sometimes appears at the end.
  (define (get-step stepper-frame stepper-editor)
    (let ([canvas (find-labelled-window #f canvas% stepper-frame (lambda () #f))])
      (when canvas
        (error 'get-steps "stepper warning present!")))
    (let* ([extraction (extract-from-editor stepper-editor)]
           [step (separate-steps extraction)])
      (unless step
        (unless (equal? (list no-more-steps-message) extraction)
          (error 'get-step "couldn't parse stepper window: ~s\n" extraction)))
      step))
      
  ;; snips = (union (cons error snips) (cons snip snips) (cons snips snips) null)
  ;; extract-from-editor : editor -> snips
  (define (extract-from-editor editor)
    (let loop ([snip (send editor find-first-snip)])
      (cond
        [(not snip)
         null]
        [(is-a? snip editor-snip%)
         (let ([editor (send snip get-editor)])
           (cons (cond
                   [(not editor) '()]
                   [(contains-error-message? editor)
                    (let ([ans (extract-from-editor editor)])
                      (unless (and (list? ans)
                                   (andmap string? ans))
                        (error 'extract-from-editor "couldn't parse error message: ~s" ans))
                      (make-err (apply string-append ans)))]
                   [else (extract-from-editor editor)])
                 (loop (send snip next))))]
        [(is-a? snip string-snip%)
         (cons (send snip get-text 0 (send snip get-count) #t)
               (loop (send snip next)))]
        [(or (is-a? snip image-snip%)
             (and (is-a? snip snip%)
                  (method-in-interface? 'get-fraction-view (object-interface snip))))
         (cons (send snip copy) (loop (send snip next)))]
        [else (cons 'unknown
                    (loop (send snip next)))])))
  
  ;; contains-error-message? : editor -> boolean
  ;; returns #t if the editors contents look like an error message
  (define (contains-error-message? editor)
    (let ([snip (send editor find-first-snip)])
      (and snip
           (error-style? (send snip get-style)))))  
  
  ;; error-style? : style -> boolean
  (define (error-style? style)
    (let ([color (send style get-foreground)])
      (and (color-equal? (send the-color-database find-color "red") color)
           (memq (send style get-style) '(slant italic)))))
  
  ;; color-equal? : color color -> boolean
  (define (color-equal? c1 c2)
    (and (= (send c1 red) (send c2 red))
         (= (send c1 green) (send c2 green))
         (= (send c1 blue) (send c2 blue))))

           
  ;; separate-steps : snips -> (union step #f)
  (define (separate-steps snips)
    (let/ec k
      (let ([program null]
            [before null]
            [after null]
            [snips snips])
        
        (define (die which when)
          (printf "~a: didn't find ~s\n  ~s\n  ~s\n  ~s\n  ~s\n" which when
                  program
                  before 
                  after 
                  snips))
        
        (define (hunt-for-list id)
          (let loop ()
            (cond
              [(null? snips)
               (die 'hunt-for-list.1 id)
               (k #f)]
              [(eq? 'unknown (car snips))
               (die 'hunt-for-list.2 id)
               (k #f)]
              [(list? (car snips))
               (begin0 (car snips)
                       (set! snips (cdr snips)))]
              [else (set! snips (cdr snips))
                    (loop)])))
        (define (hunt-for-list/error id)
          (let loop ()
            (cond
              [(null? snips) 
               (die 'hunt-for-list/error.1 id)
               (k #f)]
              [(eq? 'unknown (car snips)) 
               (die 'hunt-for-list/error.2 id)
               (k #f)]
              [(or (err? (car snips)) (list? (car snips)))
               (begin0 (car snips)
                       (set! snips (cdr snips)))]
              [else (set! snips (cdr snips))
                    (loop)])))
        (define (hunt-for-unknown id)
          (let loop ()
            (cond
              [(null? snips) 
               (die 'hunt-for-unknown id)
               (k #f)]
              [(eq? 'unknown (car snips)) (set! snips (cdr snips))]
              [else (set! snips (cdr snips))
                    (loop)])))
        
        ;(set! program (hunt-for-list 'program))
        (set! before (hunt-for-list 'before))
        (hunt-for-unknown 'between)
        (set! after (hunt-for-list/error 'after))
        
        (make-step program
                   before
                   after)))))
