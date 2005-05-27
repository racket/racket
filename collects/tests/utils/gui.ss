(module gui mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss"))
  (provide find-labelled-window whitespace-string=?)
  
  ;; whitespace-string=? : string string -> boolean
  ;; determines if two strings are equal, up to their whitespace.
  ;; each string is required to have whitespace in the same place,
  ;; but not necessarily the same kinds or amount.
  (define (whitespace-string=? string1 string2)
    (let loop ([i 0]
               [j 0]
               [in-whitespace? #t])
      (cond
        [(= i (string-length string1)) (only-whitespace? string2 j)]
        [(= j (string-length string2)) (only-whitespace? string1 i)]
        [else (let ([c1 (string-ref string1 i)]
                    [c2 (string-ref string2 j)])
                (cond
                  [in-whitespace?
                   (cond
                     [(whitespace? c1)
                      (loop (+ i 1)
                            j
                            #t)]
                     [(whitespace? c2)
                      (loop i
                            (+ j 1)
                            #t)]
                     [else (loop i j #f)])]
                  [(and (whitespace? c1)
                        (whitespace? c2))
                   (loop (+ i 1)
                         (+ j 1)
                         #t)]
                  [(char=? c1 c2)
                   (loop (+ i 1)
                         (+ j 1)
                         #f)]
                  [else #f]))])))
  
  ;; whitespace? : char -> boolean
  ;; deteremines if `c' is whitespace
  (define (whitespace? c)
    (or (char=? c #\newline)
        (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\return)))
  
  ;; only-whitespace? : string number -> boolean
  ;; returns true if string only contains whitespace, from index `i' onwards
  (define (only-whitespace? str i)
    (let loop ([n i])
      (cond
        [(= n (string-length str))
         #t]
        [(whitespace? (string-ref str n))
         (loop (+ n 1))]
        [else #f])))
  
  ;; whitespace-string=? tests
  '(map (lambda (x) (apply equal? x))
        (list (list #t (whitespace-string=? "a" "a"))
              (list #f (whitespace-string=? "a" "A"))
              (list #f (whitespace-string=? "a" " "))
              (list #f (whitespace-string=? " " "A"))
              (list #t (whitespace-string=? " " " "))
              (list #t (whitespace-string=? " " "  "))
              (list #t (whitespace-string=? "  " "  "))
              (list #t (whitespace-string=? "  " " "))
              (list #t (whitespace-string=? "a a" "a a"))
              (list #t (whitespace-string=? "a a" "a  a"))
              (list #t (whitespace-string=? "a  a" "a a"))
              (list #t (whitespace-string=? " a" "a"))
              (list #t (whitespace-string=? "a" " a"))
              (list #t (whitespace-string=? "a " "a"))
              (list #t (whitespace-string=? "a" "a "))))

  ;;; find-labelled-window : (union ((union #f string) -> window<%>)
  ;;;                               ((union #f string) (union #f class) -> window<%>)
  ;;;                               ((union #f string) (union class #f) area-container<%> -> window<%>))
  ;;;;  may call error, if no control with the label is found
  (define find-labelled-window
    (opt-lambda (label 
                 [class #f] 
                 [window (get-top-level-focus-window)] 
                 [failure (lambda ()
                            (error 'find-labelled-window "no window labelled ~e in ~e~a"
                                   label
                                   window 
                                   (if class
                                       (format " matching class ~e" class)
                                       "")))])
      (unless (or (not label)
                  (string? label))
	(error 'find-labelled-window "first argument must be a string or #f, got ~e; other args: ~e ~e"
	       label class window))
      (unless (or (class? class)
		  (not class))
	(error 'find-labelled-window "second argument must be a class or #f, got ~e; other args: ~e ~e"
	       class label window))
      (unless (is-a? window area-container<%>)
	(error 'find-labelled-window "third argument must be a area-container<%>, got ~e; other args: ~e ~e"
	       window label class))
      (let ([ans
	     (let loop ([window window])
	       (cond
                 [(and (or (not class)
                           (is-a? window class))
                       (let ([win-label (and (is-a? window window<%>)
                                             (send window get-label))])
                         (equal? label win-label)))
                  window]
                 [(is-a? window area-container<%>) (ormap loop (send window get-children))]
                 [else #f]))])
	(or ans
	    (failure))))))