(module test-box-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
	   (rename (lib  "teachprims.ss" "lang" "private") beginner-equal? beginner-equal?))
  
  (provide run-test)

  (define (run-test)
    
    (define drscheme-frame (wait-for-drscheme-frame))
    
    (define definitions-text (send drscheme-frame get-definitions-text))
    (define definitions-canvas (send drscheme-frame get-definitions-canvas))
    (define execute-button (send drscheme-frame get-execute-button))
    
    (define (insert-string string)
      (let loop ([n 0])
        (unless (= n (string-length string))
          (let ([c (string-ref string n)])
            (if (char=? c #\newline)
                (test:keystroke #\return)
                (test:keystroke c)))
          (loop (+ n 1)))))
    
    (define wait-for-execute (lambda () (wait-for-button execute-button)))

    (define (get-test-box)
      (send definitions-text find-snip (send definitions-text last-position) 'before))

    (define (get-test-image fn)
      (make-object 
       image-snip%
       (make-object bitmap% (build-path (collection-path "test-suite" "private" "icons") fn))))

    (define check-img (get-test-image "small-check-mark.jpeg"))
    (define cross-img (get-test-image "small-cross.jpeg"))
    (define non-img (get-test-image "small-empty.gif"))

    (define red-square (make-object image-snip%
				    (let* ([bm (make-object bitmap% 20 20)]
					   [dc (make-object bitmap-dc% bm)])
				      (send dc set-brush "red" 'solid)
				      (send dc set-pen "red" 1 'solid)
				      (send dc draw-rectangle 0 0 20 20)
				      (send dc set-bitmap #f)
				      bm)))

    (define (same-img? a b)
      (beginner-equal? a b))

    (define (test-box-status s)
      (let eloop ([e (send s get-editor)])
	(let sloop ([s (send e find-first-snip)])
	  (and s
	       (or
		(cond
		 [(s . is-a? . editor-snip%)
		  (eloop (send s get-editor))]
		 [(s . is-a? . image-snip%)
		  (cond
		   [(same-img? s check-img) 'pass]
		   [(same-img? s cross-img) 'fail]
		   [(same-img? s non-img) 'not-run]
		   [else #f])]
		 [else #f])
		(sloop (send s next)))))))

    (define (check-test-box-status v s)
      (let ([u (test-box-status s)])
	(if (eq? v u)
	    (printf "~a - good\n" v)
	    (printf "FAILED: ~a != ~a\n" u v))))
	

    (define (try-test preamble expr expect result)
      (test:new-window definitions-canvas)
      (send definitions-text erase)
      
      (insert-string preamble)

      (test:menu-select "Special" "Insert Test Case")
      
      (insert-string expr)
      (insert-string "\t")
      (let loop ([expect expect])
	(cond
	 [(expect . is-a? . snip%)
	  (send (send drscheme-frame get-edit-target-object) insert (send expect copy))]
	 [(list? expect)
	  (for-each loop expect)]
	 [else
	  (insert-string expect)]))
      
      (check-test-box-status 'not-run (get-test-box))
      (do-execute drscheme-frame #t)
      (check-test-box-status result (get-test-box)))

    (define scheme-languages
      '(("How to Design Programs" "Beginning Student")
	("How to Design Programs" "Intermediate Student")
	("How to Design Programs" "Advanced Student")
	("PLT" #rx"Textual")
	("PLT" #rx"Graphical")))

    (for-each (lambda (lang)
		(set-language-level! lang #t)

		(try-test "" "(+ 1 2)" "3" 'pass)
		(try-test "" "(+ 1 -2)" "3" 'fail)
		(try-test "" "(list 1)" "(list 1)" 'pass)
		(try-test "" "not-defined" "3" 'not-run)
		(try-test "(define (f x) (+ x 1))" "(f 2)" "3" 'pass)
		
		(use-get/put-dialog
		 (lambda ()
		   (test:menu-select "Language" "Add Teachpack..."))
		 (build-path (collection-path "mzlib") 'up 'up "teachpack" "htdp" "image.ss"))
		(try-test "" "(rectangle 20 20 'solid \"red\")" "3" 'fail)
		(try-test "" "(rectangle 20 20 'solid \"red\")" red-square 'pass)
		(try-test "" "(list (rectangle 20 20 'solid \"red\") 17)" `("(list " ,red-square " 17)") 'pass)
		(test:menu-select "Language" "Clear All Teachpacks")
		
		(void))
	      scheme-languages)

    (void)))
