;; this file tests frames with various edits in them

(define testing-frame #f)

(define test-frame/edit
  (lambda (frame% edit% title)
    (let* ([frame (make-object
		   (class frame% args
		     (public [get-edit% (lambda () edit%)])
		     (inherit show)
		     (sequence (apply super-init args))))]
	   [edit (send frame get-edit)]
	   [string-good "test insert"]
	   [string-bad "SHOULD NOT SEE THIS"]
	   [get-insertion
	    (lambda (string)
	      (if (is-a? edit wx:media-edit%)
		  string
		  (let ([snip (make-object wx:media-snip%)]
			[snip-e (make-object mred:media-edit%)])
		    (send snip set-media snip-e)
		    (send snip-e insert string)
		    snip)))])
      (set! testing-frame frame)
      (send frame set-title-prefix title)
      (send frame show #t)
      (send edit insert (get-insertion string-good))
      (send edit lock #t)
      (send edit insert (get-insertion string-bad))
      (send edit lock #f))))

(define continue? #t)

(define close-down
  (lambda ()
    (let ([answer (mred:get-choice "Continue the test suite?"
				   "Yes" "No"
				   "connections test suite")])
      (when (send testing-frame on-close)
	(send testing-frame show #f))
      (unless answer
	(error 'close-down)))))

(define-macro frame/edit
  (lambda (frame% edit%)
    `(when continue?
       (printf "testing frame: ~a edit: ~a~n" ',frame% ',edit%)
       (test-frame/edit ,frame% ,edit% (format "~a ~a" ',frame% ',edit%)))))
	     
(define searching-frame% (mred:make-searchable-frame% mred:simple-menu-frame%))
(define searching-info-frame% (mred:make-searchable-frame% mred:info-frame%))

(frame/edit mred:pasteboard-frame% mred:pasteboard%) (close-down)
(frame/edit mred:simple-menu-frame% mred:media-edit%) (close-down)
(frame/edit searching-frame% mred:media-edit%) (close-down)

(frame/edit mred:info-frame% mred:info-edit%) (close-down)

(frame/edit searching-info-frame% mred:searching-edit%)
(mred:find-string (send testing-frame get-canvas)
		  null
		  0 0 (list 'ignore-case))
(close-down)

(frame/edit mred:info-frame% mred:clever-file-format-edit%) (close-down)
(frame/edit mred:info-frame% mred:file-edit%) (close-down)
(frame/edit mred:info-frame% mred:backup-autosave-edit%) (close-down)
(frame/edit mred:info-frame% mred:scheme-mode-edit%) (close-down)

(frame/edit searching-info-frame% mred:clever-file-format-edit%) (close-down)
(frame/edit searching-info-frame% mred:file-edit%) (close-down)
(frame/edit searching-info-frame% mred:backup-autosave-edit%) (close-down)
(frame/edit searching-info-frame% mred:scheme-mode-edit%) (close-down)
