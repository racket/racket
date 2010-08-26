(lambda (new-name save-name console%)
  (let* ([dir (current-load-relative-directory)]
	 [console 
	  (let loop ([printout? #f])
	    (let ([f (mred:test:get-active-frame)])
	      (if (and f
		       (is-a? f console%))
		  f
		  (begin
		    (unless printout?
		      (printf "please select the console\n"))
		    (sleep 1/2)
		    (loop #t)))))]
	 [wait
	  (lambda (test desc-string [time 5])
	    (let ([int 1/2])
	      (let loop ([sofar 0])
		(cond
		  [(> sofar time) (error 'wait desc-string)]
		  [(test) (void)]
		  [else (sleep int)
			(loop (+ sofar int))]))))]
	 [wait-pending
	  (lambda ()
	    (wait (lambda () (= 0 (mred:test:number-pending-actions)))
		  "pending action sdidn't terminate")
	    (mred:test:reraise-error))]
	 [_ (mred:test:menu-select "File" new-name)]
	 [_ (wait-pending)]
	 [_ (wait (lambda () (not (eq? (mred:test:get-active-frame) console)))
		  "focus didn't change from the console after File|New")]
	 [frame (mred:test:get-active-frame)]
	 [_ (mred:test:keystroke #\a)]
	 
	 [_ (mred:test:menu-select "File" "Close")]
	 [_ (wait (lambda () (not (eq? frame (mred:test:get-active-frame))))
		  "active frame remained original frame after File|Close")]
	 [_ (mred:test:button-push "Cancel")]
	 [_ (wait-pending)]
	 
	 [_ (mred:test:menu-select "File" "Close")]
	 [_ (wait (lambda () (not (eq? frame (mred:test:get-active-frame))))
		  "active frame remained original frame after File|Close")]
	 [_ (mred:test:button-push "Cancel")]
	 [_ (wait-pending)]
	 
	 [_ (wait (lambda () (eq? frame (mred:test:get-active-frame)))
		  "active frame did not return to editor frame")]
	 [_ (mred:test:menu-select "File" "Close")]
	 [_ (wait (lambda () (not (eq? frame (mred:test:get-active-frame))))
		  "active frame remained original frame after File|Close")]
	 [_ (mred:test:button-push "Close Anyway")]
	 [_ (wait-pending)]
	 
	 [_ (unless (mred:get-preference 'mred:autosaving-on?)
	      (error 'autosave "autosaving preference turned off. Turn back on (with preferences dialog)"))]
	 [tmp-file (build-path dir "tmp.rktd")]
	 [backup-file (build-path dir "#tmp.rktd#1#")]
	 [_ (call-with-output-file tmp-file
	      (lambda (port) (display "12" port))
	      'truncate)]
	 [_ (when (file-exists? backup-file)
	      (delete-file backup-file))]
	 [_ (mred:edit-file tmp-file)]
	 [_ (wait (lambda () (not (eq? console (mred:test:get-active-frame))))
		  "after mred:edit-file, the console remained active")]
	 [frame (mred:test:get-active-frame)]
	 [_ (mred:test:keystroke #\3)]
	 [autosave-time (+ 10 (mred:get-preference 'mred:autosave-delay))]
	 [_ (printf "waiting for autosave timeout (~a secs)\n" autosave-time)]
	 [_ (sleep autosave-time)]
	 [_ (printf "finished waiting for autosave timeout\n")]
	 [_ (unless (file-exists? backup-file)
	      (error 'autosave "autosave file (~a) not created" backup-file))]
	 [_ (mred:test:menu-select "File" save-name)]
	 [_ (wait-pending)]
	 [_ (when (file-exists? backup-file)
	      (error 'autosave "autosave file (~a) not deleted after original file saved"))]
	 [_ (mred:test:menu-select "File" "Close")]
	 [_ (wait-pending)]
	 [_ (wait (lambda () (eq? (mred:test:get-active-frame) console))
		  "focus didn't return to the console after closing autosave test frame")])
    (printf "test finished\n")))

;
;  when rewriting, apply this function to: 
; "New Unit"
; "Save Definitions"
;  wx:frame%
