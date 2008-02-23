
(require mzlib/etc
	 mzlib/class100)

(let ()
  (define iter 0)
  (define c%
       (class100-asi canvas%
	 (override
	   [on-event
	    (lambda (ev)
	      (printf "~a~a MOUSE ~a (~a,~a)\n  mods:~a~a~a~a~a\n  buttons:~a~a~a~a~a~a~a~n" 
		      (es-check)
		      iter
		      (send ev get-event-type)
		      (send ev get-x)
		      (send ev get-y)
		      (if (send ev get-meta-down) " META" "")
		      (if (send ev get-control-down) " CTL" "")
		      (if (send ev get-alt-down) " ALT" "")
		      (if (send ev get-shift-down) " SHIFT" "")
		      (if (send ev get-caps-down) " CAPS" "")
		      (if (send ev get-left-down) " LEFT" "")
		      (if (send ev get-middle-down) " MIDDLE" "")
		      (if (send ev get-right-down) " RIGHT" "")
		      (if (send ev dragging?)
			  " dragging"
			  "")
		      (if (send ev moving?)
			  " moving"
			  "")
		      (if (send ev entering?)
			  " entering"
			  "")
		      (if (send ev leaving?)
			  " leaving"
			  "")))]
	   [on-char
	    (lambda (ev)
	      (set! iter (add1 iter))
	      (printf "~a~a KEY: ~a\n  rel-code: ~a\n  other-codes: ~a\n  mods:~a~a~a~a~a~n" 
		      (es-check)
		      iter
		      (let ([v (send ev get-key-code)])
			(if (symbol? v)
			    v
			    (format "~s = ASCII ~a" (string v) (char->integer v))))
		      (let ([v (send ev get-key-release-code)])
			(if (symbol? v)
			    v
			    (format "~s = ASCII ~a" (string v) (char->integer v))))
		      (let ([vs (list (send ev get-other-shift-key-code)
                                      (send ev get-other-altgr-key-code)
                                      (send ev get-other-shift-altgr-key-code)
                                      (send ev get-other-caps-key-code))])
                        (map (lambda (v)
                               (and v
                                    (if (symbol? v)
                                        v
                                        (format "~s = ASCII ~a" (string v) (char->integer v)))))
                             vs))
		      (if (send ev get-meta-down) " META" "")
		      (if (send ev get-control-down) " CTL" "")
		      (if (send ev get-alt-down) " ALT" "")
		      (if (send ev get-shift-down) " SHIFT" "")
		      (if (send ev get-caps-down) " CAPS" "")))])))
  (define f (make-object (class100 frame% ()
                           (inherit accept-drop-files)
                           (override
                             [on-drop-file (lambda (file)
                                             (printf "Dropped: ~a~n" file))])
                           (sequence
                             (super-init "tests" #f 100 100)
                             (accept-drop-files #t)))))
  (define c (make-object c% f))
  (define (es-check) (if (eq? (send f get-eventspace) (current-eventspace))
			 ""
			 ">>WRONG EVENTSPACE<<\n"))
  (send c focus)
  (send f show #t))

