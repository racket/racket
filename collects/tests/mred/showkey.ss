
(require (lib "etc.ss")
	 (lib "class100.ss"))

(let ()
  (define c%
       (class100-asi canvas%
	 (override
	   [on-event
	    (lambda (ev)
	      (printf "~aMOUSE ~a (~a,~a)~n  meta: ~a  control: ~a  alt: ~a  shift: ~a~n  buttons: ~a ~a ~a~a~a~a~a~n" 
		      (es-check)
		      (send ev get-event-type)
		      (send ev get-x)
		      (send ev get-y)
		      (send ev get-meta-down)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)
		      (send ev get-left-down)
		      (send ev get-middle-down)
		      (send ev get-right-down)
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
	      (printf "~aKEY code: ~a  rel-code: ~a~n  meta: ~a  control: ~a  alt: ~a  shift: ~a~n" 
		      (es-check)
		      (let ([v (send ev get-key-code)])
			(if (symbol? v)
			    v
			    (format "~a = ASCII ~a" v (char->integer v))))
		      (let ([v (send ev get-key-release-code)])
			(if (symbol? v)
			    v
			    (format "~a = ASCII ~a" v (char->integer v))))
		      (send ev get-meta-down)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)))])))
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
			 ">>WRONG EVENTSPACE<< "))
  (send c focus)
  (send f show #t))

