;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/

(module chameneos mzscheme 

  (define (change c1 c2)
    (case c1
      [(red)
       (case c2	 [(blue) 'yellow] [(yellow) 'blue] [else c1])]
      [(yellow)
       (case c2 [(blue) 'red] [(red) 'blue] [else c1])]
      [(blue)
       (case c2 [(yellow) 'red] [(red) 'yellow] [else c1])]))
  
  (define (place meeting-ch n)
    (thread
     (lambda ()
       (let loop ([n n])
	 (if (zero? n)
	     ;; Fade all:
	     (let loop ()
	       (let ([c (channel-get meeting-ch)])
		 (channel-put (car c) #f)
		 (loop)))
	     ;; Let two meet:
	     (let ([c1 (channel-get meeting-ch)]
		   [c2 (channel-get meeting-ch)])
	       (channel-put (car c1) (cdr c2))
	       (channel-put (car c2) (cdr c1))
	       (loop (sub1 n))))))))

  (define (creature color meeting-ch result-ch)
    (thread 
     (lambda ()
       (let ([ch (make-channel)])
	 (let loop ([color color][met 0])
	   (channel-put meeting-ch (cons ch color))
	   (let ([other-color (channel-get ch)])
	     (if other-color
		 ;; Meet:
		 (loop (change color other-color) (add1 met))
		 ;; Done:
		 (channel-put result-ch met))))))))

  (let ([result-ch (make-channel)]
	[meeting-ch (make-channel)])
    (place meeting-ch (string->number (vector-ref (current-command-line-arguments) 0)))
    (creature 'blue meeting-ch result-ch)
    (creature 'red meeting-ch result-ch)
    (creature 'yellow meeting-ch result-ch)
    (creature 'blue meeting-ch result-ch)
    (printf "~a\n" (+ (channel-get result-ch)
		      (channel-get result-ch)
		      (channel-get result-ch)
		      (channel-get result-ch)))))
