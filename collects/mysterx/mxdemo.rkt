;;; mxdemo.rkt -- demo program for MysterX

;;; Requires the MSCal Calendar control.
;;; The calendar control is normally installed with
;;; MS Office, but it can also be downloaded from
;;; elsewhere. Look for "mscal.ocx".

(module mxdemo mzscheme
  
  (require mzlib/class)
  (require mysterx)
  
  (require mzlib/runtime-path
           mzlib/process)

  ;; Ensure that DLLs are included with the distibution:
  (define-runtime-path myspage-dll '(so "myspage"))
  (define-runtime-path myssink-dll '(so "myssink"))
  ;; Register them every time we start:
  (system* "regsvr32.exe" "/s" (path->string myspage-dll))
  (system* "regsvr32.exe" "/s" (path->string myssink-dll))

  ; the browser with the calendar
  
  (define calwb (instantiate mx-browser% 
                  () ; no by-position initializers
                  (label "Calendar control")
                  (height 400)
                  (width 350)
                  (y 100)
                  (x 100)
                  (style-options '(scrollbars))))
  
  (define caldoc (send calwb current-document))
  
  (send caldoc insert-html 
	(string-append	
	 "<H1 id=\"mx-header\">MysterX Demo</H1>"
	 "<p>"
	 "<hr>"
	 "<p>"
	 (progid->html "MSCAL.Calendar.7" 300 200)
	 "<p>"
	 "<H3 id=\"event-header\"></H3>"))
  
  (define cal (car (send caldoc objects)))
  
  ; the control panel document 
  (define ctrlwb (make-object mx-browser% "Control Panel" 180 350 600 300 '()))
  (define ctrldoc (send ctrlwb current-document))
  
  (send ctrldoc insert-html 
	(string-append	
         "<table align = center>"
         
         "<caption id=\"Caption\"><b>Keypress here</b></caption>"
         
         "<colgroup align=left>"
         "<colgroup align=center>"
         "<colgroup align=right>"
         
         "<tr>"
         "<td><BUTTON id=\"Yesterday\" style=\"color: blue\">&LT----</BUTTON></td>"
         "<td><b>Day</b></td>"
         "<td><BUTTON id=\"Tomorrow\" style=\"color: red\">----&GT</BUTTON></td>"
         "</tr>"
         
         "<tr>"
         "<td><BUTTON id=\"Last-month\" style=\"color: green\">&LT----</BUTTON></td>"
         "<td><b>Month</b></td>"
         "<td><BUTTON id=\"Next-month\" style=\"color: indigo\">----&GT</BUTTON></td>"
         "</tr>"
         
         "<tr>"
         "<td><BUTTON id=\"Last-year\" style=\"color: yellow\">&LT----</BUTTON></td>"
         "<td><b>Year</b></td>"
         "<td><BUTTON id=\"Next-year\" style=\"color: purple\">----&GT</BUTTON></td>"
         "</tr>"
         
         "</table>"
         
         "<table align=center>"
         
         "<td><BUTTON id=\"Today\">Today</BUTTON></td>"
         
         "</table>"
         
         "<hr>"
         
         "<table align=center>"
         
         "<tr>"
         "<td><BUTTON id=\"Hide\">Hide</BUTTON></td>"
         "<td><BUTTON id=\"Show\">Show</BUTTON></td>"
         "</tr>"
         
         "</table>"
         
         "<table align=center>"         
         "<td><BUTTON id=\"Rub-me\">Rub me!</BUTTON></td>"
         "</table>"
         
         "<table align=center>"
         "<td><BUTTON id=\"About\">About</BUTTON></td>"
         "</table>"
         
         "<table align=center>"
         "<td><BUTTON id=\"Quit\">Quit</BUTTON></td>"
         "</table>"
         
         "<p>"
         
         "<table align=center>"
         
         "<td id=\"event-reflector\">Click on the calendar</td>"
         
         "</table>"))
  
  (define reflector (send ctrldoc find-element "TD" "event-reflector"))
  
  (com-register-event-handler 
   cal "Click" 
   (lambda ()
     (send reflector set-color! 'white) 
     (send reflector set-background-color! 'blue)
     (thread
      (lambda ()
        (sleep 0.25)
        (send reflector set-color! 'black) 
        (send reflector set-background-color! 'white)))))
  
  (define (quit-handler ev)
    (when (send ev click?)
      (exit)))
  
  (define (about-handler ev)
    (when (send ev click?)
      (com-invoke cal "AboutBox")))
  
  (define (hide-handler ev)
    (when (send ev click?)
      (send calwb show #f)))
  
  (define (show-handler ev)
    (when (send ev click?)
      (send calwb show #t)))
  
  (define rub-me-handler
    (let ([count 0])
      (lambda (ev)
        (when (send ev mousemove?)
          (printf "mousemove #~a, but who's counting?\n" count)
          (set! count (add1 count))))))
  
  (define (today-handler ev)
    (when (send ev click?)
      (com-invoke cal "Today")))
  
  (define (yesterday-handler ev)
    (when (send ev click?)
      (com-invoke cal "PreviousDay")))
  
  (define (tomorrow-handler ev)
    (when (send ev click?)
      (com-invoke cal "NextDay")))
  
  (define (last-month-handler ev)
    (when (send ev click?)
      (com-invoke cal "PreviousMonth")))
  
  (define (next-month-handler ev)
    (when (send ev click?)
      (com-invoke cal "NextMonth")))
  
  (define (last-year-handler ev)
    (when (send ev click?)
      (com-invoke cal "PreviousYear")))
  
  (define (next-year-handler ev)
    (when (send ev click?)
      (com-invoke cal "NextYear")))
  
  (define button-handlers
    `(("Quit" ,quit-handler)
      ("About" ,about-handler)
      ("Hide" ,hide-handler)
      ("Show" ,show-handler)
      ("Rub-me" ,rub-me-handler)
      ("Today" ,today-handler)
      ("Yesterday" ,yesterday-handler)
      ("Tomorrow" ,tomorrow-handler)
      ("Last-month" ,last-month-handler)
      ("Next-month" ,next-month-handler)
      ("Last-year" ,last-year-handler)
      ("Next-year" ,next-year-handler)))
  
  (send ctrlwb register-event-handler
        (send ctrldoc find-element "CAPTION" "Caption")
	(lambda (ev)
	  (when (send ev keypress?)
            (printf "ooh that tickles\n"))))
  
  (for-each
   (lambda (sym-handler)
     (send ctrlwb register-event-handler 
           (send ctrldoc find-element 
                 "BUTTON"              ; tag
                 (car sym-handler))    ; id
           (cadr sym-handler)))        ; handler
   button-handlers)
  
  (send ctrlwb handle-events)
  
  ;; Wait
  (sync never-evt))
