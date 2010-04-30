(let ([f (load-relative "gui-main.rkt")])
  (thread 
   (lambda ()
     (f "New" "Save" mred:console-frame%))))

