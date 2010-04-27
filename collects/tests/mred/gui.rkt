(let ([f (load-relative "gui-main.ss")])
  (thread 
   (lambda ()
     (f "New" "Save" mred:console-frame%))))

