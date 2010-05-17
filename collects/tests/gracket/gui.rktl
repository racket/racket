(let ([f (load-relative "gui-main.rktl")])
  (thread 
   (lambda ()
     (f "New" "Save" mred:console-frame%))))

