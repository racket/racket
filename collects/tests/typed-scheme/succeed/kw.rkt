#lang typed-scheme

(lambda ()   
  (open-input-file "foo" #:mode 'binary)  
  (open-input-file "foo" #:mode 'text)
  (open-input-file "foo"))
