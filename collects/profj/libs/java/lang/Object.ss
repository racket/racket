#cs
(module Object mzscheme
  (require "Object-composite.ss")
  (provide ObjectI Object-Mix Object)
  (provide guard-convert-Object convert-assert-Object wrap-convert-assert-Object 
           dynamic-Object/c static-Object/c wrapper))
