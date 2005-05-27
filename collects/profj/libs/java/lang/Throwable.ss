#cs
(module Throwable mzscheme
  (require "Object-composite.ss")
  (provide Throwable (struct java:exception (object))
           exception-is-a? handle-exception create-java-exception))