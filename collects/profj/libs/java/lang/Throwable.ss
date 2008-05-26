(module Throwable scheme/base
  (require "Object-composite.ss")
  (provide Throwable (struct-out java:exception)
           exception-is-a? handle-exception create-java-exception)
  (provide guard-convert-Throwable convert-assert-Throwable wrap-convert-assert-Throwable 
           dynamic-Throwable/c static-Throwable/c))
