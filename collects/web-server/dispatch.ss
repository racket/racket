(module dispatch mzscheme
  (require "connection-structs.ss"
           "request-structs.ss"
           "response-structs.ss")
  (require (lib "contract.ss"))
  
  (provide dispatcher?)
  
  (define dispatcher? (connection? request? . -> . response?)))