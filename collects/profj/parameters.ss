(module parameters mzscheme
  
  (require (lib "class.ss"))
  
  (provide (all-defined))
  
  ;Stores the classpath for the current run
  (define classpath (make-parameter null))
  
  ;Stores syntax-oddness for datum->syntax-object
  (define syntax-location (make-parameter #f))
  
  ;Stores asts of other packages
  (define packages (make-parameter null))
  
  ;Stores asts of other classes
  (define check-list (make-parameter null))
  
  ;Stores a symbol representing the class from which main should be called (#f for none)
  (define main (make-parameter #f))
  
  ;Stores a boolean indicating if compilation is directed at a file
  (define to-file (make-parameter #f))
  
  ;Stores an integer offset for interactions offset
  (define interactions-offset (make-parameter 0))
  
  ;Stores if we are the execution window executing
  (define execution? (make-parameter #f))
  
  ;Stores the error function to trigger for parsing
  (define determine-error (make-parameter (lambda () #t)))
  
  ;Stores a function which when called will produce (->token) of lexed tokens
  (define lex-stream (make-parameter (lambda () null)))
  
  ;Stores whether dynamic typing is allowed
  (define dynamic? (make-parameter #f))
  
  ;Stores whether testing extension is on or not
  (define test-ext? (make-parameter #t))
  
  ;Stores whether the test window should pop up
  (define tests? (make-parameter #t))
  
  ;Stores whether coverage information should be gathered
  (define coverage? (make-parameter #t))
    
  ;Stores whether or not we're in MrEd and therefore images can appear in the text
  (define mred? (make-parameter #f))
  
  ;Stores whether it is permitted to use Scheme functions and other values
  (define scheme-ok? (make-parameter #f))
  
  ) 
