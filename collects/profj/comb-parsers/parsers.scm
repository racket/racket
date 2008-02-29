(module parsers scheme/base
  (require "parser-units.scm"
           scheme/unit
           (only-in (lib "combinator-unit.ss" "combinator-parser") err^))
  
  (provide parse-beginner parse-intermediate parse-intermediate+access parse-advanced
           parse-beginner-interact parse-intermediate-interact parse-advanced-interact)
  (define (trim-string s f l)
    (substring s f (- (string-length s) l)))
  
  (define-values/invoke-unit beginner-definitions-parser@
    (import)
    (export (prefix beginner-def: parsers^) (prefix beginner-def: err^) token-proc^))  
  (define-values/invoke-unit beginner-interactions-parsers@
    (import)
    (export (prefix beginner-int: parsers^) (prefix beginner-int: err^)))

  (define-values/invoke-unit intermediate-definitions-parser@
    (import)
    (export (prefix intermediate-def: parsers^) (prefix intermediate-def: err^)))
  (define-values/invoke-unit intermediate-interactions-parsers@
    (import)
    (export (prefix intermediate-int: parsers^) (prefix intermediate-int: err^)))
  
  (define-values/invoke-unit intermediate+access-definitions-parser@
    (import)
    (export (prefix intermediate+acc-def: parsers^) (prefix intermediate+acc-def: err^)))
  (define-values/invoke-unit intermediate+access-interactions-parsers@
    (import)
    (export (prefix intermediate+acc-int: parsers^) (prefix intermediate+acc-int: err^)))


  (define-values/invoke-unit advanced-definitions-parser@
    (import)
    (export (prefix advanced-def: parsers^) (prefix advanced-def: err^)))
  (define-values/invoke-unit advanced-interactions-parsers@
    (import)
    (export (prefix advanced-int: parsers^) (prefix advanced-int: err^) ))
  
  (define (parse parser err? err-src err-msg)
    (lambda (program-stream location)
      (let ([output (parser (old-tokens->new program-stream) location)])
        (and (err? output) (list (err-msg output) (err-src output))))))
  
  (define parse-beginner (parse beginner-def:parse-program 
                                beginner-def:err? beginner-def:err-msg beginner-def:err-src))
  (define parse-intermediate (parse intermediate-def:parse-program
                                    intermediate-def:err? intermediate-def:err-msg intermediate-def:err-src))
  (define parse-intermediate+access (parse intermediate+acc-def:parse-program 
                                           intermediate+acc-def:err? intermediate+acc-def:err-msg intermediate+acc-def:err-src))
  (define parse-advanced (parse advanced-def:parse-program
                                advanced-def:err? advanced-def:err-msg advanced-def:err-src))
  (define parse-beginner-interact (parse beginner-int:parse-program
                                         beginner-int:err? beginner-int:err-msg beginner-int:err-src))
  (define parse-intermediate-interact (parse intermediate-int:parse-program
                                             intermediate-int:err? intermediate-int:err-msg intermediate-int:err-src))
  (define parse-advanced-interact (parse advanced-int:parse-program
                                         advanced-int:err? advanced-int:err-msg advanced-int:err-src))
  
  )
