(module beginner-contracts mzscheme
  
  (require "contracts-helpers.ss"
           lang/posn
           lang/private/teach
           mzlib/list)
  
  (provide (all-defined))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  flat type contracts for beginner language
  
  (define any-contract (lambda (stx) (build-flat-contract (lambda (x) #t) 'any stx)))
  (define symbol-contract (lambda (stx) (build-flat-contract symbol? 'symbol stx)))
  (define number-contract (lambda (stx) (build-flat-contract number? 'number stx)))
  (define integer-contract (lambda (stx) (build-flat-contract integer? 'integer stx)))
  (define exact-number-contract (lambda (stx)  (build-flat-contract exact? 'exact-number stx)))
  (define inexact-number-contract (lambda (stx) (build-flat-contract inexact? 'inexact-number stx)))
  (define boolean-contract (lambda (stx) (build-flat-contract boolean? 'boolean stx)))
  (define true-contract (lambda (stx) (build-flat-contract (lambda (x) (eq? x #t)) 'true stx)))
  (define false-contract (lambda (stx)  (build-flat-contract (lambda (x) (eq? x #f)) 'false stx)))
  (define string-contract (lambda (stx) (build-flat-contract string? 'string stx)))
  (define posn-contract (lambda (stx) (build-flat-contract posn? 'posn stx)))
  (define empty-contract (lambda (stx) (build-flat-contract null? 'empty stx)))
  (define list-contract (lambda (stx) (build-flat-contract pair? 'list stx))))
