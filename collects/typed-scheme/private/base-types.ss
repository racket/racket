#lang scheme/base

(require (for-template (only-in (lib "list.ss") foldl)
                       scheme/base
                       '#%paramz
                       scheme/promise
                       string-constants/string-constant
                       #;'#%more-scheme
                       #;'#%qq-and-or
                       (only-in scheme/match/patterns match:error))
         )


(require
 "extra-procs.ss"
 scheme/promise
 (except-in "type-rep.ss" make-arr)
 (only-in scheme/list cons?)
 "type-effect-convenience.ss"
 (only-in "type-effect-convenience.ss" [make-arr* make-arr])
 "union.ss"
 string-constants/string-constant
 (only-in scheme/match/patterns match:error)
 "tc-structs.ss")

(require (for-syntax
          scheme/base
          "init-envs.ss"
          (except-in "type-rep.ss" make-arr)
          (only-in (lib "list.ss") foldl)
          "type-effect-convenience.ss"
          (only-in "type-effect-convenience.ss" [make-arr* make-arr])
          "union.ss"
          string-constants/string-constant
          (only-in scheme/match/patterns match:error)
          "tc-structs.ss"))

;; the initial type name environment - just the base types
(define-syntax (define-tname-env stx)
  (syntax-case stx ()
    [(_ var provider [nm ty] ...)
     #`(begin
         (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
         (provide nm) ...
         (define-syntax provider (lambda (stx) #'(begin (provide nm) ...)))
         (provide provider)
         (begin-for-syntax
           (initialize-type-name-env
            (list (list #'nm ty) ...))))]))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ provider requirer nm ...)
     (with-syntax ([(nms ...) (generate-temporaries #'(nm ...))])
       (let ([body-maker (lambda (stx)
                           (map (lambda (nm nms) (datum->syntax stx `(rename ,#'mod ,nm ,nms)))
                                (syntax->list #'(nm ...))
                                (syntax->list #'(nms ...))))])
         #'(begin (define-syntax nms (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
                  (provide nms) ...
                  (define-syntax (requirer stx) 
                    (syntax-case stx () 
                      [(_ mod) 
                       (datum->syntax
                        stx
                        `(require . ,(map (lambda (nm* nms*) (datum->syntax stx `(rename ,#'mod ,nm* ,nms*)))
                                          (list 'nm ...)
                                          (list #'nms ...))))]))                  
                  (define-syntax provider (lambda (stx) #'(begin (provide (rename-out [nms nm])) ...)))
                  (provide provider requirer))))]))

;; the initial set of available type names
(define-tname-env initial-type-names provide-tnames
  [Number N]
  [Integer -Integer]
  [Void -Void]
  [Boolean B]
  [Symbol Sym]
  [String -String]
  [Any Univ]
  [Port -Port]
  [Path -Path]
  [Regexp -Regexp]
  [PRegexp -PRegexp]
  [Char -Char]
  [Option (-poly (a) (-opt a))]
  [List (-lst Univ)]
  [Listof -Listof]
  [Namespace -Namespace]
  [Input-Port -Input-Port]
  [Output-Port -Output-Port]
  [Bytes -Bytes]
  [EOF (-val eof)]
  [Keyword -Keyword]
  [HashTable (-poly (a b) (-HT a b))]
  [Promise (-poly (a) (-Promise a))]
  [Pair (-poly (a b) (-pair a b))]
  [Box (-poly (a) (make-Box a))]
  [Syntax Any-Syntax]
  [Identifier Ident]
  )

(define-other-types
  provide-extra-tnames
  require-extra-tnames
  
  
  -> U mu Un All Opaque Vectorof
  Parameter Tuple Class
  )  

(provide-extra-tnames)