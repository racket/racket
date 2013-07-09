#lang typed/racket/base

(require typed/private/utils)
;; -- basic mime structures --
(require-typed-struct disposition
                      ([type : Symbol]
                       [filename : String]
                       [creation : String]
                       [modification : String]
                       [read : String]
                       [size : Number]
                       [params : (Listof (Pair Symbol String))])
                      #:extra-constructor-name make-disposition
                      net/mime)
(require-typed-struct entity  ([type : (U Symbol String)]
                               [subtype : (U Symbol String)]
                               [charset : (U Symbol String)]
                               [encoding : Symbol]
                               [disposition : disposition ]
                               [params : (Listof (cons Symbol String))]
                               [id : String]
                               [description : String]
                               [other : String]
                               [fields : (Listof String)]
                               [parts : (Listof String) ]
                               [body : (Output-Port -> Void)])
                      #:extra-constructor-name make-entity
                      net/mime)
(require-typed-struct message
                      ([version : String] [entity :  entity] [fields : (Listof Symbol)])
                      #:extra-constructor-name make-message
                      net/mime)


;; -- exceptions raised --
#| ;; constructors not exported
(require-typed-struct mime-error () net/mime)
(require-typed-struct (unexpected-termination mime-error) ([msg : String]) net/mime)
(require-typed-struct (missing-multipart-boundary-parameter mime-error) () net/mime)
(require-typed-struct (malformed-multipart-entity mime-error) ([msg : String]) net/mime)
(require-typed-struct (empty-mechanism mime-error) () net/mime)
(require-typed-struct (empty-type mime-error) () net/mime)
(require-typed-struct (empty-subtype mime-error) () net/mime)
(require-typed-struct (empty-disposition-type mime-error) () net/mime)
|#
;; -- mime methods --
(require/typed/provide net/mime
  [mime-analyze ((U Bytes Input-Port) Any -> message)])

(provide
 ;; -- basic mime structures --
 (struct-out message)
 (struct-out entity)
 (struct-out disposition)
 #|
 (struct-out mime-error)
 (struct-out unexpected-termination)
 (struct-out missing-multipart-boundary)
 (struct-out malformed-multipart-entity)
 (struct-out empty-mechanism)
 (struct-out empty-type)
 (struct-out empty-subtype)
 (struct-out empty-disposition-type)
|#
 )

