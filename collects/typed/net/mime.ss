#lang typed-scheme

(require typed/private/utils)
;; -- basic mime structures --
(require-typed-struct disposition
                      ([type : Symbol] 
                       [filename : String]
                       [creation : String]
                       [modification : String]
                       [read : String]                                       
                       [size : Number]
                       [params : Any])
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
                               [fields : Any]
                               [parts : (Listof String) ]
                               [body : (Output-Port -> Void)])
                      net/mime)
(require-typed-struct message 
                      ([version : String] [entity :  entity] [fields : (Listof Symbol)])
                      net/mime)


;; -- exceptions raised --
(require/typed mime-error? (Any -> Boolean : (Opaque mime-error?)) net/mime)
(require/typed unexpected-termination? (Any -> Boolean :(Opaque unexpected-termination?)) net/mime)
(require/typed unexpected-termination-msg ((Opaque unexpected-termination?) -> message) net/mime)
(require/typed missing-multipart-boundary-parameter? (Any -> Boolean : (Opaque missing-multipart-boundary-parameter?)) net/mime)
(require/typed malformed-multipart-entity? (Any -> Boolean : (Opaque malformed-multipart-entity?)) net/mime)
(require/typed malformed-multipart-entity-msg ((Opaque malformed-multipart-entity?)-> message) net/mime)
(require/typed empty-mechanism? (Any -> Boolean : (Opaque empty-mechanism?)) net/mime)
(require/typed empty-type? (Any -> Boolean : (Opaque empty-type?)) net/mime)
(require/typed empty-subtype? (Any -> Boolean : (Opaque empty-subtype?)) net/mime)
(require/typed empty-disposition-type? (Any -> Boolean : (Opaque empty-disposition-type?)) net/mime)


;; -- mime methods --
(require/typed/provide net/mime
  [mime-analyze ((U Bytes Input-Port) Any -> message)])

(provide
 ;; -- exceptions raised --
 mime-error? 
 unexpected-termination? 
 unexpected-termination-msg
 missing-multipart-boundary-parameter? 
 malformed-multipart-entity?
 malformed-multipart-entity-msg
 empty-mechanism? 
 empty-type? 
 empty-subtype?
 empty-disposition-type? 
 
 ;; -- basic mime structures --
 message 
 entity
 
 disposition
 
 ;; -- mime methods --
 mime-analyze
)

