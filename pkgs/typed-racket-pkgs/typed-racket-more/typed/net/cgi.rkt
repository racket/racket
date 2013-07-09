#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/cgi
  [get-bindings (-> (Listof (cons (U Symbol String) String)))]
  [get-bindings/post (-> (Listof (Pair (U Symbol String) String)))]
  [get-bindings/get (-> (Listof (Pair (U Symbol String) String)))]
  [output-http-headers (-> Void)]
  [generate-html-output (case-lambda (String (Listof String) -> Void)
				     (String (Listof String) String String String String String -> Void))]
  [generate-error-output ((Listof String) -> (U))]
  [bindings-as-html ((Listof (cons (U Symbol String) String)) -> (Listof String))]
  [extract-bindings ((U Symbol String) (Listof (cons (U Symbol String) String)) -> ( Listof  String))]
  [extract-binding/single ((U Symbol String) (Listof (Pair (U Symbol String) String)) ->  String)]
  [get-cgi-method (-> (U "GET" "POST"))]
  [string->html (String -> String)]
  [generate-link-text (String String -> String)])
