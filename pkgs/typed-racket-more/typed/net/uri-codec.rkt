#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/uri-codec
  [uri-encode ( String -> String )]
  [uri-decode ( String -> String )]

  [form-urlencoded-encode ( String -> String )]
  [form-urlencoded-decode ( String -> String )]

  [alist->form-urlencoded ( (Listof (cons Symbol String)) -> String )]
  [form-urlencoded->alist ( String -> (Listof (cons Symbol String)) )]
  [current-alist-separator-mode (Parameter Symbol)])
