#lang racket/base

(require
  file/private/glob
  racket/contract
  (only-in racket/sequence sequence/c))

(provide
  glob/c
  (contract-out
    [glob (->* [glob/c] [#:capture-dotfiles? boolean?] (listof path-string?))]
    [in-glob (->* [glob/c] [#:capture-dotfiles? boolean?] (sequence/c path-string?))]
    [glob-match? (->* [glob/c path-string?] [#:capture-dotfiles? boolean?] boolean?)]
    [glob-quote (->i ([ps path-string?]) [r (ps) (if (path? ps) path? string?)])]
    [glob-capture-dotfiles? (parameter/c boolean?)]))
