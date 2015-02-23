#lang scheme/base
(require "match-parse.rkt")

(provide match-grammar)

(define grammar "
pat     ::= id                                @match anything, bind identifier
         |  (VAR id)                          @match anything, bind identifier
         |  _                                 @match anything
         |  literal                           @match literal
         |  (QUOTE datum)                     @match equal% value
         |  (LIST lvp ...)                    @match sequence of lvps
         |  (LIST-REST lvp ... pat)           @match lvps consed onto a pat
         |  (LIST-NO-ORDER pat ...)           @match pats in any order
         |  (LIST-NO-ORDER pat ... lvp)       @match pats in any order
         |  (VECTOR lvp ...)                  @match vector of pats
         |  (HASH-TABLE (pat pat) ...)        @match hash table
         |  (HASH-TABLE (pat pat) ...+ ooo)   @match hash table
         |  (CONS pat pat)                    @match pair of pats
         |  (MCONS pat pat)                   @match mutable pair of pats
         |  (BOX pat)                         @match boxed pat
         |  (struct-id pat ...)               @match struct-id instance
         |  (STRUCT struct-id (pat ...))      @match struct-id instance
         |  (REGEXP rx-expr)                  @match string
         |  (REGEXP rx-expr pat)              @match string, result with pat
         |  (PREGEXP px-expr)                 @match string
         |  (PREGEXP px-expr pat )            @match string, result with pat
         |  (AND pat ...)                     @match when all pats match
         |  (OR pat ...)                      @match when any pat match
         |  (NOT pat ...)                     @match when no pat matches
         |  (APP expr pats ...)               @match (expr value) output values to pats
         |  (? expr pat ...)                  @match if (expr value) and pats
         |  (QUASIQUOTE qp)                   @match a quasipattern
         |  derived-pattern                   @match using extension
literal ::= #t                                @match true
         |  #f                                @match false
         |  string                            @match equal% string
         |  bytes                             @match equal% byte string
         |  number                            @match equal% number
         |  char                              @match equal% character
         |  keyword                           @match equal% keyword
         |  regexp literal                    @match equal% regexp literal
         |  pregexp literal                   @match equal% pregexp literal
lvp     ::= (code:line pat ooo)               @greedily match pat instances
         |  pat                               @match pat
qp      ::= literal                           @match literal
         |  id                                @match symbol
         |  (qp ...)                          @match sequences of qps
         |  (qp ... . qp)                     @match qps ending qp
         |  (qp ooo . qp)                     @match qps beginning with repeated qp
         |  #(qp ...)                         @match vector of qps
         |  #&qp                              @match boxed qp
         |  ,pat                              @match pat
         |  ,@(LIST lvp ...)                  @match lvps, spliced
         |  ,@(LIST-REST lvp ... pat)         @match lvps plus pat, spliced
         |  ,@'qp                             @match list-matching qp, spliced
ooo     ::= ***                               @zero or more; *** is literal
         |  ___                               @zero or more
         |  ..K                               @K or more
         |  __K                               @K or more
")

(define match-grammar
  (parse-match-grammar grammar))
