#lang scheme/base
(require scribblings/reference/match-parse)

(provide match-grammar)

(define grammar "
pat     ::= id                         @match anything, bind identifier
         |  _                          @match anything
         |  literal                    @match literal
         |  'datum                     @match equal% datum
         |  (lvp ...)                  @match sequence of lvps
         |  (lvp ... . pat)            @match lvps consed onto a pat
         |  #(lvp ...)                 @match vector of pats
         |  #&pat                      @match boxed pat
         |  ($ struct-id pat ...)      @match struct-id instance
         |  (AND pat ...)              @match when all pats match
         |  (OR pat ...)               @match when any pat match
         |  (NOT pat ...)              @match when no pat match
         |  (= expr pat)               @match (expr value) to pat
         |  (? pred-expr pat ...)      @match if (expr value) and pats
         |  `qp                        @match quasipattern
literal ::= #t                         @match true
         |  #f                         @match false
         |  string                     @match equal% string
         |  number                     @match equal% number
         |  character                  @match equal% character
         |  bytes                      @match equal% byte string
         |  keyword                    @match equal% keyword
         |  regexp literal             @match equal% regexp literal
         |  pregexp literal            @match equal% pregexp literal
lvp     ::= pat ooo                    @greedily match pat instances
         |  pat                        @match pat
ooo     ::= ***                        @zero or more; *** is literal
         |  ___                        @zero or more
         |  ..K                        @K or more
         |  __K                        @K or more
qp      ::= literal                    @match literal
         |  id                         @match equal% symbol
         |  (qp ...)                   @match sequences of qps
         |  (qp ... . qp)              @match sequence of qps consed onto a qp
         |  (qp ... qp ooo)            @match qps consed onto a repeated qp
         |  #(qp ...)                  @match vector of qps
         |  #&qp                       @match boxed qp
         |  ,pat                       @match pat
         |  ,@pat                      @match pat, spliced
")

(define match-grammar
  (parse-match-grammar grammar))
