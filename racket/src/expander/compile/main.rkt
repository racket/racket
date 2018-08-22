#lang racket/base
(require "context.rkt"
         "top.rkt"
         "multi-top.rkt"
         "module.rkt"
         "recompile.rkt")

;; Compilation of expanded code produces an S-expression (but enriched
;; with source locations and properties) where run-time primitive are
;; accessed directly, and all linklet imports and local variables are
;; renamed to avoid collisions with the primitive names and to make
;; every binding have a unique symbolic name (within the linklet) that
;; does not shadow a primitive. A `compile-linklet` function
;; (currently provided by the runtime system) then compiles the
;; enriched S-expression to bytecode.

;; Compilation to linklets uses one of two protocols, which differ in
;; the shapes of linklets that they generate:
;;
;; * Top-level forms or stand-alone expressions (such as the
;;   right-hand side of a `define-syntaxes` form within a module,
;;   which must be compiled to continue expanding the module) are
;;   compiled using one protocol.
;;
;;   In the case of top-level forms, a sequence of forms that affect
;;   binding or transformers must be compiled separately --- normally
;;   via `per-top-level` in "../eval/main.rkt". The separarately
;;   compiled forms can them be combined into a single compilation
;;   record.
;;
;;   The generated linklets for a single form include one linklet for
;;   potentially serialized module path indices and syntax objects,
;;   plus one linklet per relevant phase.
;;
;;   Multi-form combinations group the linklet sets for individual
;;   compilations in nested linklet directories. In addition, a
;;   linklet implements deserialization of all the data across
;;   top-level forms that are compiled together, so that they share.
;;   (In that case, the deserialization linklet with each inidvidual
;;   form turns out not to be used.)
;;
;; * Modules are compiled to a slightly different protocol. Like the
;;   top-level protocol, the resulting set of linklets includes on
;;   linklet per phase plus three linklets for housing potentially
;;   serialized data. An additional linklet reports metadata about the
;;   modules, such as its requires and provides. An individual module
;;   is represented by a linklet bundle, and a module is compiled with
;;   submodules through nested linklet directories.
;;
;;   Besides the extra metadata module, the handling of syntax-object
;;   deserialization is a little different for modules than top-level
;;   forms, because syntax-literal unmarshaling is lazy for modules.
;;
;; Whichever protocol is used, the result is wrapped in a
;; `compiled-in-memory` structure, which retains original module path
;; indices and syntax objects. If the compiled code is evaluated
;; directly, then the retained values are used instead of running
;; unmarshaling code in generated linklets. That's both faster an
;; preserves some expected sharing. When a `compile-in-memory`
;; structure is written, it writes the same as a linklet directory
;; (i.e., it loses the shortcut information, as well as some
;; inspector information).

(provide make-compile-context

         compile-single
         compile-top
         compiled-tops->compiled-top

         compile-module

         compiled-expression-recompile)
