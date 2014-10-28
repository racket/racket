#lang scribble/manual
@(require scribble/eval
          (for-label drracket/check-syntax
                     racket))
@(define syncheck-example-eval (make-base-eval))
@(begin 
   (syncheck-example-eval
    '(require drracket/check-syntax racket/class
              setup/main-doc setup/collects)))
               

@title{DrRacket Tools}
@author{Robert Bruce Findler}

This manual describes portions of DrRacket's functionality
that are exposed via Racket APIs to be used with other editors.

@table-of-contents[]

@section{Accessing Check Syntax Programmatically}

@defmodule[drracket/check-syntax]

@defproc[(show-content [file-or-stx (or/c path-string?
                                          (and/c syntax?
                                                 (λ (x) (path-string? (syntax-source x)))))])
         (listof vector?)]{

This procedure composes the other pieces of this library together in a way that can be used
for REPL-style experimentation with the results from Check Syntax, as shown in the example
below. The list it returns has one vector for each call that would be made to the
object in @racket[current-annotations]. Each vector's first element is a symbol naming
a method in @racket[syncheck-annotations<%>] and the other elements of the vector are
the arguments passed to the method.

This doesn't work as well for use in a real tool, however, because it doesn't account for
the callback procedures present in @method[syncheck-annotations<%> syncheck:add-arrow/name-dup]
and @method[syncheck-annotations<%> syncheck:add-id-set] and the resulting vectors are probably less
convenient to work with than direct method calls for most uses of this library. Nevertheless,
it gives a quick feel for the results that can come back from Check Syntax.

See @racket[annotations-mixin] for some example code to use the other parts of this library.

Note that the paths in the example below have been replaced via @racket[make-paths-be-module-paths]
in order to make the results be platform independent.

  @interaction[#:eval
               syncheck-example-eval
               
               (define (make-paths-be-module-paths x)
                 (let loop ([x x])
                   (cond
                     [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
                     [(vector? x) (for/vector ([x (in-vector x)])
                                    (loop x))]
                     [(path? x)
                      (define p (path->collects-relative x))
                      (if (path? p)
                          (path->main-doc-relative x)
                          p)]
                     [else x])))
               
               (let ([example-module
                      '(module m racket (λ (x) x))])
                 (make-paths-be-module-paths
                  (show-content 
                   (read-syntax
                    (build-path (current-directory) "dummy-file.rkt")
                    (open-input-string (format "~s" example-module))))))]
}

@defproc[(make-traversal [namespace namespace?]
                         [path (or/c #f path-string?)])
         (values (->* (syntax?)
                      ((-> any/c void?))
                      void?)
                 (-> void?))]{
  This function creates some local state about a traversal of syntax objects
  and returns two functions. The first one should be called with each of the
  (fully expanded) syntax objects that make up a program (there will be only
  one if the program is a module) and then the second one should be called to
  indicate there are no more. 
  
  The optional argument to the first function is ignored.
  It is left there for historical reasons. In the past it
  was called for each sequence
  of binding identifiers encountered in @racket[define-values], @racket[define-syntaxes],
  and @racket[define-values-for-syntax].
  
  During the dynamic extent of the call to the two result functions, the value
  of the @racket[current-annotations] parameter is consulted and various
  methods are invoked in the corresponding object (if any), to indicate
  what has been found in the syntax object. These methods will only be called
  if the syntax objects have source locations.
}

@defparam[current-annotations ca (or/c #f (is-a?/c syncheck-annotations<%>))]{
  The methods of the value of this parameter are invoked by the functions returned
  from @racket[make-traversal]. 
}

@defparam[current-max-to-send-at-once m (or/c +inf.0 (and/c exact-integer? (>=/c 2)))]{
 No longer used.
}

@definterface[syncheck-annotations<%> ()]{

  Classes implementing this interface are
  accceptors of information about a traversal
  of syntax objects. See @racket[make-traversal].
  
  Do not implement this interface directly, as it
  is liable to change without warning. Instead, use
  the @racket[annotations-mixin] and override
  the methods you're interested in. The
  @racket[annotations-mixin] will keep in sync
  with this interface, providing methods that 
  ignore their arguments. 

  @defmethod[(syncheck:find-source-object [stx syntax?]) (or/c #f (not/c #f))]{
    This should return @racket[#f] if the source of this syntax object is
    uninteresting for annotations (if, for example, the only interesting
    annotations are those in the original file and this is a syntax object
    introduced by a macro and thus has a source location from some other file).
    
    Otherwise, it should return some (non-@racket[#f])
    value that will then be passed to one of the other methods below as 
    a @racket[_source-obj] argument.
  }
  
 @defmethod[(syncheck:add-background-color [source-obj (not/c #f)] 
                                           [start exact-nonnegative-integer?]
                                           [end exact-nonnegative-integer?]
                                           [color string?])
            void?]{
   Called to indicate that the color @racket[color] should be drawn on the background of 
   the given range in the editor, when the mouse moves over it. This method is typically
   called in conjuction with some other method that provides some other annotation
   on the source.
 }
 @defmethod[(syncheck:add-require-open-menu [source-obj (not/c #f)]
                                            [start exact-nonnegative-integer?]
                                            [end exact-nonnegative-integer?]
                                            [file path-string?])
            void?]{
   Called to indicate that there is a @racket[require] at the location from 
                                      @racket[start] to @racket[end],
   and that it corresponds to @racket[file]. Check Syntax adds a popup menu.
 }

 @defmethod[(syncheck:add-docs-menu [source-obj (not/c #f)]
                                    [start exact-nonnegative-integer?]
                                    [end exact-nonnegative-integer?]
                                    [id symbol?]
                                    [label any/c]
                                    [path any/c]
                                    [tag any/c])
            void?]{
   Called to indicate that there is something that has documentation between the range
   @racket[start] and @racket[end]. The documented identifier's name is given by @racket[id]
   and the docs are found in the html file @racket[path] at the html tag @racket[tag].
   The @racket[label] argument describes the binding for use in the menu item (although it may
   be longer than 200 characters).
 }
                  
 @defmethod[(syncheck:add-id-set [all-ids (listof (list/c (not/c #f) 
                                                          exact-nonnegative-integer?
                                                          exact-nonnegative-integer?))]
                                 [new-name-interferes? (-> symbol boolean?)])
            void?]{This method is no longer called by Check Syntax. It is here
                   for backwards compatibility only. The information it provided
                   must now be synthesized from the information supplied to
                   @method[syncheck-annotations<%> syncheck:add-arrow/name-dup].}
           
 @defmethod[(syncheck:add-arrow [start-source-obj (not/c #f)]
                                [start-left exact-nonnegative-integer?]
                                [start-right exact-nonnegative-integer?]
                                [end-source-obj (not/c #f)]
                                [end-left exact-nonnegative-integer?]
                                [end-right exact-nonnegative-integer?]
                                [actual? boolean?]
                                [phase-level (or/c exact-nonnegative-integer? #f)])
            void?]{
    This function is not called directly anymore by Check Syntax. Instead
    @method[syncheck-annotations<%> syncheck:add-arrow/name-dup] is.
    
    This method is invoked by the default implementation of 
    @racket[_syncheck:add-arrow/name-dup] in 
    @racket[annotations-mixin].
 }
 @defmethod[(syncheck:add-arrow/name-dup [start-source-obj (not/c #f)]
                                         [start-left exact-nonnegative-integer?]
                                         [start-right exact-nonnegative-integer?]
                                         [end-source-obj (not/c #f)]
                                         [end-left exact-nonnegative-integer?]
                                         [end-right exact-nonnegative-integer?]
                                         [actual? boolean?]
                                         [phase-level (or/c exact-nonnegative-integer? #f)]
                                         [require-arrow? boolean?]
                                         [name-dup? (-> string? boolean?)])
            void?]{
   Called to indicate that there should be an arrow between the locations described by the first 
   six arguments.
   
   The @racket[phase-level] argument indicates the phase of the binding and the 
   @racket[actual?] argument indicates if the binding is a real one, or a predicted one from
   a syntax template (predicted bindings are drawn with question marks in Check Syntax). 
   
   The @racket[require-arrow?] argument indicates if this arrow points from
   an imported identifier to its corresponding @racket[require].
   
   The @racket[name-dup?] predicate returns @racket[#t]
   in case that this variable (either the start or end), when replaced with the given string, would
   shadow some other binding (or otherwise interfere with the binding structure of the program at
   the time the program was expanded).
 }
 @defmethod[(syncheck:add-tail-arrow [from-source-obj (not/c #f)]
                                     [from-pos exact-nonnegative-integer?]
                                     [to-source-obj (not/c #f)]
                                     [to-pos exact-nonnegative-integer?])
            void?]{
   Called to indicate that there are two expressions, beginning at 
   @racket[from-pos] and @racket[to-pos]
   that are in tail position with respect to each other.
 }
 @defmethod[(syncheck:add-mouse-over-status [source-obj (not/c #f)]
                                            [pos-left exact-nonnegative-integer?]
                                            [pos-right exact-nonnegative-integer?]
                                            [str string?])
            void?]{
   Called to indicate that the message in @racket[str] should be shown when the mouse 
                                          passes over the given position.
 }
 @defmethod[(syncheck:add-jump-to-definition [source-obj (not/c #f)] 
                                             [start exact-nonnegative-integer?]
                                             [end exact-nonnegative-integer?]
                                             [id any/c]
                                             [filename path-string?]
                                             [submods (listof symbol?)])
            void?]{
   Called to indicate that there is some identifier at the given location (named @racket[id]) that
   is defined in the @racket[submods] of the file @racket[filename] (where an empty list in
   @racket[submods] means that the identifier is defined at the top-level module).
 }
                  
 @defmethod[(syncheck:add-definition-target [source-obj (not/c #f)]
                                            [start exact-nonnegative-integer?]
                                            [finish exact-nonnegative-integer?]
                                            [style-name any/c]) void?]{
     
  }
                                                      
 @defmethod[(syncheck:color-range [source-obj (not/c #f)]
                                  [start exact-nonnegative-integer?]
                                  [finish exact-nonnegative-integer?]
                                  [style-name any/c]
                                  [mode any/c])
            void?]{
   Called to indicate that the given location should be colored according to the
   style @racket[style-name] when in @racket[mode]. The mode either indicates regular
   check syntax or is used indicate blame for potential contract violations 
   (and still experimental).
 }
 @defmethod[(syncheck:add-rename-menu [id symbol?]
                                      [all-ids (listof (list/c (not/c #f) 
                                                               exact-nonnegative-integer?
                                                               exact-nonnegative-integer?))]
                                      [new-name-interferes? (-> symbol boolean?)])
            void?]{
    This method is listed only for backwards compatibility. It is not called
    by Check Syntax anymore.
  }
}

@defmixin[annotations-mixin () (syncheck-annotations<%>)]{
  Supplies all of the methods in @racket[syncheck-annotations<%>]
  with default behavior. Be sure to use this mixin to future-proof
  your code and then override the methods you're interested in.
  
  By default:
  @itemlist[@item{The @method[syncheck-annotations<%> syncheck:find-source-object] 
                      method ignores its arguments and returns @racket[#f];}
            @item{the @method[syncheck-annotations<%> syncheck:add-arrow/name-dup] method drops the
                      @racket[_require-arrow?] and @racket[_name-dup?] arguments and calls
                      @method[syncheck-annotations<%> syncheck:add-arrow]; and}
            @item{all of the other methods ignore their arguments and return @racket[(void)].}]
    
  Here is an example showing how use this library to extract all
  of the arrows that Check Syntax would draw from various
  expressions. One subtle point: arrows are only included when
  the corresponding identifiers are @racket[syntax-original?];
  the code below manages this by copying the properties from
  an identifier that is @racket[syntax-original?] in the
  call to @racket[datum->syntax].
  @interaction[#:eval
               syncheck-example-eval
               (define arrows-collector%
                 (class (annotations-mixin object%)
                   (super-new)
                   (define/override (syncheck:find-source-object stx)
                     stx)
                   (define/override (syncheck:add-arrow/name-dup
                                     start-source-obj start-left start-right
                                     end-source-obj end-left end-right
                                     actual? phase-level require-arrow? name-dup?)
                     (set! arrows
                           (cons (list start-source-obj end-source-obj)
                                 arrows)))
                   (define arrows '())
                   (define/public (get-collected-arrows) arrows)))
               (define (arrows form)
                 (define base-namespace (make-base-namespace))
                 (define-values (add-syntax done)
                   (make-traversal base-namespace #f))
                 (define collector (new arrows-collector%))
                 (parameterize ([current-annotations collector]
                                [current-namespace base-namespace])
                   (add-syntax (expand form))
                   (done))
                 (send collector get-collected-arrows))
               (define (make-id name pos orig?)
                 (datum->syntax
                  #f
                  name
                  (list #f #f #f pos (string-length (symbol->string name)))
                  (and orig? #'is-orig)))
               (arrows `(λ (,(make-id 'x 1 #t)) ,(make-id 'x 2 #t)))
               (arrows `(λ (x) x))
               (arrows `(λ (,(make-id 'x 1 #f)) ,(make-id 'x 2 #t)))
               (arrows `(λ (,(make-id 'x 1 #t)) x))]
}

@(define-syntax-rule 
   (syncheck-method-id x ...)
   (begin @defidform[x]{Bound to an identifier created with
                        @racket[define-local-member-name]
                        that is used in @racket[syncheck-annotations<%>].}
          ...))
@syncheck-method-id[syncheck:find-source-object
                    syncheck:add-background-color
                    syncheck:add-require-open-menu
                    syncheck:add-docs-menu
                    syncheck:add-rename-menu
                    syncheck:add-arrow
                    syncheck:add-arrow/name-dup
                    syncheck:add-tail-arrow
                    syncheck:add-mouse-over-status
                    syncheck:add-jump-to-definition
                    syncheck:add-id-set 
                    syncheck:color-range]

@section{Module Browser}

@defmodule[drracket/module-browser]

@defproc[(module-browser [path path-string?]) void?]{
  Opens a window containing the module browser for @racket[path].
}


@(close-eval syncheck-example-eval)
