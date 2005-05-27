
(module labels mzscheme

  ; XXX labels contain types and types contain labels, so we will need another layer for
  ; the contracts once the types are extracted from cgp.ss
  
  (provide
   (struct label (parents children type-var trace prim? term set edges)) 
   (struct label-cst (value))
   (struct label-cons (car cdr))
   (struct label-vector (element))
   (struct label-promise (value))
   (struct label-case-lambda (struct rest-arg?s req-args argss exps effects))
   (struct label-values (label))
   (struct label-struct-value (type fields))
   (struct label-struct-type (name parent parent-fields-nbr total-fields-nbr error?))
   )
  
  ; parents = children = (listof label)
  ; type-var = (union type-var #f), trace = boolean, prim? = boolean
  ; term = syntax-object, set = (hash-table-of label (make-arrows (listof label) (listof label) (listof label)),
  ; edges = (hashtableof symbol edge))
  ; a flow graph label/node type-var is a type variable name used when reconstructing recursive
  ; types.
  ; trace is used to detect cycles in the graph during type reconstruction.
  ; prim? tells whether the label was created during graph reconstruction from a primitive
  ; type. We need this to detect the entrance of a tunnel.
  ; Note that the only reason we need to have this tunneling stuff is to keep the GUI arrows right.
  ; term: the source program term (or a fake version of it, in the case of "null" when we have
  ; a rest argument)
  ; set: contains label structures (see below) for all the values that flow into this term.
  ; Each label in the set has two lists of in and out edges pointing back and forth to the nodes
  ; from which this label has flowed in (or '() if the label is the source of the label) and
  ; flowed out to. The in edges (which need to be checked each time a propagation is done, to 
  ; revent cycles) are in a list, and not in an hash-table, because we assume that the same
  ; label is not going to flow very often into this term through several paths. The out-edge
  ; list is only used to draw arrows, so it doesn't have to be implemented very efficiently.
  ; Note that, since constants are represented by label structs, the same constant can appear
  ; several times in the set, even symbols.
  ; edges: functions that take two labels as argument and either propagate the second one to
  ; another label, using the first label as the source, or transform the graph accordingly (if
  ; the inflowing label is a function pseudo-label and the label into which it flows corresponds
  ; to the operator in an application, for example).
  ; parent and children are used to memoize the parent and children arrows for all the values
  ; in the label's value set.  Computing these when the code contains huge amounts of macro-
  ; generated recurisve code is quite expensive.
  (define-struct label (parents children type-var trace prim? term set edges))
  
  ; a constant...
  (define-struct (label-cst label) (value))
  
  ; car = label, cdr = label
  (define-struct (label-cons label) (car cdr))
  
  (define-struct (label-vector label) (element))
  (define-struct (label-promise label) (value))
  
  ; struct = label-struct
  ; rest-arg?s = (listof boolean), req-args = (listof number), argss = (listof (listof label)),
  ; exps = (listof label), app-thunks = (listof (-> void))
  ; Each "level" of the six lists represents the args and body labels of a given clause in the
  ; case-lambda. At a given level, rest-arg? tells whether this clause has a rest argument,
  ; and req-args gives the number of required arguments, so it only has to be computed once.
  ; top-free-varss are the labels of the top level free variables in the corresponding clause.
  ; This field is updated as a side effect when analyzing top level variable references inside
  ; the body of a lambda. Edges flowing into these free variables must be created when the
  ; clause is applied. app-thunk is a thunk that is used to delay the transformation of the
  ; graph when a function flows into an application, until the clause around the application
  ; is itself applied. The two are merged, because one of the delayed app could set! a top level
  ; variable, and the top level variable can be referenced both before and after the application,
  ; so lookups and applications have to be done in exactly the right order.
  ; struct is just a placeholder to tell the type of structure a given structure-processing
  ; function is supposed to deal with.
  (define-struct (label-case-lambda label)
                 (struct rest-arg?s req-args argss exps effects))
  
  ; label = label (a label-cons based list of labels)
  ; used to simulate multiple values. So this label is going to flow around and work pretty
  ; much like a cons label. the problem is that multiple values are not first-class in Scheme,
  ; so we have to be careful to only propagate them through edges that correspond to the result
  ; of applications, never through edges that correspond to arguments of applications. Hence
  ; the reason for the complication in create-simple-edge. Note that define-struct expands
  ; into a define-values, so we need all that stuff.
  (define-struct (label-values label) (label))
  
  ; symbol symbol label number (listof label)
  (define-struct (label-struct-value label) (type fields))
  (define-struct (label-struct-type label)
                 (name parent parent-fields-nbr total-fields-nbr error?))
  
  )
