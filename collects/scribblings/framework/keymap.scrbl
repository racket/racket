#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Keymap}

@definterface[keymap:aug-keymap<%> (keymap%)]{
  This keymap overrides some of the built in \iscmclass{keymap} methods
  to be able to extract the keybindings from the keymap.
  @defmethod*[(((get-chained-keymaps) (listof (instance keymap%))))]{

    Returns the list of keymaps that are chained to this one.
  }
  @defmethod*[(((get-map-function-table) hash-table))]{

    Returns a hash-table that maps symbols naming key sequences to the
    names of the keymap functions the are bound to.
  }
  @defmethod*[(((get-map-function-table/ht (ht hash-table)) hash-table))]{

    This is a helper function for 
    @method[keymap:aug-keymap<%> get-map-function-table]
    that returns the same result, except it accepts a hash-table that
    it inserts the bindings into. It does not replace any bindings already in 
    \var{ht}.
  }
}
@defmixin[keymap:aug-keymap-mixin (keymap%) (keymap:aug-keymap<%>)]{
  @defmethod*[#:mode override (((chain-to-keymap (next (instance keymap%)) (prefix? boolean)) void))]{

    Keeps a list of the keymaps chained to this one.
  }
  @defmethod*[#:mode override (((remove-chained-keymap (keymap (is-a?/c keymap))) void))]{

    Keeps the list of the keymaps chained to this one up to date.
  }
  @defmethod*[#:mode override (((map-function (key-name string) (function-name string)) void))]{

    Keeps a separate record of the key names and functions that they are
    bound to in this keymap.
  }
}
@defclass[keymap:aug-keymap% (keymap:aug-keymap-mixin keymap%) ()]{}
@(require framework/framework-docs)
@(def-fw-procs keymap)
