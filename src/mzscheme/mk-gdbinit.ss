#lang scheme/base

;; This script generates "mz-gdbinit" to the current directory.
;; It's normally run via `make mz-gdbinit', in which case
;; putting
;;   source mz-gdbinit
;; in your "~/.gdbinit" file causes it to be loaded. 

;; The "mz-gdbinit" file is generated so that it can use specific
;; type-tag constants from "stypes.h", in case the debugging info
;; can't see them. If "stypes.h" changes, then "mz-gdbinit" needs
;; to be re-built.

(require scheme/runtime-path)

(define-runtime-path stypes-path "src/stypes.h")

(define template #<<EOS
define pso
  psox $arg0 0
end
document pso
Print Scheme Object
end

define indent
  set $i = $arg0
  while $i > 0
    printf " "
    set $i = $i - 1
  end
end

define psox
  set $O = ((Scheme_Object*) ($arg0))
  indent $arg1
  if (((int)$arg0) & 0x1)
    set $OT = <<scheme_integer_type>>
  else
    set $OT = $O->type
  end
  printf "Scheme_Object %p type=%d\n", $O, $OT
  psoq $O $arg1
end

define psoq
indent $arg1
if (((int)$arg0) & 0x1)
  printf "scheme_integer_type %d", (((int)$arg0) >> 1)
else
  set $O = ((Scheme_Object*) ($arg0))
  set $OT = $O->type
  if ( $OT == <<scheme_toplevel_type>> )
    set $TL = ((Scheme_Toplevel*) ($O))
    printf "scheme_toplevel_type depth=%d position=%d", $TL->depth, $TL->position
  end
  if ( $OT == <<scheme_syntax_type>> )
    set $SSO = ((Scheme_Simple_Object*) ($O))
    set $index = $SSO->u.ptr_int_val.pint
    set $object = (Scheme_Object *) $SSO->u.ptr_int_val.ptr
    printf "scheme_syntax_type index=%d\n", $index
    psox $object $arg1+1
  end
  if ( $OT == <<scheme_application2_type>> )
    printf "scheme_application2_type\n"
    set $AP = ((Scheme_App2_Rec*) ($O))
    set $RATOR = $AP->rator
    set $RAND = $AP->rand
    indent $arg1
    printf "rator="
    psox $RATOR $arg1+1
    indent $arg1
    printf "rand="
    psox $RAND $arg1+1
  end
  if ( $OT == <<scheme_sequence_type>> )
    printf "scheme_sequence\n"
    set $seq = ((Scheme_Sequence *) $O)
    p *$seq
    set $size = $seq->count
    set $cnt = 0
    while ( $cnt < $size ) 
      p $cnt
      p $seq->array[$cnt]
      psox $seq->array[$cnt] $arg1+1
      set $cnt++
    end
    $OT = 0
  end
  if ( $OT == <<scheme_prim_type>> )
    printf "scheme_prim_type\n"
    set $pproc = ((Scheme_Primitive_Proc *) $O)
    p *$pproc
  end
  if ( $OT == <<scheme_closure_type>> )
    printf "scheme_closure_type\n"
    set $closure = ((Scheme_Closure *) $O)
    p *$closure
    set $code = $closure->code
    printf "scheme_closure_type code1\n"
    p *$code
    set $name = $code->name
    p *$name
    #psox $name $arg+1
    printf "scheme_closure_type code2\n"
    psox $code->code $arg+1
    $OT = <<scheme_closure_type>>
  end
  if ( $OT == <<scheme_structure_type>>)
    printf "scheme_structure_type\n"
  end
  if ( $OT == <<scheme_unix_path_type>>)
    printf "scheme_unix_path_type "
    p (char *)((Scheme_Simple_Object *)$O)->u.byte_str_val.string_val
  end
  if ( $OT == <<scheme_symbol_type>> )
    printf "scheme_symbol_type %s", (char *)((Scheme_Symbol*) $O)->s
  end
  if ( $OT == <<scheme_null_type>> )
    printf "scheme_null"
  end
  if ( $OT == <<scheme_pair_type>> )
    printf "scheme_pair\n"
    set $SSO = ((Scheme_Simple_Object*) ($O))
    set $CAR = $SSO->u.pair_val.car
    set $CDR = $SSO->u.pair_val.cdr
    indent $arg1
    printf "car=\n"
    psox $CAR $arg1+1
    indent $arg1
    printf "cdr=\n"
    psox $CDR $arg1+1
  end
  if ( $OT == <<scheme_vector_type>> )
    set $vector = ((struct Scheme_Vector *) $O)
    set $size = $vector->size
    printf "scheme_vector_type size=%d\n", $size
    set $cnt = 0
    while ( $cnt < $size ) 
      p $cnt
      psox $vector->els[$cnt] $arg1+1
      set $cnt++
    end
  end
  if ( $OT == <<scheme_true_type>> )
    printf "scheme_true"
  end
  if ( $OT == <<scheme_false_type>> )
    printf "scheme_false"
  end
  if ( $OT == <<scheme_void_type>> )
    printf "scheme_void"
  end
  if ( $OT == <<scheme_hash_table_type>> )
    psht $O $arg1
  end
  if ( $OT == <<scheme_module_index_type>> )
    printf "scheme_module_index_type\n"
    set $modidx = ((Scheme_Modidx *) $O)
    indent $arg1
    printf "path="
    psox $modidx->path $arg1+1
    indent $arg1
    printf "base="
    psox $modidx->base $arg1+1
    indent $arg1
    printf "resolved="
    psox $modidx->resolved $arg1+1
  end
  if ( $OT == <<scheme_namespace_type>>)
    printf "scheme_namespace_type\n"
    set $env = ((Scheme_Env*)$O)
    if ($env->module != 0)
      psox $env->module $arg1+1
    else
      indent $arg1
      printf "top-level\n"
    end
  end
  if ( $OT == <<scheme_stx_type>> )
    printf "scheme_stx_type\n"
    set $stx = ((Scheme_Stx*) $O)
    p *$stx
    indent $arg1
    printf "content="
    psox $stx->val $arg1+1
    set $srcloc = $stx->srcloc
    set $name = ($stx->srcloc->src)
    set $name = (char *)((Scheme_Simple_Object *)$name)->u.byte_str_val.string_val
    indent $arg1
    printf "%s:%i:%i\n", $name, $srcloc->line, $srcloc->col
  end
  if ( $OT == <<scheme_compilation_top_type>>)
    printf "scheme_compilation_top_type\n"
    set $top = ((Scheme_Compilation_Top*)$O)
    p *$top
    psox $top->code $arg1+1
  end
  if ( $OT == <<scheme_module_type>>)
    printf "scheme_module_type\n"
    set $module = ((Scheme_Module*)$O)
    psox $module->modname $arg1+1
  end
  if ( $OT == <<scheme_resolved_module_path_type>>)
    set $OO = (((Scheme_Small_Object *)$arg0)->u.ptr_val)
    printf "scheme_resolved_module_path_type %s %p", (char *)((Scheme_Symbol*) $OO)->s, $OO
  end
end
printf "\n"
end
document psoq
print scheme object quiet
end

define psht
    set $ht= ((struct Scheme_Hash_Table *) $arg0)
    set $size = $ht->size
    printf "scheme_hash_table_type size=%d count=%d\n", $size, $ht->count
    set $cnt = 0
    while ( $cnt < $size ) 
      set $item = $ht->vals[$cnt]
      if ($item != 0)
        set $item = $ht->keys[$cnt]
        indent $arg1
        printf "key=\n"
        psox $item $arg1+1
        set $item = $ht->vals[$cnt]
        indent $arg1
        printf "val=\n"
        psox $item $arg1+1
      end
      set $cnt++
    end
end
document psht
print scheme hash table
end
EOS
)

(define styles (with-input-from-file stypes-path
                 (lambda () (read-string (* 2 (file-size stypes-path))))))

(call-with-output-file* "mz-gdbinit"
  #:exists 'truncate 
  (lambda (out)
    (let ([in (open-input-string template)])
      (let loop ()
        (let ([m (regexp-match #rx"<<([^>]*)>>" in 0 #f out)])
          (when m
            (let ([m2 (regexp-match (format "~a, */[*] ([0-9]+) [*]/" (cadr m))
                                    styles)])
              (if m2
                  (display (cadr m2) out)
                  (error 'mk-gdbinit "cannot find type in stypes.h: ~e" (cadr m))))
            (loop)))))
    (newline out)))
