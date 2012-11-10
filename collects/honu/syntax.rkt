#lang honu

/* Standard syntax-rules but as a macro-defining form */

provide macro_rules;
macro macro_rules(){
  name:identifier (literal ...){ pattern ... }{ template ... }
} {
  syntax(macro name (literal ...){ pattern ... }{ syntax(template ...) })
}
