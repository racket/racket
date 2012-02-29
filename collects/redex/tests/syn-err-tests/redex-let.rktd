(#rx"redex-let: duplicate pattern variable"
 ([dup number])
 (redex-let syn-err-lang ([(dup) 1] [dup 1]) (term dup)))
