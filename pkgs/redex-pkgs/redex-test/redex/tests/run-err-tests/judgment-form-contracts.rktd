(#rx"contract: \\(ctc-fail s s\\).*values: \\(ctc-fail q _\\)"
 ([judgment (ctc-fail q s)])
 (judgment-holds judgment))
(#rx"contract: \\(ctc-fail s s\\).*values: \\(ctc-fail a q\\)"
 ([judgment (ctc-fail a s)])
 (judgment-holds judgment))
(#rx"contract: \\(ctc-fail s s\\).*values: \\(ctc-fail q _\\)"
 ([judgment (ctc-fail b s)])
 (judgment-holds judgment))
(#rx"contract: \\(ctc-fail s s\\).*values: \\(ctc-fail a q\\)"
 ([judgment (ctc-fail c s)])
 (judgment-holds judgment))
(#rx"contract: \\(inv-fail s_1 s_2\\).*values: \\(inv-fail a a\\)"
 ([judgment (inv-fail a s)])
 (judgment-holds judgment))
