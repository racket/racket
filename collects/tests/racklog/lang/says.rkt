#lang racklog
tpme(tpme1).
ms(m1,'TPME',tpme1,ek,tp).
says(TPME,M) :- tpme(TPME),ms(M,'TPME',TPME,A,B).
says(A,B)?
