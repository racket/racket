(module contract-forms mzscheme
  
  (require "contracts/contracts-module-begin.ss"
	   "contracts/contracts.ss"
	   "contracts/define-data.ss")
  
  (provide beginner-contract 
           beginner-define-data 
           beginner-module-begin

	   intermediate-contract 
           intermediate-define-data 
           intermediate-module-begin

	   advanced-contract 
           advanced-define-data 
           advanced-module-begin))
