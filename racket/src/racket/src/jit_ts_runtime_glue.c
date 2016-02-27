case SIG_siS_s:
  {
     prim_siS_s f = (prim_siS_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(int, arg_i1); JIT_TS_LOCALIZE(Scheme_Object**, arg_S2);
     
     future->arg_s0 = NULL; future->arg_S2 = NULL;
     ADJUST_RS_ARG(future, arg_S2);
     retval = 
     f(arg_s0, arg_i1, arg_S2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_iSs_s:
  {
     prim_iSs_s f = (prim_iSs_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(Scheme_Object**, arg_S1); JIT_TS_LOCALIZE(Scheme_Object*, arg_s2);
     
     future->arg_S1 = NULL; future->arg_s2 = NULL;
     ADJUST_RS_ARG(future, arg_S1);
     retval = 
     f(arg_i0, arg_S1, arg_s2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_s_s:
  {
     prim_s_s f = (prim_s_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0);
     receive_special_result(future, future->arg_s0, 1);
     future->arg_s0 = NULL;
     
     retval = 
     f(arg_s0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_n_s:
  {
     prim_n_s f = (prim_n_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Native_Lambda*, arg_n0);
     
     future->arg_n0 = NULL;
     
     retval = 
     f(arg_n0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG__s:
  {
     prim__s f = (prim__s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     
     
     
     
     retval = 
     f();
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_ss_s:
  {
     prim_ss_s f = (prim_ss_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL;
     
     retval = 
     f(arg_s0, arg_s1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_ssi_s:
  {
     prim_ssi_s f = (prim_ssi_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1); JIT_TS_LOCALIZE(int, arg_i2);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL;
     
     retval = 
     f(arg_s0, arg_s1, arg_i2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_tt_s:
  {
     prim_tt_s f = (prim_tt_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(const Scheme_Object*, arg_t0); JIT_TS_LOCALIZE(const Scheme_Object*, arg_t1);
     
     future->arg_t0 = NULL; future->arg_t1 = NULL;
     
     retval = 
     f(arg_t0, arg_t1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_ss_m:
  {
     prim_ss_m f = (prim_ss_m)future->prim_func;
     GC_CAN_IGNORE MZ_MARK_STACK_TYPE retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL;
     
     retval = 
     f(arg_s0, arg_s1);
     future->retval_m = retval;
     
     break;
  }
case SIG_Sl_s:
  {
     prim_Sl_s f = (prim_Sl_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object**, arg_S0); JIT_TS_LOCALIZE(intptr_t, arg_l1);
     
     future->arg_S0 = NULL;
     ADJUST_RS_ARG(future, arg_S0);
     retval = 
     f(arg_S0, arg_l1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_l_s:
  {
     prim_l_s f = (prim_l_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(intptr_t, arg_l0);
     
     
     
     retval = 
     f(arg_l0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_bsi_v:
  {
     prim_bsi_v f = (prim_bsi_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Bucket*, arg_b0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1); JIT_TS_LOCALIZE(int, arg_i2);
     
     future->arg_b0 = NULL; future->arg_s1 = NULL;
     
     
     f(arg_b0, arg_s1, arg_i2);
     
     
     break;
  }
case SIG_iiS_v:
  {
     prim_iiS_v f = (prim_iiS_v)future->prim_func;
     
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(int, arg_i1); JIT_TS_LOCALIZE(Scheme_Object**, arg_S2);
     
     future->arg_S2 = NULL;
     ADJUST_RS_ARG(future, arg_S2);
     
     f(arg_i0, arg_i1, arg_S2);
     
     
     break;
  }
case SIG_ss_v:
  {
     prim_ss_v f = (prim_ss_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL;
     
     
     f(arg_s0, arg_s1);
     
     
     break;
  }
case SIG_b_v:
  {
     prim_b_v f = (prim_b_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Bucket*, arg_b0);
     
     future->arg_b0 = NULL;
     
     
     f(arg_b0);
     
     
     break;
  }
case SIG_sl_s:
  {
     prim_sl_s f = (prim_sl_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(intptr_t, arg_l1);
     
     future->arg_s0 = NULL;
     
     retval = 
     f(arg_s0, arg_l1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_iS_s:
  {
     prim_iS_s f = (prim_iS_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(Scheme_Object**, arg_S1);
     
     future->arg_S1 = NULL;
     ADJUST_RS_ARG(future, arg_S1);
     retval = 
     f(arg_i0, arg_S1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_S_s:
  {
     prim_S_s f = (prim_S_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object**, arg_S0);
     
     future->arg_S0 = NULL;
     ADJUST_RS_ARG(future, arg_S0);
     retval = 
     f(arg_S0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_s_v:
  {
     prim_s_v f = (prim_s_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0);
     receive_special_result(future, future->arg_s0, 1);
     future->arg_s0 = NULL;
     
     
     f(arg_s0);
     
     
     break;
  }
case SIG_iSi_s:
  {
     prim_iSi_s f = (prim_iSi_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(Scheme_Object**, arg_S1); JIT_TS_LOCALIZE(int, arg_i2);
     
     future->arg_S1 = NULL;
     ADJUST_RS_ARG(future, arg_S1);
     retval = 
     f(arg_i0, arg_S1, arg_i2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_siS_v:
  {
     prim_siS_v f = (prim_siS_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(int, arg_i1); JIT_TS_LOCALIZE(Scheme_Object**, arg_S2);
     
     future->arg_s0 = NULL; future->arg_S2 = NULL;
     ADJUST_RS_ARG(future, arg_S2);
     
     f(arg_s0, arg_i1, arg_S2);
     
     
     break;
  }
case SIG_z_p:
  {
     prim_z_p f = (prim_z_p)future->prim_func;
     GC_CAN_IGNORE void* retval;
     JIT_TS_LOCALIZE(size_t, arg_z0);
     
     
     
     retval = 
     f(arg_z0);
     future->retval_p = retval;
     
     break;
  }
case SIG_si_s:
  {
     prim_si_s f = (prim_si_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(int, arg_i1);
     
     future->arg_s0 = NULL;
     
     retval = 
     f(arg_s0, arg_i1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_sis_v:
  {
     prim_sis_v f = (prim_sis_v)future->prim_func;
     
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(int, arg_i1); JIT_TS_LOCALIZE(Scheme_Object*, arg_s2);
     
     future->arg_s0 = NULL; future->arg_s2 = NULL;
     
     
     f(arg_s0, arg_i1, arg_s2);
     
     
     break;
  }
case SIG_ss_i:
  {
     prim_ss_i f = (prim_ss_i)future->prim_func;
     GC_CAN_IGNORE int retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL;
     
     retval = 
     f(arg_s0, arg_s1);
     future->retval_i = retval;
     
     break;
  }
case SIG_iSp_v:
  {
     prim_iSp_v f = (prim_iSp_v)future->prim_func;
     
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(Scheme_Object**, arg_S1); JIT_TS_LOCALIZE(void*, arg_p2);
     
     future->arg_S1 = NULL; future->arg_p2 = NULL;
     ADJUST_RS_ARG(future, arg_S1);
     
     f(arg_i0, arg_S1, arg_p2);
     
     
     break;
  }
case SIG_sss_s:
  {
     prim_sss_s f = (prim_sss_s)future->prim_func;
     GC_CAN_IGNORE Scheme_Object* retval;
     JIT_TS_LOCALIZE(Scheme_Object*, arg_s0); JIT_TS_LOCALIZE(Scheme_Object*, arg_s1); JIT_TS_LOCALIZE(Scheme_Object*, arg_s2);
     
     future->arg_s0 = NULL; future->arg_s1 = NULL; future->arg_s2 = NULL;
     
     retval = 
     f(arg_s0, arg_s1, arg_s2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG__v:
  {
     prim__v f = (prim__v)future->prim_func;
     
     
     
     
     
     
     f();
     
     
     break;
  }
case SIG_iS_v:
  {
     prim_iS_v f = (prim_iS_v)future->prim_func;
     
     JIT_TS_LOCALIZE(int, arg_i0); JIT_TS_LOCALIZE(Scheme_Object**, arg_S1);
     
     future->arg_S1 = NULL;
     ADJUST_RS_ARG(future, arg_S1);
     
     f(arg_i0, arg_S1);
     
     
     break;
  }
