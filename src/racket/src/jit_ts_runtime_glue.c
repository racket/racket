case SIG_siS_s:
  {
     prim_siS_s f = (prim_siS_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_s0, future->arg_i1, future->arg_S2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_iSs_s:
  {
     prim_iSs_s f = (prim_iSs_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_i0, future->arg_S1, future->arg_s2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_s_s:
  {
     prim_s_s f = (prim_s_s)future->prim_func;
     Scheme_Object* retval;
     receive_special_result(future, future->arg_s0, 1);
     retval = 
     f(future->arg_s0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_n_s:
  {
     prim_n_s f = (prim_n_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_n0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG__s:
  {
     prim__s f = (prim__s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f();
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_ss_s:
  {
     prim_ss_s f = (prim_ss_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_s0, future->arg_s1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_ss_m:
  {
     prim_ss_m f = (prim_ss_m)future->prim_func;
     MZ_MARK_STACK_TYPE retval;
     
     retval = 
     f(future->arg_s0, future->arg_s1);
     future->retval_m = retval;
     
     break;
  }
case SIG_Sl_s:
  {
     prim_Sl_s f = (prim_Sl_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_S0, future->arg_l1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_l_s:
  {
     prim_l_s f = (prim_l_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_l0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_bsi_v:
  {
     prim_bsi_v f = (prim_bsi_v)future->prim_func;
     
     
     
     f(future->arg_b0, future->arg_s1, future->arg_i2);
     
     
     break;
  }
case SIG_iiS_v:
  {
     prim_iiS_v f = (prim_iiS_v)future->prim_func;
     
     
     
     f(future->arg_i0, future->arg_i1, future->arg_S2);
     
     
     break;
  }
case SIG_ss_v:
  {
     prim_ss_v f = (prim_ss_v)future->prim_func;
     
     
     
     f(future->arg_s0, future->arg_s1);
     
     
     break;
  }
case SIG_b_v:
  {
     prim_b_v f = (prim_b_v)future->prim_func;
     
     
     
     f(future->arg_b0);
     
     
     break;
  }
case SIG_sl_s:
  {
     prim_sl_s f = (prim_sl_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_s0, future->arg_l1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_iS_s:
  {
     prim_iS_s f = (prim_iS_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_i0, future->arg_S1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_S_s:
  {
     prim_S_s f = (prim_S_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_S0);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_s_v:
  {
     prim_s_v f = (prim_s_v)future->prim_func;
     
     receive_special_result(future, future->arg_s0, 1);
     
     f(future->arg_s0);
     
     
     break;
  }
case SIG_iSi_s:
  {
     prim_iSi_s f = (prim_iSi_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_i0, future->arg_S1, future->arg_i2);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_siS_v:
  {
     prim_siS_v f = (prim_siS_v)future->prim_func;
     
     
     
     f(future->arg_s0, future->arg_i1, future->arg_S2);
     
     
     break;
  }
case SIG_z_p:
  {
     prim_z_p f = (prim_z_p)future->prim_func;
     void* retval;
     
     retval = 
     f(future->arg_z0);
     future->retval_p = retval;
     
     break;
  }
case SIG_si_s:
  {
     prim_si_s f = (prim_si_s)future->prim_func;
     Scheme_Object* retval;
     
     retval = 
     f(future->arg_s0, future->arg_i1);
     future->retval_s = retval;
     send_special_result(future, retval);
     break;
  }
case SIG_sis_v:
  {
     prim_sis_v f = (prim_sis_v)future->prim_func;
     
     
     
     f(future->arg_s0, future->arg_i1, future->arg_s2);
     
     
     break;
  }
