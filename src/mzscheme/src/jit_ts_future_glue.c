 Scheme_Object* scheme_rtcall_siS_s(prim_siS_s f, Scheme_Object* g43, int g44, Scheme_Object** g45)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_siS_s;
  future->prim_func = f;
      future->arg_s0 = g43;
    future->arg_i1 = g44;
    future->arg_S2 = g45;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iSs_s(prim_iSs_s f, int g46, Scheme_Object** g47, Scheme_Object* g48)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iSs_s;
  future->prim_func = f;
      future->arg_i0 = g46;
    future->arg_S1 = g47;
    future->arg_s2 = g48;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_s_s(prim_s_s f, Scheme_Object* g49)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_s_s;
  future->prim_func = f;
      future->arg_s0 = g49;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_n_s(prim_n_s f, Scheme_Native_Closure_Data* g50)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_n_s;
  future->prim_func = f;
      future->arg_n0 = g50;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall__s(prim__s f )
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG__s;
  future->prim_func = f;
  
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_ss_s(prim_ss_s f, Scheme_Object* g51, Scheme_Object* g52)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_ss_s;
  future->prim_func = f;
      future->arg_s0 = g51;
    future->arg_s1 = g52;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(prim_ss_m f, Scheme_Object* g53, Scheme_Object* g54)
{
  START_XFORM_SKIP;
  future_t *future;
  MZ_MARK_STACK_TYPE retval;

  future = current_ft;
  future->prim_protocol = SIG_ss_m;
  future->prim_func = f;
      future->arg_s0 = g53;
    future->arg_s1 = g54;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_m;
  future->retval_m = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_Sl_s(prim_Sl_s f, Scheme_Object** g55, long g56)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_Sl_s;
  future->prim_func = f;
      future->arg_S0 = g55;
    future->arg_l1 = g56;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_l_s(prim_l_s f, long g57)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_l_s;
  future->prim_func = f;
      future->arg_l0 = g57;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_bsi_v(prim_bsi_v f, Scheme_Bucket* g58, Scheme_Object* g59, int g60)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_bsi_v;
  future->prim_func = f;
      future->arg_b0 = g58;
    future->arg_s1 = g59;
    future->arg_i2 = g60;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_iiS_v(prim_iiS_v f, int g61, int g62, Scheme_Object** g63)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_iiS_v;
  future->prim_func = f;
      future->arg_i0 = g61;
    future->arg_i1 = g62;
    future->arg_S2 = g63;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_ss_v(prim_ss_v f, Scheme_Object* g64, Scheme_Object* g65)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_ss_v;
  future->prim_func = f;
      future->arg_s0 = g64;
    future->arg_s1 = g65;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_b_v(prim_b_v f, Scheme_Bucket* g66)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_b_v;
  future->prim_func = f;
      future->arg_b0 = g66;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_sl_s(prim_sl_s f, Scheme_Object* g67, long g68)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_sl_s;
  future->prim_func = f;
      future->arg_s0 = g67;
    future->arg_l1 = g68;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iS_s(prim_iS_s f, int g69, Scheme_Object** g70)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iS_s;
  future->prim_func = f;
      future->arg_i0 = g69;
    future->arg_S1 = g70;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_S_s(prim_S_s f, Scheme_Object** g71)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_S_s;
  future->prim_func = f;
      future->arg_S0 = g71;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_s_v(prim_s_v f, Scheme_Object* g72)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_s_v;
  future->prim_func = f;
      future->arg_s0 = g72;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iSi_s(prim_iSi_s f, int g73, Scheme_Object** g74, int g75)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iSi_s;
  future->prim_func = f;
      future->arg_i0 = g73;
    future->arg_S1 = g74;
    future->arg_i2 = g75;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = NULL;
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_siS_v(prim_siS_v f, Scheme_Object* g76, int g77, Scheme_Object** g78)
{
  START_XFORM_SKIP;
  future_t *future;
  

  future = current_ft;
  future->prim_protocol = SIG_siS_v;
  future->prim_func = f;
      future->arg_s0 = g76;
    future->arg_i1 = g77;
    future->arg_S2 = g78;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  END_XFORM_SKIP;
}
