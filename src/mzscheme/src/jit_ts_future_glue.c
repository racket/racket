 Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g44, int g45, Scheme_Object** g46)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_siS_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g44;
    future->arg_i1 = g45;
    future->arg_S2 = g46;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g47, Scheme_Object** g48, Scheme_Object* g49)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iSs_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g47;
    future->arg_S1 = g48;
    future->arg_s2 = g49;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g50)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_s_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g50;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g51)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_n_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_n0 = g51;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f )
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG__s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
  
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g52, Scheme_Object* g53)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_ss_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g52;
    future->arg_s1 = g53;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g54, Scheme_Object* g55)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  MZ_MARK_STACK_TYPE retval;

  future = current_ft;
  future->prim_protocol = SIG_ss_m;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g54;
    future->arg_s1 = g55;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_m;
  future->retval_m = 0;
  
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g56, long g57)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_Sl_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g56;
    future->arg_l1 = g57;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, long g58)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_l_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_l0 = g58;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g59, Scheme_Object* g60, int g61)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_bsi_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g59;
    future->arg_s1 = g60;
    future->arg_i2 = g61;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g62, int g63, Scheme_Object** g64)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_iiS_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g62;
    future->arg_i1 = g63;
    future->arg_S2 = g64;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g65, Scheme_Object* g66)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_ss_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g65;
    future->arg_s1 = g66;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g67)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_b_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g67;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g68, long g69)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_sl_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g68;
    future->arg_l1 = g69;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g70, Scheme_Object** g71)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iS_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g70;
    future->arg_S1 = g71;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g72)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_S_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g72;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g73)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_s_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g73;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g74, Scheme_Object** g75, int g76)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = current_ft;
  future->prim_protocol = SIG_iSi_s;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g74;
    future->arg_S1 = g75;
    future->arg_i2 = g76;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval);
  return retval;
  END_XFORM_SKIP;
}
 void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g77, int g78, Scheme_Object** g79)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  

  future = current_ft;
  future->prim_protocol = SIG_siS_v;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g77;
    future->arg_i1 = g78;
    future->arg_S2 = g79;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  
  
  
  
  END_XFORM_SKIP;
}
 void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g80)
{
  START_XFORM_SKIP;
  future_t *future;
  double tm;
  void* retval;

  future = current_ft;
  future->prim_protocol = SIG_z_p;
  future->prim_func = f;
  tm = scheme_get_inexact_milliseconds();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_z0 = g80;
  future_do_runtimecall((void*)f, 0);
  future = current_ft;
  retval = future->retval_p;
  future->retval_p = 0;
  
  return retval;
  END_XFORM_SKIP;
}
