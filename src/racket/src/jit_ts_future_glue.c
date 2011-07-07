 Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g56, int g57, Scheme_Object** g58)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_siS_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g56;
    future->arg_i1 = g57;
    future->arg_S2 = g58;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g59, Scheme_Object** g60, Scheme_Object* g61)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iSs_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g59;
    future->arg_S1 = g60;
    future->arg_s2 = g61;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g62)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_s_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g62;
  send_special_result(future, g62);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g63)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_n_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_n0 = g63;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f )
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG__s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
  
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g64, Scheme_Object* g65)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g64;
    future->arg_s1 = g65;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g66, Scheme_Object* g67, int g68)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ssi_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g66;
    future->arg_s1 = g67;
    future->arg_i2 = g68;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g69, const Scheme_Object* g70)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_tt_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_t0 = g69;
    future->arg_t1 = g70;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g71, Scheme_Object* g72)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  MZ_MARK_STACK_TYPE retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_m;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g71;
    future->arg_s1 = g72;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_m;
  future->retval_m = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g73, intptr_t g74)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_Sl_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g73;
    future->arg_l1 = g74;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g75)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_l_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_l0 = g75;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g76, Scheme_Object* g77, int g78)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_bsi_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g76;
    future->arg_s1 = g77;
    future->arg_i2 = g78;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g79, int g80, Scheme_Object** g81)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iiS_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g79;
    future->arg_i1 = g80;
    future->arg_S2 = g81;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g82, Scheme_Object* g83)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g82;
    future->arg_s1 = g83;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g84)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_b_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g84;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g85, intptr_t g86)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_sl_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g85;
    future->arg_l1 = g86;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g87, Scheme_Object** g88)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iS_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g87;
    future->arg_S1 = g88;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g89)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_S_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g89;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g90)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_s_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g90;
  send_special_result(future, g90);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g91, Scheme_Object** g92, int g93)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iSi_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g91;
    future->arg_S1 = g92;
    future->arg_i2 = g93;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g94, int g95, Scheme_Object** g96)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_siS_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g94;
    future->arg_i1 = g95;
    future->arg_S2 = g96;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g97)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  void* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_z_p;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_z0 = g97;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_p;
  future->retval_p = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g98, int g99)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_si_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g98;
    future->arg_i1 = g99;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g100, int g101, Scheme_Object* g102)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_sis_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g100;
    future->arg_i1 = g101;
    future->arg_s2 = g102;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g103, Scheme_Object* g104)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  int retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_i;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g103;
    future->arg_s1 = g104;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_i;
  future->retval_i = 0;
  
  return retval;
}
 void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g105, Scheme_Object** g106, void* g107)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iSp_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g105;
    future->arg_S1 = g106;
    future->arg_p2 = g107;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
