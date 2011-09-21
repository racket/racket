 Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g60, int g61, Scheme_Object** g62)
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
      future->arg_s0 = g60;
    future->arg_i1 = g61;
    future->arg_S2 = g62;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g63, Scheme_Object** g64, Scheme_Object* g65)
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
      future->arg_i0 = g63;
    future->arg_S1 = g64;
    future->arg_s2 = g65;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g66)
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
      future->arg_s0 = g66;
  send_special_result(future, g66);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g67)
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
      future->arg_n0 = g67;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
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
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g68, Scheme_Object* g69)
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
      future->arg_s0 = g68;
    future->arg_s1 = g69;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g70, Scheme_Object* g71, int g72)
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
      future->arg_s0 = g70;
    future->arg_s1 = g71;
    future->arg_i2 = g72;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g73, const Scheme_Object* g74)
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
      future->arg_t0 = g73;
    future->arg_t1 = g74;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g75, Scheme_Object* g76)
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
      future->arg_s0 = g75;
    future->arg_s1 = g76;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_m;
  future->retval_m = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g77, intptr_t g78)
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
      future->arg_S0 = g77;
    future->arg_l1 = g78;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g79)
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
      future->arg_l0 = g79;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g80, Scheme_Object* g81, int g82)
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
      future->arg_b0 = g80;
    future->arg_s1 = g81;
    future->arg_i2 = g82;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g83, int g84, Scheme_Object** g85)
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
      future->arg_i0 = g83;
    future->arg_i1 = g84;
    future->arg_S2 = g85;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g86, Scheme_Object* g87)
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
      future->arg_s0 = g86;
    future->arg_s1 = g87;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g88)
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
      future->arg_b0 = g88;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g89, intptr_t g90)
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
      future->arg_s0 = g89;
    future->arg_l1 = g90;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g91, Scheme_Object** g92)
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
      future->arg_i0 = g91;
    future->arg_S1 = g92;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g93)
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
      future->arg_S0 = g93;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g94)
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
      future->arg_s0 = g94;
  send_special_result(future, g94);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g95, Scheme_Object** g96, int g97)
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
      future->arg_i0 = g95;
    future->arg_S1 = g96;
    future->arg_i2 = g97;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g98, int g99, Scheme_Object** g100)
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
      future->arg_s0 = g98;
    future->arg_i1 = g99;
    future->arg_S2 = g100;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g101)
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
      future->arg_z0 = g101;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_p;
  future->retval_p = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g102, int g103)
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
      future->arg_s0 = g102;
    future->arg_i1 = g103;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g104, int g105, Scheme_Object* g106)
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
      future->arg_s0 = g104;
    future->arg_i1 = g105;
    future->arg_s2 = g106;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g107, Scheme_Object* g108)
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
      future->arg_s0 = g107;
    future->arg_s1 = g108;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_i;
  future->retval_i = 0;
  
  return retval;
}
 void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g109, Scheme_Object** g110, void* g111)
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
      future->arg_i0 = g109;
    future->arg_S1 = g110;
    future->arg_p2 = g111;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sss_s(const char *who, int src_type, prim_sss_s f, Scheme_Object* g112, Scheme_Object* g113, Scheme_Object* g114)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_sss_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g112;
    future->arg_s1 = g113;
    future->arg_s2 = g114;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
