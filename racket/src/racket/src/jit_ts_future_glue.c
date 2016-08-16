 Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g238, int g239, Scheme_Object** g240)
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
      future->arg_s0 = g238;
    future->arg_i1 = g239;
    future->arg_S2 = g240;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g241, Scheme_Object** g242, Scheme_Object* g243)
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
      future->arg_i0 = g241;
    future->arg_S1 = g242;
    future->arg_s2 = g243;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g244)
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
      future->arg_s0 = g244;
  send_special_result(future, g244);
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Lambda* g245)
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
      future->arg_n0 = g245;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
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
  
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g246, Scheme_Object* g247)
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
      future->arg_s0 = g246;
    future->arg_s1 = g247;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g248, Scheme_Object* g249, int g250)
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
      future->arg_s0 = g248;
    future->arg_s1 = g249;
    future->arg_i2 = g250;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g251, const Scheme_Object* g252)
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
      future->arg_t0 = g251;
    future->arg_t1 = g252;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g253, Scheme_Object* g254)
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
      future->arg_s0 = g253;
    future->arg_s1 = g254;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_m;
  future->retval_m = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g255, intptr_t g256)
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
      future->arg_S0 = g255;
    future->arg_l1 = g256;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g257)
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
      future->arg_l0 = g257;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g258, Scheme_Object* g259, int g260)
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
      future->arg_b0 = g258;
    future->arg_s1 = g259;
    future->arg_i2 = g260;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g261, int g262, Scheme_Object** g263)
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
      future->arg_i0 = g261;
    future->arg_i1 = g262;
    future->arg_S2 = g263;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g264, Scheme_Object* g265)
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
      future->arg_s0 = g264;
    future->arg_s1 = g265;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g266)
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
      future->arg_b0 = g266;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g267, intptr_t g268)
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
      future->arg_s0 = g267;
    future->arg_l1 = g268;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g269, Scheme_Object** g270)
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
      future->arg_i0 = g269;
    future->arg_S1 = g270;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g271)
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
      future->arg_S0 = g271;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g272)
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
      future->arg_s0 = g272;
  send_special_result(future, g272);
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g273, Scheme_Object** g274, int g275)
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
      future->arg_i0 = g273;
    future->arg_S1 = g274;
    future->arg_i2 = g275;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g276, int g277, Scheme_Object** g278)
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
      future->arg_s0 = g276;
    future->arg_i1 = g277;
    future->arg_S2 = g278;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_Sii_s(const char *who, int src_type, prim_Sii_s f, Scheme_Object** g279, int g280, int g281)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_Sii_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g279;
    future->arg_i1 = g280;
    future->arg_i2 = g281;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g282)
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
      future->arg_z0 = g282;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_p;
  future->retval_p = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g283, int g284)
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
      future->arg_s0 = g283;
    future->arg_i1 = g284;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g285, int g286, Scheme_Object* g287)
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
      future->arg_s0 = g285;
    future->arg_i1 = g286;
    future->arg_s2 = g287;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g288, Scheme_Object* g289)
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
      future->arg_s0 = g288;
    future->arg_s1 = g289;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_i;
  future->retval_i = 0;
  
  return retval;
}
 void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g290, Scheme_Object** g291, void* g292)
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
      future->arg_i0 = g290;
    future->arg_S1 = g291;
    future->arg_p2 = g292;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sss_s(const char *who, int src_type, prim_sss_s f, Scheme_Object* g293, Scheme_Object* g294, Scheme_Object* g295)
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
      future->arg_s0 = g293;
    future->arg_s1 = g294;
    future->arg_s2 = g295;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall__v(const char *who, int src_type, prim__v f )
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG__v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
  
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_iS_v(const char *who, int src_type, prim_iS_v f, int g296, Scheme_Object** g297)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iS_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g296;
    future->arg_S1 = g297;
  
  future_do_runtimecall(fts, (void*)f, 0, 1, 0);
  fts->thread = scheme_current_thread;
  future = fts->thread->current_ft;
  
  
  
  
}
