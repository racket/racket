
wxGLConfig::wxGLConfig()
  : wxObject(FALSE)
{
  doubleBuffered = 1;
  depth = 1;
}

wxGLConfig::~wxGLConfig()
{
}

wxGLConfig *wxGLConfig::Clone()
{
  wxGLConfig *c;
  c = new wxGLConfig();
  c->doubleBuffered = doubleBuffered;
  c->stereo = stereo;
  c->stencil = stencil;
  c->accum = accum;
  c->depth = depth;
  c->multisample = multisample;
  return c;
}
