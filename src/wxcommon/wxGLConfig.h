
class wxGLConfig : public wxObject {
 public:
  Bool doubleBuffered, stereo;
  int stencil;
  int accum;
  int depth;
  int multisample;
  
  wxGLConfig();
  ~wxGLConfig();

  wxGLConfig *Clone();
};
