XFORM_NONGCING static double SCH_ROUND(double d)
{
  double i, frac;
  int invert;

#ifdef FMOD_CAN_RETURN_POS_ZERO
  if ((d == 0.0) && minus_zero_p(d))
    return d;
#endif

  if (d < 0) {
    d = -d;
    invert = 1;
  } else
    invert = 0;

  frac = modf(d, &i);
  if (frac < 0.5)
    d = i;
  else if (frac > 0.5)
    d = i + 1;
  else if (fmod(i, 2.0) != 0.0)
    d = i + 1;
  else
    d = i;

  if (invert)
    d = -d;

  return d;
}
