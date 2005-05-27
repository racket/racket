
package java.lang;

public final class Math extends Object {
  private Math() { super(); }

  private native static double getE();
  private native static double getPI();

  public static final double E = getE();
  public static final double PI = getPI();

  public native static double abs(double a);
  public native static float abs(float a);
  public native static int abs(int a);
  public native static long abs(long a);
  public native static double acos(double a);
  public native static double asin(double a);
  public native static double atan(double a);
  public native static double atan2(double y, double x);
  public native static double ceil(double a);
  public native static double cos(double a);
  public native static double exp(double a);
  public native static double floor(double a);
  public native static double IEEEremainder(double f1, double f2);
  public native static double log(double a);
  public native static double max(double a, double b);
  public native static float max(float a, float b);
  public native static int max(int a, int b);
  public native static long max(long a, long b);
  public native static double min(double a, double b);
  public native static float min(float a, float b);
  public native static int min(int a, int b);
  public native static long min(long a, long b);
  public native static double pow(double a, double b);
  public native static double random();
  public native static double rint(double a);
  public native static long round(double a);
  public native static int round(float a);
  public native static double sin(double a);
  public native static double sqrt(double a);
  public native static double tan(double a);
  public native static double toDegrees(double angrad);
  public native static double toRadians(double angdeg); 
}
