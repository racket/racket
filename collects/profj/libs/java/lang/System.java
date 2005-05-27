package java.lang;

public final class System {

  public static native long currentTimeMillis();

  public static native void gc();

  public static native int identityHashCode( Object x);
 
}