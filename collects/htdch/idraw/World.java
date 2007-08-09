package idraw;
        
public abstract class World {
 protected Canvas theCanvas; 
 public void bigBang(int width, int height, double s) {
	if (width <= 0)
           throw new RuntimeException(
	              "The method bigBang(int,int,double) expects " +
		       "the first argument to be greather than 0, given " 
		       + width);
	if (height <= 0)
	    throw new RuntimeException(
                       "The method bigBang(int,int,double) expects " +
		       "the second argument to be greather than 0, given " 
		       + height);
	if (s <= 0)
	    throw new RuntimeException(
                       "The method bigBang(int,int,double) expects " +
		       "the third argument to be greather than 0, given " 
		       + s);
	theCanvas = new Canvas(width,height); 
	bigBangO(s);
	return ; 
    }
 private native void bigBangO(double s);

 // --------------------------------------------------------    

 public native boolean endOfTime(String s);
 public native World endOfWorld(String s);
 public abstract void onTick();
 public abstract void onKeyEvent(String ke); 
 public abstract void draw();
}
