package draw;
        
public abstract class World {
    protected Canvas theCanvas;
    public boolean bigBang(int width, int height, double s) {
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
	return bigBangO(s);
 };
 private native boolean bigBangO(double s);

 // --------------------------------------------------------    

 public native boolean endOfTime();
 public native World endOfWorld();
 public abstract World onTick();
 public abstract World onKeyEvent(String ke); 
 public abstract boolean draw();
 public abstract boolean erase();
}    
