package draw;

public abstract class World {
    protected Canvas theCanvas = new SillyCanvas(600,600); // can I do better than null here? 
    
    /** 
     *@author Matthias Felleisen, Kathy Gray 
     *@param width positive int, the width of the visible canvas 
     *@param height positive int, the height of the visible canvas 
     *@param s positive double, the rate at which the clock ticks per second 
     *@return true if the world is created without obstacle 
     */ 
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

    /**
     *@param s is the message to be displayed 
     *@return true, if it succeeds in stopping the clock and displaying the message
     *After the end of time, events no longer trigger calls
     *to onTick or onKeyEvent (see below). The canvas remains visible. 
     */
    public native boolean endOfTime(String s);

    /**
     *@param s is the message to be displayed 
     *@return the last World, if it succeeds in stopping the clock and displaying the message
     *After the end of the world, events no longer trigger calls
     *to onTick or onKeyEvent (see below). The canvas remains visible. 
     */
    public native World endOfWorld(String s);

    /**
     *@return the new world 
     *The method is invoked for every tick of the clock. Its purpose is to create a 
     * World whose differences with this one represent what happened during the amount 
     * of time it takes the clock to tick. 
     */
    public abstract World onTick();

    /** 
     *@param ke the String representing the key that was pressed
     *@return the new world
     *The method is invoked for every keyboard event associated with the canvas. 
     * Its purposes is to create a World whose differences with this one represent
     * what happens due to the user's use of the keyboard. 
     */
    public abstract World onKeyEvent(String ke); 

    /**
     *@return true, if the method succeeds in printing this world ('s canvas)
     */
    public abstract boolean draw();

}    
