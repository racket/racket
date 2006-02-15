package graphics;

public abstract class World {

  public View display;

  World(View v) {
     display = v;
  }

  //Produce a World with the effects of receiving the given key
  public abstract World onKey( String key );

  //Produce a World with the effects of one clock tick passing
  public abstract World onTick();

  //Produces a World that will animate with a clock tick of rate
  public abstract boolean animate( int width, int height, int rate );

  public abstract Image draw();

  public abstract Image erase();

}

