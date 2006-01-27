package graphics;

public abstract class CommandSequence {
  abstract void drawAll(View v);
  public CommandSequence reverse() {
    return this.rev(new EmptySeq());
  }
  abstract CommandSequence rev(CommandSequence acc);
}
