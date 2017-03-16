public class Nil<T> implements IList<T> {

    public T first() {
	   throw new RuntimeException("Error: Nil has no first");
    }

    public IList<T> rest() {
	   throw new RuntimeException("Error: Nil has no first");
    }

    public boolean isEmpty() {
	   return true;
    }

    public int length() {
	   return 0;
    }

    public T nth(int n) {
	   throw new RuntimeException("Error: can't take nth of Nil!");
    }

    public boolean same(IList<T> that) {
       if (that == null)
       		return false;
	   else if(that.isEmpty())
            return true;
        else
            return false;
    }

    public String toString() {
        return "Nil";
    }

}