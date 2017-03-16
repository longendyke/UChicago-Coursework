public class Cons<T> implements IList<T> {

    private int length;
    private T head;
    private IList<T> tail;

    public Cons(T h, IList<T> t) {
	if (h==null) {
	    throw new IllegalArgumentException("h cannot be null");
	}
	if (t==null) {
	    throw new IllegalArgumentException("t cannot be null");
	}
	this.length = 1+t.length();
	this.head = h;
	this.tail = t;
    }

    public T first() {
	   return this.head;
    }

    public IList<T> rest() {
	   return this.tail;
    }

    public boolean isEmpty() {
	   return false;
    }

    public int length() {
	   return 1 + this.tail.length();
    }

    public T nth(int n) {
	   if(n == 0){
        return this.first();
       } else {
        return this.tail.nth(n-1);
       }
    }

    public boolean same(IList<T> that) {
	/* use .equals to compare list items */
	   if( this.first().equals(that.first()) ){
            if(this.rest() == null){
                return true;
            } else {
                return this.rest().same(that.rest());
            }
       } else {
            return false;
       }
    }

    public String toString() {
        return this.first() + "::" + this.tail.toString();
    }
}
