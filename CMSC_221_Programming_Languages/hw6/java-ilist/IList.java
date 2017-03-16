public interface IList<T> {
    public T first();
    public IList<T> rest();
    public boolean isEmpty();
    public int length();
    public T nth(int n);
    public boolean same(IList<T> that);
}
