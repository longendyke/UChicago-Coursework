public interface Complex {

    public double getRe();
    public double getIm();

    public Complex negate();
    public Complex plus(Complex that);
    public Complex minus(Complex that);
    public Complex times(Complex that);

    public String toString();

}