public class ComplexNumber implements Complex {

    /* private instance variables */
    private final double re;
    private final double im;
    
    /* The "final" modifier means the values of re and im can only be set once. */

    public ComplexNumber(double r, double i) {
	this.re = r;
	this.im = i;
    }

    public double getRe() {
		return this.re;
    }

    public double getIm() {
		return this.im;
    }
    
    public Complex negate() {
		double re = -(this.getRe());
		double im = -(this.getIm());
		ComplexNumber ret = new ComplexNumber(re, im);
		return ret;
    }

    public Complex plus(Complex that) {
		double re = (this.getRe() + that.getRe());
		double im = (this.getIm() + that.getIm());
		ComplexNumber ret = new ComplexNumber(re, im);
		return ret;
    }

    public Complex minus(Complex that) {
		double re = (this.getRe() - that.getRe());
		double im = (this.getIm() - that.getIm());
		ComplexNumber ret = new ComplexNumber(re, im);
		return ret;
    }

    public Complex times(Complex that) {
		double re = (this.getRe() * that.getRe()) - (this.getIm() * that.getIm());
		double im = (this.getRe() * that.getIm()) + (this.getIm() * that.getRe());
		ComplexNumber ret = new ComplexNumber(re, im);
		return ret;
    }

    public String toString() {
		return "(" + this.re + ", " + this.im + ")";
    }

}