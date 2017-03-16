public class Tests {

    public static void main(String[] args) {
	
	Complex z0  = new ComplexNumber(0.0,0.0);
	Complex z1  = new ComplexNumber(1.0,0.0);
	Complex i   = new ComplexNumber(0.0,1.0);
	Complex z1i = new ComplexNumber(1.0,1.0);

	System.out.println("ComplexNumber tests.");

	System.out.println(" z0: " + z0);
	System.out.println(" z1: " + z1);
	System.out.println("  i: " +  i);
	System.out.println("z1i: " + z1i);

	System.out.println("z0.plus(z1): " + z0.plus(z1));
	System.out.println("z1.negate(): " + z1.negate());
	System.out.println("i.times(i):  " + i.times(i));	

    }

}