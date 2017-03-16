public class Zero implements Term {

    public boolean isVal() {
	return true;
    }

    public boolean isNumVal() {
	return true;
    }

    public boolean isTrue() {
	return false;
    }

    public boolean isFalse() {
	return false;
    }

    public boolean isZero() {
	return true;
    }

    public boolean isSucc() {
	return false;
    }

    public boolean isPred() {
	return false;
    }

    public boolean isIsZero() {
	return false;
    }

    public boolean isIf() {
	return false;
    }

    public int size() {
	return 1;
    }

    public Type type() {
	return Type.NAT;
    }

    public Term eval() {
	return this;
    }	

    public String toString() {
	return "Zero";
    }

    public boolean same(Term that) {
	return that.isZero();
    }

    public Term t1() {
	throw new RuntimeException("Zero has no subterm t1");
    }

    public Term t2() {
	throw new RuntimeException("Zero has no subterm t2");
    }

    public Term t3() {
	throw new RuntimeException("Zero has no subterm t3");
    }
    
}
