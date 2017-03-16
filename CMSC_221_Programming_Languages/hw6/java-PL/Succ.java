public class Succ implements Term {

	private Term t1;

	public Succ(Term ter1) {
		t1 = ter1;
	}

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
	return false;
    }

    public boolean isSucc() {
	return true;
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
	return 1 + this.t1().size();
    }

    public Type type() {
		if (this.t1().type().equals(Type.NAT) ) {
			return Type.NAT;
		} else
		throw new TypeErrorException("Succ is not well-typed!");
    }

    public Term eval() {
	return new Succ(this.t1().eval());
    }	

    public String toString() {
	return "Succ(" + this.t1().toString() + ")";
    }

    public boolean same(Term that) {
	return that.isSucc() && this.t1().same(that.t1());
    }

    public Term t1() {
	return t1;
    }

    public Term t2() {
	throw new RuntimeException("Succ has no subterm t2");
    }

    public Term t3() {
	throw new RuntimeException("Succ has no subterm t3");
    }
    
}

