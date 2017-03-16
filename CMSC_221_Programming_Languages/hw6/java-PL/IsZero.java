public class IsZero implements Term {

	public Term t1;

	public IsZero(Term ter1) {
		t1 = ter1;
	}

    public boolean isVal() {
	return false;
    }

    public boolean isNumVal() {
	return false;
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
	return false;
    }

    public boolean isPred() {
	return false;
    }

    public boolean isIsZero() {
	return true;
    }

    public boolean isIf() {
	return false;
    }

    public int size() {
	return 1 + this.t1().size();
    }

    public Type type() {
		if (this.t1().type() == Type.NAT) {
			return Type.BOOL;
		} else
		throw new TypeErrorException("IsZero is not well-typed!");
    }

    public Term eval() {
		if (this.t1().eval().isZero()) {
			return new True();
		} else {
			return new False();
		}
    }	

    public String toString() {
	return "IsZero(" + this.t1().toString() + ")";
    }

    public boolean same(Term that) {
	return that.isZero() && this.t1().same(that.t1());
    }

    public Term t1() {
	return t1;
    }

    public Term t2() {
	throw new RuntimeException("IsZero has no subterm t2");
    }

    public Term t3() {
	throw new RuntimeException("IsZero has no subterm t3");
    }
    
}
