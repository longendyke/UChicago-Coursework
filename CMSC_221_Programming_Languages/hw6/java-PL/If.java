public class If implements Term {

	public Term t1;
	public Term t2;
	public Term t3;

	public If(Term ter1, Term ter2, Term ter3) {
		t1 = ter1;
		t2 = ter2;
		t3 = ter3;
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
	return false;
    }

    public boolean isIf() {
	return true;
    }

    public int size() {
	return 1 + this.t1().size() + this.t2().size() + this.t3().size();
    }

    public Type type() {
		if (this.t1().type() == Type.BOOL) {
			if ( this.t2().type().equals(this.t3().type()) ){
				return this.t2().type();
			}
			else
				throw new TypeErrorException("Non-matching return types in If statement!");
		} else
		throw new TypeErrorException("If is not well-typed!");
    }

    // Assuming we're well-typed here...
    public Term eval() {
    	Term ev_t1 = this.t1().eval();
    	if (ev_t1.isTrue()){
    		return this.t2().eval();
    	} else
    		return this.t3().eval();
    }	

    public String toString() {
	return "If(" + this.t1().toString() + "," + this.t2().toString() + "," + this.t3().toString() + ")";
    }

    public boolean same(Term that) {
	return that.isIf() && this.t1().same(that.t1()) && this.t2().same(that.t2()) && this.t3().same(that.t3());
    }

    public Term t1() {
	return t1;
    }

    public Term t2() {
	return t2;
    }

    public Term t3() {
	return t3;
    }
    
}
