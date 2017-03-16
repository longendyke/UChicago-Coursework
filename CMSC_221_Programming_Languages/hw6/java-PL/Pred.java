public class Pred implements Term {

	public Term t1;

	public Pred(Term ter1) {
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
	return true;
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
		if (this.t1().type() == Type.NAT) {
			return Type.NAT;
		} else
		throw new TypeErrorException("Pred is not well-typed!");
    }

    public Term eval() {
    	Term eval_d = this.t1().eval();
		if (eval_d.isZero()) {
			return new Zero();
		} else if (eval_d.isSucc()) {
			return eval_d.t1();
		} else {
			return new Pred(eval_d);
		}
    }	

    public String toString() {
	return "Pred(" + this.t1().toString() + ")";
    }

    public boolean same(Term that) {
	return that.isPred() && this.t1().same(that.t1());
    }

    public Term t1() {
	return t1;
    }

    public Term t2() {
	throw new RuntimeException("Pred has no subterm t2");
    }

    public Term t3() {
	throw new RuntimeException("Pred has no subterm t3");
    }
    
}
