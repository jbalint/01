import java.util.*;
public class v01 {
	static interface Formula {
		boolean value(boolean a[]);
	}
	static interface Term extends Formula {}
	static class SSym implements Term {
		public SSym(int n) {
			this.n = n;
		}
		int n;
		public boolean value(boolean assignment[]) {
			return assignment[n];
		}
	}
	static class Not implements Formula {
		public Not(Formula f) {
			this.f = f;
		}
		Formula f;
		public boolean value(boolean assignment[]) {
			return !f.value(assignment);
		}
	}
	static abstract class TwoTermFormula implements Formula {
		public TwoTermFormula(Formula f1, Formula f2) {
			this.f1 = f1;
			this.f2 = f2;
		}
		Formula f1;
		Formula f2;
	}
	static class And extends TwoTermFormula {
		public And(Formula f1, Formula f2) { super(f1, f2); }
		public boolean value(boolean assignment[]) {
			return f1.value(assignment) && f2.value(assignment);
		}
	}
	static class Or extends TwoTermFormula {
		public Or(Formula f1, Formula f2) { super(f1, f2); }
		public boolean value(boolean assignment[]) {
			return f1.value(assignment) || f2.value(assignment);
		}
	}
	static class Implies extends TwoTermFormula {
		public Implies(Formula f1, Formula f2) { super(f1, f2); }
		public boolean value(boolean assignment[]) {
			return !f1.value(assignment) ||
					f2.value(assignment);
		}
	}
	static class Iff extends TwoTermFormula {
		public Iff(Formula f1, Formula f2) { super(f1, f2); }
		public boolean value(boolean assignment[]) {
			return f1.value(assignment) == f2.value(assignment);
		}
	}
	static boolean isTaut(Formula f, int symCount) {
		boolean a[] = new boolean[symCount];
		for (int i = 0; i < Math.pow(2, symCount); ++i) {
			// create assignment
			for (int j = 0; j < symCount; ++j) {
				a[j] = ((i >> j) & 1) == 1;
			}
			/*
			List l = new ArrayList(symCount);
			for (int j = 0; j < symCount; ++j) {
				l.add(a[j]);
			}
			Collections.reverse(l);
			System.out.println(l);
			*/
			if (!f.value(a))
				return false;
		}
		return true;
	}
	public static void main(String args[]) {
		boolean a[] = new boolean[] {true, true, false};
		Term S0 = new SSym(0);
		Term S1 = new SSym(1);
		Term S2 = new SSym(2);
		Formula sent = new And(new Not(S2), new And(S0, S1));
		//System.out.println(sent.value(a));
		Formula taut1 = new Or(S0, new Not(S0));
		System.out.println("is taut: " + isTaut(taut1, 3));
	}
}
