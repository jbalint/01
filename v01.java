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
	static class And implements Formula {
		public And(Formula f1, Formula f2) {
			this.f1 = f1;
			this.f2 = f2;
		}
		Formula f1;
		Formula f2;
		public boolean value(boolean assignment[]) {
			return f1.value(assignment) && f2.value(assignment);
		}
	}
	public static void main(String args[]) {
		boolean a[] = new boolean[] {true, true, false};
		Term S0 = new SSym(0);
		Term S1 = new SSym(1);
		Term S2 = new SSym(2);
		Formula sent = new And(new Not(S1), new And(S2, S1));
		System.out.println(sent.value(a));
	}
}
