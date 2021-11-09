using System;
class X {
	void M ()
	{
		Z <int>(P, x => { return 1; });
	}

	string P { get => null; }
	void Z<T> (string p1, Func<int, T> func)
	{
	}
}