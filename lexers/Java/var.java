class Test {
  void func() {
    A a = new A();
    var b = new B(); ////////
    C var = new C(); //-----
    var = new C(); //-----
    {var d = new D();} ///////////

    for (var d : elements()) { ///////////
    }
  }
}

private
