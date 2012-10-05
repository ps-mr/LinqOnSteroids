//Trigger a violation of the UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR analysis
class Base {
  void foo() {}
  Base() {
    foo();
  }
}

class Derived extends Base {
  int a = 1;
  @Override void foo() {
    System.out.println(a);
  }
}
// vim: set sw=2:
