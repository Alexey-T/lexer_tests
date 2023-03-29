auto f0() {}
auto f1() const {}
auto f2() noexcept {}
auto f3() : a0(0), a1(new b) #ifdef {}
auto f4() & {}
auto f5() && {}
auto f10() -> int {}
auto f11() -> decltype(a + b) {}
auto f12() -> std::pair<decltype(n)> /**/ {}
struct s1 {}
operator ++(a, b) {}

auto Aa<T,TT>::f0() {}
auto Aa<T,TT>::f1() const {}
auto Aa<T,TT>::f2() noexcept {}
auto Aa<T,TT>::f3() : a0(0), a1(new b) #ifdef {}
auto Aa<T,TT>::f4() & {}
auto Aa<T,TT>::f5() && {}
auto Aa<T,TT>::f10() -> int {}
auto Aa<T,TT>::f11() -> decltype(a + b) {}
auto Aa<T,TT>::f12() -> std::pair<decltype(n)> /**/ {}
struct Aa<T,TT>::s1 {}

auto Aa<T,TT>::Bb<T>::f0() {}
auto Aa<T,TT>::Bb<T>::f1() const {}
auto Aa<T,TT>::Bb<T>::f2() noexcept {}
auto Aa<T,TT>::Bb<T>::f3() : a0(0), a1(new b) #ifdef {}
auto Aa<T,TT>::Bb<T>::f4() & {}
auto Aa<T,TT>::Bb<T>::f5() && {}
auto Aa<T,TT>::Bb<T>::f10() -> int {}
auto Aa<T,TT>::Bb<T>::f11() -> decltype(a + b) {}
auto Aa<T,TT>::Bb<T>::f12() -> std::pair<decltype(n)> /**/ {}
struct Aa<T,TT>::Bb<T>::s1 {}

auto Aa<T,TT>::Bb<T>::Cc::f0() {}
auto Aa<T,TT>::Bb<T>::Cc::f1() const {}
auto Aa<T,TT>::Bb<T>::Cc::f2() noexcept {}
auto Aa<T,TT>::Bb<T>::Cc::f3() : a0(0), a1(new b) #ifdef {}
auto Aa<T,TT>::Bb<T>::Cc::f4() & {}
auto Aa<T,TT>::Bb<T>::Cc::f5() && {}
auto Aa<T,TT>::Bb<T>::Cc::f10() -> int {}
auto Aa<T,TT>::Bb<T>::Cc::f11() -> decltype(a + b) {}
auto Aa<T,TT>::Bb<T>::Cc::f12() -> std::pair<decltype(n)> /**/ {}
struct Aa<T,TT>::Bb<T>::Cc::s1 {}
