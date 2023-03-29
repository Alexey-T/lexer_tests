template <typename T1, typename T2> auto f1(T1 a, T2 b) -> decltype(a + b)
{
}

template<auto n> // C++17 auto parameter declaration
auto f2() -> std::pair<decltype(n), decltype(n)> // auto can't deduce from brace-init-list
{
}

auto f3() -> int
{
}

//------------------------
_NODISCARD mapped_type& g1(const key_type& _Keyval) {
}

_NODISCARD const mapped_type& g2(const key_type& _Keyval) const {
}
