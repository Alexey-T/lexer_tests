template <typename T1, typename T2> auto compose(T1 a, T2 b) -> decltype(a + b)
{
   return a+b;
}

template<auto n> // C++17 auto parameter declaration
auto f() -> std::pair<decltype(n), decltype(n)> // auto can't deduce from brace-init-list
{
    return {n, n};
}

auto make_var() -> int
{
  return 1;
}
