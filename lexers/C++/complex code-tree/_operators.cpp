template <class T>
    inline CStringT<T>::operator int() const
    {
        return 0;
    }

template <class T>
    inline CStringT<T>::operator double() const
    {
        return 0.0;
    }

template <class T>
    inline CStringT<T>::operator std::string() const
    {
        return std::string();
    }
    
    
class MyFunctor
{
  public:
    void operator +()
    {
    }
    void operator []()
    {
    }
    
    void operator()()
    {
        printf("calling without parameters");
    }
    
    int operator()(int x)
    {
        printf("calling with int parameter: %d", x);
        return x;
    }
};
