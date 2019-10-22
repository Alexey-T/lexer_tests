    class A {
    public:
        int x;
        int Get() const { return x; }
    };

    int main()
    {
        A a;
        return a.Get();
    }
