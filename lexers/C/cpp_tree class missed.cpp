#include <iostream>
#include <gtest/gtest.h>

using ::testing::InitGoogleTest;
using ::testing::UnitTest;
using ::testing::TestCase;
using ::testing::TestInfo;

unsigned long fibonacci(unsigned long n)
{
    return n < 2 ? n : fibonacci(n - 1) + fibonacci(n - 2);
}

TEST(fibonacci, 0)
{
    EXPECT_EQ(0, fibonacci(0));
}

TEST(fibonacci, 1)
{
    EXPECT_EQ(1, fibonacci(1));
}

class MyTest : public testing::Test {
    static int priv;

    const int tot;

    unsigned long fib(unsigned long n) {
        return n < 2 ? n : fib(n - 1) + fib(n - 2);
    }

protected:

    int prot;

    virtual void SetUp() {
        if (priv++ == 0) {
            std::cout << "SUITE_setup" << std::endl;
        }
        std::cout << "setup" << std::endl;
    }

    virtual void TearDown() {
        std::cout << "teardown" << std::endl;
        if (priv == tot) {
            std::cout << "SUITE_teardown" << std::endl;
        }
    }

public:

    int pub;

    MyTest() : tot(2), prot(1), pub(2) { }

    virtual ~MyTest() { }

    void DoTest1() {
        EXPECT_EQ(1, fib(2));
        EXPECT_EQ(2, pub++); // pub is changed...
    }

    void DoTest2() {
        EXPECT_EQ(2, fib(3));
        EXPECT_EQ(2, pub); // but is reset by google :-(
    }

};
int MyTest::priv = 0;

TEST_F(MyTest, test1)
{
    //EXPECT_EQ(0, priv);
    EXPECT_EQ(1, prot);
    EXPECT_EQ(2, pub);
    DoTest1();
}

TEST_F(MyTest, test2)
{
    DoTest2();
    //EXPECT_EQ(0, priv);
    EXPECT_EQ(1, prot);
    EXPECT_EQ(2, pub);
}

int main(int argc, char** argv)
{
	InitGoogleTest(&argc, argv);
	RUN_ALL_TESTS();

/*
    UnitTest& unit_test = *UnitTest::GetInstance();

    for (int i = 0, l1 = unit_test.total_test_case_count(); i < l1; i++)
    {
		const TestCase& test_case = *unit_test.GetTestCase(i);
		for (int j = 0, l2 = test_case.total_test_count(); j < l2; j++)
        {
			const TestInfo& test_info = *test_case.GetTestInfo(j);
            std::cout << test_case.name() << "::" << test_info.name() << ": "
                    << (test_info.result()->Passed() ? "" : "FAILED!\n");
			if (test_info.result()->Passed())
            {
                std::cout << test_info.result()->elapsed_time() <<
                        " ms" << std::endl;
            }
		}
	}
	puts("");
*/
	return 0;
}
