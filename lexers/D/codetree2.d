void sampleFunc(int[] num)
{
}

// not found
class Sample1 : Base
{
}

// not found
class Sample2 // : Base
{
}

// not found
class Sample3 /* : Base */
{
}

// found
enum Sample4 : Base
{
}
