using System;
using System.Collections.Generic;
using System.Text;

class Program
{
    static int A(params int[] I)
    // Эта функция находит сумму произвольного количества
    // целочисленных параметров обращения
    {
        int Sum = 0;
        for (int i = 0; i < I.Length; i++)
            Sum += I[i]; ;
        return Sum;
    }
    static void Main()
    {
        Console.WriteLine("{0} {1}", A(1, 2, 3), A(4, 5, 6, 7, 8, 9));
        Console.ReadLine();
    }
}
