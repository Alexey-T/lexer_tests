using System;
using System.Collections.Generic;
using System.Text;

class Class1
{
    public delegate void DelDemo();	// Объявляем делегата как 
    // процедуру без параметров
    public static void Del1()		// 1-й экземпляр делегата
    {
        Console.WriteLine("Это работает Del1");
    }
    public static void Del2()		// 2-й экземпляр делегата
    {
        Console.WriteLine("Это работает Del2");
    }
    // Процедура DelegateDemo принимает как одиночного (одноадресного), 
    // так и многоадресного делегата:
    public static void DelegateDemo(DelDemo del)
    {
        del();
    }

    static void Main()
    {
        DelDemo DD1 = new DelDemo(Del1);
        DelDemo DD2 = new DelDemo(Del2);
        // Объединяем оба экземпляра:
        DelegateDemo(DD1 + DD2);		/* Вывод:  	Это работает Del1
												Это работает Del2   */
        Console.ReadLine();
    }
}
