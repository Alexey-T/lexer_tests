using System;

class HelloClass
{
    // Конструктор по умолчанию:
    public HelloClass()
    {
        Console.WriteLine("Вызов умалчиваемого конструктора");
    }
    // Перегруженный конструктор:
    public HelloClass(int x, int y)
    {
        Console.WriteLine("Вызов перегруженного конструктора");
        intx = x;
        inty = y;
    }
    // Объявляем новые поля класса:
    public int intx, inty;
    // Точка входа программы:
    public static void Main()
    {
        HelloClass hc1 = new HelloClass();
        Console.WriteLine("hc1.intx = {0}\nhc1.inty = {1}\n",
            hc1.intx, hc1.inty);
        HelloClass hc2 = new HelloClass(100, 200);
        Console.WriteLine("hc2.intx = {0}\nhc2.inty = {1}\n",
            hc2.intx, hc2.inty);
        Console.ReadLine();
    }
}
