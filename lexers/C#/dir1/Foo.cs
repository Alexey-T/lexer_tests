using System;
using System.Collections.Generic;
using System.Text;

// Определяем структуру с двумя целочисленными полями:
struct Foo
{
    public int x, y;
}
class ValRefClass
{
    static void Main()
    {
        // Создаем две переменные класса Foo
        Foo f1 = new Foo();
        Foo f2 = new Foo();
        f1.x = 100;
        f1.y = 200;
        // Присваиваем переменной f1 значение переменной f2:
        f2 = f1;
        // Выводим значения аеременных:
        Console.WriteLine("f1.x = {0}", f1.x);
        Console.WriteLine("f1.y = {0}", f1.y);
        Console.WriteLine("f2.x = {0}", f2.x);
        Console.WriteLine("f2.y = {0}", f2.y);
        // Изменяем f1:
        f1.x = 300;
        // Выводим поле х обеих переменных:
        Console.WriteLine("После изменения f1.x:");
        Console.WriteLine("f1.x = {0}", f1.x);
        Console.WriteLine("f2.x = {0}", f2.x);

        Console.ReadLine();
    }
}
