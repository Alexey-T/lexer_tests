using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
class ArrayListDemo
{
    static ArrayList Books = new ArrayList();
    public static void ShowCollection(IEnumerable Coll, string Rem)
    {
        Console.WriteLine(Rem);
        System.Collections.IEnumerator myEnum = Coll.GetEnumerator();
        while (myEnum.MoveNext())
            Console.WriteLine(myEnum.Current);
    }
    static void Main()
    {
        // Создаем и наполняем коллекцию:
        Books.Add("А.Бокс, К.Селлз. Основы платформы .NET, том 1.");
        Books.Add("А.Гарнаев. Visual Studio .NET 2003.");
        Books.Add("Э Троелсен. С# и платформа .NET");
        Books.Add("К. Пачеко. Delphi for .NET");
        Books.Add("С. Уолтер. ASP.NET");
        ShowCollection(Books, "Исходная коллекция:\n");
        // Сортируем коллекцию:
        Books.Sort();
        ShowCollection(Books, "\nМетод Sort:");
        // Сортируем в обратном порядке:
        Books.Reverse();
        ShowCollection(Books, "\nМетод Revers:");
        // Выделяем диапазон:
        ArrayList SubList = new ArrayList();
        SubList = Books.GetRange(2, 3);
        ShowCollection(SubList, "\nМетод GetRange:");
        // Создаем и вставляем диапазон:
        ArrayList Range = new ArrayList();
        Range.Add("Н.Секунов. Разработка приложений на С++ и C#.");
        Range.Add("М.Кенту. Delphi 7 для профессионалов.");
        Books.AddRange(Range);
        ShowCollection(Books, "\nМетод AddRange:");
        // Иллюстрация метода TrimToSize:
        Console.WriteLine("\nЕмкость перед TrimToSize(): {0}",
                                                        Books.Capacity);
        Books.TrimToSize();
        Console.WriteLine("Емкость после TrimToSize(): {0}",
                                                        Books.Capacity);
        Console.ReadLine();
    }
}
