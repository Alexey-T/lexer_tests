using System;
using System.Collections.Generic;
using System.Text;

class IndexerDemo
{
    class Book			// Класс для одной книги
    {
        string Title, Author;
        public Book(string Author, string Title) // Конструктор для книги
        {
            this.Author = Author;
            this.Title = Title;
        }
        class Books		// Класс для нескольких книг
        {
            Book[] bookArray = new Book[10]; //Массив на 10 книг
            public Book this[int pos]		// Индексатор	
            {
                get
                {
                    if (pos >= 0 || pos < 10) return bookArray[pos];
                    else throw new IndexOutOfRangeException("Вне диапазона!");
                }
                set { bookArray[pos] = value; }
            }
        }
        static void Main()
        {
            // Создаем и наполняем объект Books:
            Books BS = new Books();
            BS[0] = new Book("A.Гарнаев.", "Visual Studio .NET 2003");
            BS[1] = new Book("Э.Троелсен.", "С# и платформа .NET");
            // Выводим на консоле:
            for (int i = 0; i < 2; i++)
                Console.WriteLine("Книга {0}: {1} {2}",
                                        i, BS[i].Author, BS[i].Title);
            Console.ReadLine();
        }
    }
}
