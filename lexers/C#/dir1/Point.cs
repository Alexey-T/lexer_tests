using System;
using System.Collections.Generic;
using System.Text;

class Point
/* 
Класс Point определяет координаты точки. Он имеет перегруженные операции + и -, а также перегруженный конструктор и метод ToString().
*/
{
    private int x, y;	// Координаты точки
    public Point() { }		// Умалчиваемый конструктор
    public Point(int xPos, int yPos)
    // Перегруженный конструктор принимает координаты точки
    {
        x = xPos;
        y = yPos;
    }
    public override string ToString()
    // Перегруженный метод ToString() возвращает строку с координатами
    {
        return "X pos: " + this.x + "; Y pos = " + this.y;
    }
    public static Point operator +(Point p1, Point p2)
    // Перегруженная операция "+" возвращает объект класса Point
    {
        Point newPoint = new Point(p1.x + p2.x, p1.y + p2.y);
        return newPoint;
    }
    public static Point operator -(Point p1, Point p2)
    // Перегруженная операция "-" возвращает объект класса Point
    {
        Point newPoint = new Point(p1.x - p2.x, p1.y - p2.y);
        return newPoint;
    }
    static void Main()
    {
        Point pt1 = new Point(100, 100);
        Point pt2 = new Point(40, 40);
        Console.WriteLine("1-я точка: {0}\n2-я точка: {1}\n",
                                     pt1.ToString(), pt2.ToString());
        Point bigPoint = pt1 + pt2;		// Сложение точек
        Console.WriteLine("Сложение : {0}", bigPoint.ToString());
        Point minorPoint = pt1 - pt2;	// Вычитание точек
        Console.WriteLine("Вычитание : {0}", minorPoint.ToString());
        Console.ReadLine();
    }
}
