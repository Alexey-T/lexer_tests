using System;

public class EnumTest 
{
	enum Days {Понедельник, Вторник, Среда, Четверг, Пятница, Суббота, Воскресенье};

	public static void Main() 
	{

		Type weekdays = typeof(Days);
		Days wd = Days.Пятница;
		Console.WriteLine("Члены перечисления Days и их значения:");

		foreach ( string s in Enum.GetNames(weekdays) )
			Console.WriteLine( "{0,-12}= {1}", s, Enum.Format( weekdays, Enum.Parse(weekdays, s), "d"));

		Console.WriteLine();
		Console.WriteLine("Базовый тип членов: {0}\n", Enum.GetUnderlyingType(weekdays));
		Console.WriteLine("16-ричное значение члена {0} равно {1}", wd.ToString(), Enum.Format(weekdays, wd, "X"));
		Console.ReadLine();
	}
}