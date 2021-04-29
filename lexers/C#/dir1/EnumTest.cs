using System;

public class EnumTest 
{
	enum Days {�����������, �������, �����, �������, �������, �������, �����������};

	public static void Main() 
	{

		Type weekdays = typeof(Days);
		Days wd = Days.�������;
		Console.WriteLine("����� ������������ Days � �� ��������:");

		foreach ( string s in Enum.GetNames(weekdays) )
			Console.WriteLine( "{0,-12}= {1}", s, Enum.Format( weekdays, Enum.Parse(weekdays, s), "d"));

		Console.WriteLine();
		Console.WriteLine("������� ��� ������: {0}\n", Enum.GetUnderlyingType(weekdays));
		Console.WriteLine("16-������ �������� ����� {0} ����� {1}", wd.ToString(), Enum.Format(weekdays, wd, "X"));
		Console.ReadLine();
	}
}