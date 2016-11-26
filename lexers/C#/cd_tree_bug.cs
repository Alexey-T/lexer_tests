namespace iText
{
    public class Program
    {
        static int Main(string[] args)
        {
            if (args.Length == 1 && args[0].StartsWith("@"))
            {
                foreach (string argsLine in File.ReadLines(file, Encoding.GetEncoding(1251)))
                {
                    // Разбираем файл с параметрами построчно. Отрезки в кавычках сохраняем как есть
                    string[] cmdArgs = Regex.Split(argsLine, "(?<=^[^\"]*(?:\"[^\"]*\"[^\"]*)*) (?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                } // foreach
            } // if args.Length == 1
        } //-- Main
    } // class Program
} // namespace iText
