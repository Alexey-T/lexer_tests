//--------------
interface IExample
{
    string Text { get; set; }
}

// must be in code-tree!
ref struct RefStructExample : IExample
{
    public string Text { get; set; }
}

//--------------

public struct Vector3
{
  public Vector3(double x, double y, double z)
  {
  }

  public void Add(Vector3 vector)
  {
  }
}