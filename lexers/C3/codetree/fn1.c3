struct Vec2
{
    int x, y;
}

fn Vec2 Vec2.add(self, Vec2 other) @operator(+)
{
    return { self.x + other.x, self.y + other.y };
}

fn Vec2 Vec2.sub(self, Vec2 other) @operator(-)
{
    return { self.x - other.x, self.y - other.y };
}

fn void main()
{
    Vec2 v1 = { 1, 2 };
    Vec2 v2 = { 100, 4 };
    Vec2 v3 = v1 + v2;    // v3 = { 101, 6 }
}