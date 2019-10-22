MODULE Shape;

   TYPE
      Shape* = POINTER TO ShapeDesc;
      ShapeDesc* = RECORD
         x: INTEGER;
         y: INTEGER;
      END;

   (* accessors for x & y *)
   PROCEDURE (self: Shape)getX*(): INTEGER;
   BEGIN
      RETURN self.x;
   END getX;

   PROCEDURE (self: Shape)getY*(): INTEGER;
   BEGIN
      RETURN self.y;
   END getY;

   PROCEDURE (self: Shape)setX(newx: INTEGER);
   BEGIN
      self.x := newx;
   END setX;

   PROCEDURE (self: Shape)setY(newy: INTEGER);
   BEGIN
      self.y := newy;
   END setY;

   (* move the shape position *)
   PROCEDURE (self: Shape)moveTo*(newx: INTEGER; newy: INTEGER);
   BEGIN
      self^.setX(newx);
      self^.setY(newy);
   END moveTo;

   PROCEDURE (self: Shape)rMoveTo*(deltax: INTEGER; deltay: INTEGER);
   BEGIN
      self^.moveTo(self^.getX() + deltax, self^.getY() + deltay);
   END rMoveTo;

   (* abstract draw method *)
   PROCEDURE (self: Shape)draw*();
   BEGIN
   END draw;

END Shape.