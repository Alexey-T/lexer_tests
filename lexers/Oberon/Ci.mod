MODULE Circle;
   IMPORT Shape, Out;

   TYPE
      Circle* = POINTER TO CircleDesc;
      CircleDesc* = RECORD(Shape.ShapeDesc)
         radius: INTEGER;
      END;

   (* accessors for radius *)
   PROCEDURE (self: Circle)getRadius*(): INTEGER;
   BEGIN
      RETURN self.radius;
   END getRadius;

   PROCEDURE (self: Circle)setRadius*(newradius: INTEGER);
   BEGIN
      self.radius := newradius;
   END setRadius;

   (* allocate and initialize a new object instance *)
   PROCEDURE Make*(x: INTEGER; y: INTEGER; radius: INTEGER): Circle;
   VAR self: Circle;
   BEGIN
      NEW(self);
      self^.moveTo(x, y);
      self^.setRadius(radius);
      RETURN self;
   END Make;

   (* draw the circle *)
   PROCEDURE (self: Circle)draw*();
   BEGIN
      Out.String('Drawing a Circle at:(');
      Out.Int(self^.getX(), 1);
      Out.String(',');
      Out.Int(self^.getY(), 1);
      Out.String('), radius ');
      Out.Int(self^.getRadius(), 1);
      Out.Ln;
   END draw;

END Circle.