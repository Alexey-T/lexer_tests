FUNCTION Lock_By_Id___ (
   objid_      IN  VARCHAR2,
   objversion_ IN  VARCHAR2 ) RETURN &TABLE%ROWTYPE
IS
   row_changed EXCEPTION;
   row_deleted EXCEPTION;
   row_locked  EXCEPTION;
   PRAGMA      exception_init(row_locked, -0054);
   rec_        &TABLE%ROWTYPE;
   dummy_      NUMBER;
   CURSOR lock_control IS
      SELECT *
      FROM   &TABLE
      WHERE  &OBJID = objid_
      AND    &OBJVERSION = objversion_
      FOR UPDATE NOWAIT;
   CURSOR exist_control IS
      SELECT 1
      FROM   &TABLE
      WHERE  &OBJID = objid_;
BEGIN
   OPEN lock_control;
   FETCH lock_control INTO rec_;
   IF (lock_control%FOUND) THEN
      CLOSE lock_control;
      RETURN rec_;
   END IF;
   CLOSE lock_control;
   OPEN exist_control;
   FETCH exist_control INTO dummy_;
   IF (exist_control%FOUND) THEN
      CLOSE exist_control;
      RAISE row_changed;
   ELSE
      CLOSE exist_control;
      RAISE row_deleted;
   END IF;
EXCEPTION
   WHEN row_locked THEN
      Error_SYS.Record_Locked(lu_name_);
   WHEN row_changed THEN
      Error_SYS.Record_Modified(lu_name_);
   WHEN row_deleted THEN
      Error_SYS.Record_Removed(lu_name_);
END Lock_By_Id___;

FUNCTION Lock_By_Keys___ (
   operation_order_no_ IN VARCHAR2,
   operation_order_line_no_ IN NUMBER ) RETURN &TABLE%ROWTYPE
IS
   row_deleted EXCEPTION;
   rec_        &TABLE%ROWTYPE;
   CURSOR lock_control IS
      SELECT *
      FROM  &TABLE
      WHERE operation_order_no = operation_order_no_
      AND   operation_order_line_no = operation_order_line_no_
      FOR UPDATE;
BEGIN
   OPEN lock_control;
   FETCH lock_control INTO rec_;
   IF (lock_control%FOUND) THEN
      CLOSE lock_control;
      RETURN rec_;
   ELSE
      CLOSE lock_control;
      RAISE row_deleted;
   END IF;
EXCEPTION
   WHEN row_deleted THEN
      Error_SYS.Record_Removed(lu_name_);
END Lock_By_Keys___;

FUNCTION Get_Object_By_Id___ (
   objid_ IN VARCHAR2 ) RETURN &TABLE%ROWTYPE
IS
   lu_rec_ &TABLE%ROWTYPE;
   CURSOR getrec IS
      SELECT *
      FROM   &TABLE
      WHERE  &OBJID = objid_;
BEGIN
   OPEN getrec;
   FETCH getrec INTO lu_rec_;
   IF (getrec%NOTFOUND) THEN
      CLOSE getrec;
      Error_SYS.Record_Removed(lu_name_);
   END IF;
   CLOSE getrec;
   RETURN(lu_rec_);
END Get_Object_By_Id___;
