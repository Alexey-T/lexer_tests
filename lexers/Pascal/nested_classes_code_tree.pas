unit Example;

interface

type
      TOuterClass = class
       strict private
          myField: Integer;

       public type
             TInnerClass = class
              public type
                TInner2Class = class
                public
                  procedure inner2Proc(dd: int);
                end;
                myInnerField: Integer;
                procedure innerProc;
             end;

         procedure outerProc;
       end;

implementation

procedure TOuterClass.TInnerClass.TInner2Class.inner2Proc(var n: int);
begin
end;

procedure TOuterClass.TInnerClass.innerProc;
begin
end;

procedure TOuterClass.outerProc;
begin
end;

end.
