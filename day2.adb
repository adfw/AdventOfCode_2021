with Ada.Text_IO;

procedure Day2
   with SPARK_Mode => On 
is
   type Command is
      (Forward,
       Down,
       Up);

   subtype Coord_Units is Long_Integer range 0 .. Long_Integer(Integer'Last);

   pragma Warnings (GNATprove, Off, "initialization of ""Default_Setting"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Width"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Base"" has no effect",
   Reason => "Not using Put statements");
   
   package Command_IO is new Ada.Text_IO.Enumeration_IO (Command);
   package Units is new Ada.Text_IO.Integer_IO (Coord_Units);

   pragma Warnings (GNATprove, On, "initialization of ""Default_Setting"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Width"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Base"" has no effect");


   Depth_P1 : Coord_Units := 0;
   Depth_P2 : Coord_Units := 0;
   Horiz : Coord_Units := 0;
   Aim   : Coord_Units := 0;

   Current_Val : Coord_Units;
   Current_Cmd : Command;

begin
   loop
      Command_IO.Get(Current_Cmd);
      Units.Get (Current_Val);
      case Current_Cmd is
         when Forward =>
            if (Coord_Units'Last - Current_Val) > Horiz then
               Horiz := Horiz + Current_Val;
            end if;
            if (Coord_Units'Last - (Aim * Current_Val)) > Depth_P2 then
               Depth_P2 := Depth_P2 + (Aim * Current_Val);
            end if;
         when Down =>
            if (Coord_Units'Last - Current_Val) >= Depth_P1 then
               Depth_P1 := Depth_P1 + Current_Val;
            end if;
            if (Coord_Units'Last - Current_Val) >= Aim then
               Aim := Aim + Current_Val;
            end if;
         when Up =>
            if (Coord_Units'First + Current_Val) <= Depth_P1 then
               Depth_P1 := Depth_P1 - Current_Val;
            end if;
            if (Coord_Units'First + Current_Val) <= Aim then
               Aim := Aim - Current_Val;
            end if;
      end case;
      exit when Ada.Text_IO.End_Of_File;
   end loop;
   Ada.Text_IO.Put_Line ("Part 1:" & Long_Integer'(Depth_P1 * Horiz)'Img);
   Ada.Text_IO.Put_Line ("Part 2:" & Long_Integer'(Depth_P2 * Horiz)'Img);
end Day2;
