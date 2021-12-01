with Ada.Text_IO;

procedure Day1
is

   -- Part 1
   Part_1_Increase_Count : Natural := 0;
   Previous_Value : Natural := Natural'Value(Ada.Text_IO.Get_Line);

   -- Part 2
   type Window_Size is mod 3;
   type Window_T is array (Window_Size) of Natural;

   Window : Window_T := Window_T'(others => Natural'Last);
   Window_Count : Window_Size := 0;

   Previous_Window_Sum : Natural := Natural'Last;
   Current_Window_Sum : Natural;
   Part_2_Increase_Count : Natural := 0;

begin
   Window (Window_Count) :=  Previous_Value;
   loop
      declare
         Value : constant Natural := Natural'Value(Ada.Text_IO.Get_Line);
      begin
         -- Part 1
         if (Value > Previous_Value) then
            Part_1_Increase_Count := Part_1_Increase_Count + 1;
         end if;
         Previous_Value := Value;

         -- Part 2
         Window_Count := Window_Count + 1;
         Window (Window_Count) := Value;

         Current_Window_Sum := 0;
         for I in Window_Size'Range loop
            if Long_Integer(Current_Window_Sum + Window (I)) < Long_Integer(Natural'Last) then
               Current_Window_Sum := Current_Window_Sum + Window (I);
            else
               Current_Window_Sum := Natural'Last;
            end if;
         end loop;
         if Current_Window_Sum > Previous_Window_Sum then
            Part_2_Increase_Count := Part_2_Increase_Count + 1;
         end if;
         Previous_Window_Sum := Current_Window_Sum;

         -- Common
         exit when Ada.Text_IO.End_Of_File;
      end;
   end loop;
   Ada.Text_IO.Put_Line(Part_1_Increase_Count'Img & " measurements.");
   Ada.Text_IO.Put_Line(Part_2_Increase_Count'Img & " window measurements.");
end Day1;
