with Ada.Text_IO;
with Ada.Integer_Text_IO;


procedure Day1
   with SPARK_Mode => On
is
   -- Part 1
   Part_1_Increase_Count : Natural := 0;
   subtype String_Length is Integer range 1 .. 10;
   Previous_Val : Integer;
   Current_Val  : Integer;

   -- Part 2
   type Window_Size is mod 3;
   type Window_T is array (Window_Size) of Integer;

   Window : Window_T := Window_T'(others => Natural'Last);
   Window_Count : Window_Size := 0;

   Previous_Window_Sum : Long_Integer := Long_Integer'Last;
   Current_Window_Sum : Long_Integer;
   Part_2_Increase_Count : Natural := 0;

begin
   Ada.Integer_Text_IO.Get (Previous_Val);
   Window (Window_Count) :=  Previous_Val;
   loop
      Ada.Integer_Text_IO.Get (Current_Val);
         -- Part 1
      if (Current_Val > Previous_Val) and then
        Part_1_Increase_Count < Natural'Last
      then
         Part_1_Increase_Count := Part_1_Increase_Count + 1;
      end if;
      Previous_Val := Current_Val;

      -- Part 2
      Window_Count := Window_Count + 1;
      Window (Window_Count) := Current_Val;

      Current_Window_Sum := 0;
      for I in Window_Size'Range loop
         if Current_Window_Sum + Long_Integer(Window (I)) < Long_Integer'Last then
            Current_Window_Sum := Current_Window_Sum + Long_Integer (Window (I));
         else
            Current_Window_Sum := Long_Integer'Last;
         end if;
      end loop;
      if Current_Window_Sum > Previous_Window_Sum and then
        Part_2_Increase_Count < Natural 'Last
      then
         Part_2_Increase_Count := Part_2_Increase_Count + 1;
      end if;
      Previous_Window_Sum := Current_Window_Sum;

      -- Common
      exit when Ada.Text_IO.End_Of_File;
   end loop;
   Ada.Text_IO.Put_Line(Part_1_Increase_Count'Img & " measurements.");
   Ada.Text_IO.Put_Line(Part_2_Increase_Count'Img & " window measurements.");
end Day1;
