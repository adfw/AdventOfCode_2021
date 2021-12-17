with Ada.Containers.Formal_Vectors;
with Ada.Text_IO;
 
use type Ada.Containers.Count_Type;

procedure Day3 
   with SPARK_Mode => On
is
   
   subtype Width is Natural range 0 .. 32;
   type Rate_T is mod (2 ** Width'Last);

   type Count_T is array (Width) of Rate_T;


   subtype Reading_Len_T is Natural range 0 .. 2000;

   package Inputs_T is new Ada.Containers.Formal_Vectors
      (Index_Type   => Reading_Len_T,
       Element_Type => Rate_T);

   pragma Warnings (GNATprove, Off, "initialization of ""Default_Setting"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Width"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Base"" has no effect",
   Reason => "Not using Put statements");
   
   package Diag_In is new Ada.Text_IO.Integer_IO (Natural);

   pragma Warnings (GNATprove, On, "initialization of ""Default_Setting"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Width"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Base"" has no effect");

   Zero_Count : Count_T := (others => 0);
   One_Count  : Count_T := (others => 0);

   Oxygen_R : Inputs_T.Vector (2000);
   Scrubber_R : Inputs_T.Vector (2000);
   
   Gamma   : Rate_T := 0;
   Epsilon : Rate_T := 0;
   Input_Len : Rate_T := 0;

   Current_Val : Natural;
   Convert_Val : Rate_T;
   subtype String_Length is Natural       range 0 .. 20;
   subtype String_Array  is String_Length range 1 .. String_Length'Last;
   Last_Str : String_Length;
   Last_Int : Natural;

   Dec_String : String (String_Array);
   
   
   procedure Part2_Deletions
     (Ratings : in out Inputs_T.Vector;
      Oxygen  : in     Boolean;
      Bit     : in     Width)
   is
      Zero_Count : Rate_T := 0;
      One_Count  : Rate_T := 0;
   begin
      if Inputs_T.Length (Ratings) > 1 then
         for J in Inputs_T.First_Index (Ratings) .. Inputs_T.Last_Index (Ratings) loop
           if (Inputs_T.Element (Ratings, Integer(J)) and 2 ** Bit) = (2 ** Bit) then
              One_Count := One_Count + 1;
           else
              Zero_Count := Zero_Count + 1;
           end if;
         end loop;
         if Oxygen then
            for J in reverse Inputs_T.First_Index (Ratings) .. Inputs_T.Last_Index (Ratings) loop

               if One_Count >= Zero_Count
               then
                  if Inputs_T.Has_Element (Ratings, J) and then (Inputs_T.Element (Ratings, Integer(J)) and 2 ** Bit) /= (2 ** Bit) then
                     Inputs_T.Delete (Ratings, Integer(J)); 
                  end if;
               else
                  if Inputs_T.Has_Element (Ratings, J) and then (Inputs_T.Element (Ratings, Integer(J)) and 2 ** Bit) = (2 ** Bit) then
                     Inputs_T.Delete (Ratings, Integer(J));
                  end if;
               end if;
            end loop;
            
         else
            -- Slightly different for the "lowest common" case as we need to handle the default all zero case for zero-bits.
            if (Zero_Count < Rate_T (Inputs_T.Length (Ratings))) and then One_Count < Zero_Count then
               for J in reverse Inputs_T.First_Index (Ratings) .. Inputs_T.Last_Index (Ratings) loop
                  if Inputs_T.Has_Element (Ratings, J) and then (Inputs_T.Element (Scrubber_R, Integer(J)) and 2 ** Bit) /= (2 ** Bit) then
                     Inputs_T.Delete (Ratings, Integer(J));
                  end if;
               end loop;
            else
               for J in reverse Inputs_T.First_Index (Ratings) .. Inputs_T.Last_Index (Ratings) loop
                  if Inputs_T.Has_Element (Ratings, J) and then (Inputs_T.Element (Ratings, Integer(J)) and 2 ** Bit) = (2 ** Bit) then
                     Inputs_T.Delete (Ratings, Integer(J));
                  end if;
               end loop;
            end if;
         end if;
      end if;

   end Part2_Deletions;

begin
   loop
      pragma Loop_Invariant (Inputs_T.Length (Oxygen_R) = Inputs_T.Length (Scrubber_R));
      
      Ada.Text_IO.Get_Line (Dec_String, Last_Str);
      Diag_In.Get (From => "2#" & Dec_String (1 .. Last_Str) & "#", Item => Current_Val, Last => Last_Int);
      if Last_Int /= Last_Str + 3 then
         Ada.Text_IO.Put_Line ("Potential read error - read" & Last_Str'Img & 
                               " characters, but only converted" & Last_Int'Img);
      end if;
      
      Convert_Val := Rate_T (Current_Val);
      for I in Width range Width'First .. Width'Last -1  loop
         if (Convert_Val and 2 ** I) = (2 ** I) then
            One_Count (I) := One_Count (I) + 1;
         else
            Zero_Count (I) := Zero_Count (I) + 1;
         end if;
      end loop;

      if Inputs_T.Length (Oxygen_R) < Inputs_T.Capacity (Oxygen_R) then
         Inputs_T.Append (Oxygen_R, Convert_Val);
         Inputs_T.Append (Scrubber_R, Convert_Val);
      end if;

      Input_Len := Input_Len + 1;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   for I in reverse Width'Range loop
      if Zero_Count (I) /= Input_Len then
         -- Assume we get at least one bit set
         if Zero_Count (I) < One_Count (I) then
            Gamma := Gamma + (2 ** I);
         else
            Epsilon := Epsilon + (2 ** I);
         end if;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Gamma" & Gamma'Img);
   Ada.Text_IO.Put_Line ("Epsilon" & Epsilon'Img);
   Ada.Text_IO.Put_Line ("Part 1" & Long_Integer (Gamma * Epsilon)'Img);

   for I in reverse 0 .. 31 loop
      --  One_Count := (others => 0);
      --  Zero_Count := (others => 0);
      --  if Inputs_T.Length (Oxygen_R) > 1 then
      --     for J in Inputs_T.First_Index (Oxygen_R) .. Inputs_T.Last_Index (Oxygen_R) loop
      --       if (Inputs_T.Element (Oxygen_R, Integer(J)) and 2 ** I) = (2 ** I) then
      --          One_Count (I) := One_Count (I) + 1;
      --       else
      --          Zero_Count (I) := Zero_Count (I) + 1;
      --       end if;
      --     end loop;
      --     if One_Count (I) >= Zero_Count (I) then
      --        for J in reverse Inputs_T.First_Index (Oxygen_R) .. Inputs_T.Last_Index (Oxygen_R) loop
      --           if Inputs_T.Has_Element (Oxygen_R, J) and then (Inputs_T.Element (Oxygen_R, Integer(J)) and 2 ** I) /= (2 ** I) then
      --              Inputs_T.Delete (Oxygen_R, Integer(J));
      --           end if;
      --        end loop;
      --     else
      --        for J in reverse Inputs_T.First_Index (Oxygen_R) .. Inputs_T.Last_Index (Oxygen_R) loop
      --           if Inputs_T.Has_Element (Oxygen_R, J) and then (Inputs_T.Element (Oxygen_R, Integer(J)) and 2 ** I) = (2 ** I) then
      --              Inputs_T.Delete (Oxygen_R, Integer(J));
      --           end if;
      --        end loop;
      --     end if;
      --  end if;
      Part2_Deletions (Ratings => Oxygen_R,
                       Oxygen  => True,
                       Bit     => I);
      Part2_Deletions (Ratings => Scrubber_R,
                       Oxygen  => False,
                       Bit     => I);
      --  One_Count := (others => 0);
      --  Zero_Count := (others => 0);
      --  
      --  if Inputs_T.Length (Scrubber_R) > 1 then
      --     for J in Inputs_T.First_Index (Scrubber_R) .. Inputs_T.Last_Index (Scrubber_R) loop
      --       if (Inputs_T.Element (Scrubber_R, Integer(J)) and 2 ** I) = (2 ** I) then
      --          One_Count (I) := One_Count (I) + 1;
      --       else
      --          Zero_Count (I) := Zero_Count (I) + 1;
      --       end if;
      --     end loop;
      --     if (Zero_Count (I) < Rate_T (Inputs_T.Length (Scrubber_R))) and then One_Count (I) < Zero_Count (I) then
      --        for J in reverse Inputs_T.First_Index (Scrubber_R) .. Inputs_T.Last_Index (Scrubber_R) loop
      --           if Inputs_T.Has_Element (Scrubber_R, J) and then (Inputs_T.Element (Scrubber_R, Integer(J)) and 2 ** I) /= (2 ** I) then
      --              Inputs_T.Delete (Scrubber_R, Integer(J));
      --           end if;
      --        end loop;
      --     else
      --        for J in reverse Inputs_T.First_Index (Scrubber_R) .. Inputs_T.Last_Index (Scrubber_R) loop
      --           if Inputs_T.Has_Element (Scrubber_R, J) and then (Inputs_T.Element (Scrubber_R, Integer(J)) and 2 ** I) = (2 ** I) then
      --              Inputs_T.Delete (Scrubber_R, Integer(J));
      --           end if;
      --        end loop;
      --     end if;
      --  end if;
   end loop;
   if Inputs_T.Length (Oxygen_R) = 1 and then Inputs_T.Length (Scrubber_R) = 1 then
      Ada.Text_IO.Put_Line ("Oxygen" & Inputs_T.Element (Oxygen_R, 0)'Img);
      Ada.Text_IO.Put_Line ("Scrubber" & Inputs_T.Element (Scrubber_R, 0)'Img);
      Ada.Text_IO.Put_Line ("Part 2:" & Long_Integer (Inputs_T.Element (Oxygen_R, 0) * Inputs_T.Element (Scrubber_R, 0))'Img);
   end if;
end Day3;
