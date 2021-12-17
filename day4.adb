with Ada.Text_IO;
with GNAT.String_Split;

procedure Day4
  with SPARK_Mode => On
is

   subtype Bingo_Numbers is Natural range 0 .. 99;

   subtype Row_Length is Natural range 1 .. 5;
   subtype Col_Length is Natural range 1 .. 5;

   type Row_T is array (Row_Length) of Bingo_Numbers;
   type Bingo_Card_T is array (Col_Length) of Row_T;

   type Row_Mark_T is array (Row_Length) of Boolean;
   type Bingo_Mark_T is array (Col_Length) of Row_Mark_T;
   No_Mark  : constant Bingo_Mark_T := Bingo_Mark_T'(others => Row_Mark_T'(others => False));

   type Marked_Card_T is record
      Card : Bingo_Card_T;
      Mark : Bingo_Mark_T;
   end record;

   Null_Marked_Card_T : constant Marked_Card_T :=
     Marked_Card_T'(Card => Bingo_Card_T'(others => Row_T'(others => 0)),
                    Mark => No_Mark);

   subtype N_Of_Cards_Length is Natural range 0 .. 1000;
   subtype N_Of_Cards_Index is N_Of_Cards_Length range 1 .. N_Of_Cards_Length'Last;

   type Card_List_T is array (N_Of_Cards_Index) of Marked_Card_T;
   type Card_Found_T is array (N_Of_Cards_Index) of Boolean;
   No_Found_Cards : constant Card_Found_T := (others => True);

   Card_list : Card_List_T :=
     (others =>
        Marked_Card_T'(Card => Bingo_Card_T'(others => Row_T'(others => 0)),
                       Mark => No_Mark));
   Number_Of_Cards : N_Of_Cards_Length := 0;
   Found_Cards : Card_Found_T := No_Found_Cards;


   pragma Warnings (GNATprove, Off, "initialization of ""Default_Setting"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Width"" has no effect",
   Reason => "Not using Put statements");
   pragma Warnings (GNATprove, Off, "initialization of ""Default_Base"" has no effect",
   Reason => "Not using Put statements");

   package Numbers_In is new Ada.Text_IO.Integer_IO (Bingo_Numbers);

   pragma Warnings (GNATprove, On, "initialization of ""Default_Setting"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Width"" has no effect");
   pragma Warnings (GNATprove, On, "initialization of ""Default_Base"" has no effect");

   subtype String_Length is Natural       range 0 .. 2000;
   subtype String_Array  is String_Length range 1 .. String_Length'Last;
   Last_Str : String_Length;
   Last_Int : Natural;

   Input_String : String (String_Array);

   Calling_Set : GNAT.String_Split.Slice_Set;
   Card_Line   : GNAT.String_Split.Slice_Set;

   Bingo_Card : Bingo_Card_T := Bingo_Card_T'(others => Row_T'(others => 0));
   Row_Count : Row_Length;

   Called_Number : Bingo_Numbers;

   function Check_Col (Card : Bingo_Mark_T; Row_Num : Row_Length) return Boolean is
     (for all Col in Col_Length'Range => Card (Col) (Row_Num) = True);

   function Total_Unmarked (Card : Marked_Card_T) return Natural is
      Total : Natural := 0;
   begin
      for Col in Col_Length'Range loop
         for Row in Row_Length'Range loop
            pragma Assert (for all I in Col_Length'First .. Col =>
                             (for all J in Row_Length'First .. Row =>
                                (I * J) <= Row_Length'Last * Col_Length'Last));
            pragma Assert (for all I in Col_Length'First .. Col =>
                                     (for all J in Row_Length'First .. Row =>
                                        (if ((Bingo_Numbers'Last * (I * J)) <= 2475) then
                                         Total'Loop_Entry + Card.Card (I) (J) < Natural'Last)));
            if not Card.Mark (Col) (Row) then
               Total := Total + Card.Card (Col) (Row);
            end if;
         end loop;
      end loop;
      return Total;
   end Total_Unmarked;

   First_Card : Marked_Card_T := Null_Marked_Card_T;
   First_Num  : N_Of_Cards_Length := 0;
   Total_Unmarked_P1 : Natural := 0;
   Last_Card  : Marked_Card_T := Null_Marked_Card_T;
   Last_Num   : N_Of_Cards_Length := 0;
   Total_Unmarked_P2 : Natural := 0;

begin
   Ada.Text_IO.Get_Line (Input_String, Last_Str);
   if Last_Str = String_Length'Last then
      Ada.Text_IO.Put_Line ("Did not read to end of line");
   end if;

   GNAT.String_Split.Create (From       => Input_String (String_Array'First .. Last_Str),
                             Separators => ",",
                             S          => Calling_Set);

   Ada.Text_IO.Put_Line (GNAT.String_Split.Slice_Count (Calling_Set)'Img & " calling numbers");

   loop
      Ada.Text_IO.Get_Line (Input_String, Last_Str);

      if Last_Str > 0 then -- Skip newline
         for Col in Col_Length'Range loop
            Row_Count := 1;
            GNAT.String_Split.Create (From       => Input_String (String_Array'First .. Last_Str),
                                      Separators => " ",
                                      Mode       => GNAT.String_Split.Multiple,
                                      S          => Card_Line);

            for Slice in GNAT.String_Split.Slice_Number range 1 .. GNAT.String_Split.Slice_Count (Card_Line) loop
               if GNAT.String_Split.Slice (Card_Line, Slice)'Length > 0 then
                  Numbers_In.Get(From => GNAT.String_Split.Slice (Card_Line, Slice),
                                 Item => Bingo_Card (Col) (Row_Count),
                                 Last => Last_Int);
                  exit when Row_Count = Row_Length'Last;
                  Row_Count := Row_Count + 1;
               end if;
            end loop;
            exit when Ada.Text_IO.End_Of_File;

            Ada.Text_IO.Get_Line (Input_String, Last_Str);

         end loop;

         if Number_Of_Cards < N_Of_Cards_Length'Last then
            Number_Of_Cards := Number_Of_Cards + 1;
            Card_List (Number_Of_Cards) := Marked_Card_T'(Card => Bingo_Card,
                                                          Mark => No_Mark);
            Found_Cards (Number_Of_Cards) := False;
         end if;

      end if;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   -- Read in data, next to play bingo...
   Slice_Loop:
   for Slice in GNAT.String_Split.Slice_Number range 1 .. GNAT.String_Split.Slice_Count (Calling_Set) loop
      Numbers_In.Get (From => GNAT.String_Split.Slice (Calling_Set, Slice),
                      Item => Called_Number,
                      Last => Last_Int);
      for Card in N_Of_Cards_Index range N_Of_Cards_Index'First .. Number_Of_Cards loop
         for Col in Col_Length'Range loop
            for Row in Row_Length'Range loop
               if Called_Number = Card_List (Card).Card (Col) (Row) then
                  Card_List (Card).Mark (Col) (Row) := True;
               end if;
               if Check_Col (Card_List (Card).Mark, Row) then
                  if First_Card = Null_Marked_Card_T then
                     First_Card := Card_List (Card);
                     First_Num := Called_Number;
                     Ada.Text_IO.Put_Line ("Setting Card" & Card'Img);

                  end if;
                  Found_Cards (Card) := True;
               end if;
            end loop;
            if Card_List (Card).Mark (Col) = Row_Mark_T'(others => True) then
               if First_Card = Null_Marked_Card_T then
                  First_Card := Card_List (Card);
                  First_Num := Called_Number;
                  Ada.Text_IO.Put_Line ("Setting Card" & Card'Img);

               end if;
               Found_Cards (Card) := True;

            end if;
            if Found_Cards = No_Found_Cards then
               Last_Card := Card_List (Card);
               Last_Num := Called_Number;
               Ada.Text_IO.Put_Line ("Last Card" & Card'Img);
               exit Slice_Loop;
            end if;

         end loop;
      end loop;
   end loop Slice_Loop;


   if First_Card /= Null_Marked_Card_T then
      Total_Unmarked_P1 := Total_Unmarked (First_Card);
   end if;
   if Last_Card /= Null_Marked_Card_T then
      Total_Unmarked_P2 := Total_Unmarked (Last_Card);
   end if;

   Ada.Text_IO.Put_Line ("Total: " & Total_Unmarked_P1'Img);
   Ada.Text_IO.Put_Line ("Part 1:" & Long_Integer(Total_Unmarked_P1 * First_Num)'Img);
   Ada.Text_IO.Put_Line ("Part 2:" & Long_Integer(Total_Unmarked_P2 * Last_Num)'Img);




end Day4;
