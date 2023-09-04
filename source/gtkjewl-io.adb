------------------------------------------------------------------------------
--
--                     G T K J E W L .  I O
--
-- A package for developing GUI-based programs for beginners.
--
-- This is a large package, but splitting it into child packages would
-- require multiple generic instantiations in order to use it.
--
-- Universitat Rovira i Virgili
-- Departament d'Enginyeria Informatica i Matematiques
-- PFC 2004-05 by Luis Miguel Sanz Gutierrez
--
-- This file is part of GtkJewl
-- 
-- GtkJewl is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- GtkJewl is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with GtkJewl; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
------------------------------------------------------------------------------

with GtkJewl.Simple_Windows;   use GtkJewl.Simple_Windows;

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;

package body GtkJewl.IO is

  ----------------------------------------------------------------------------
  --  Command codes
  ----------------------------------------------------------------------------

  OK     : constant Character := 'Y';
  Cancel : constant Character := 'Q';
  Yes    : constant Character := 'Y';
  No     : constant Character := 'N';

  ----------------------------------------------------------------------------
  --  Renaming of Trim function (for convenience)
  ----------------------------------------------------------------------------

  function Trim (Source : String;
                 Side   : Ada.Strings.Trim_End := Ada.Strings.Both)
           return  String
           renames Ada.Strings.Fixed.Trim;

  ----------------------------------------------------------------------------
  --
  --             M E S S A G E   B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Error: Display an error message box and echo the message to the
  --         standard output.
  --
  procedure Error (Text : in String) is
  begin        
    Show_Error (Text, "Error");
  end Error;

  ----------------------------------------------------------------------------
  --
  --  Query: display a query message box and echo the message and response
  --         to the standard output.
  --
  function Query (Text : in String) return Boolean is
    B : Boolean;
  begin
    Ada.Text_io.Put ("Query: " & Text & ' ');
    B := Show_Query (Text, "Query");
    if B then
      Ada.Text_io.Put_Line ("YES");
    else
      Ada.Text_io.Put_Line ("NO");
    end if;
    return B;
  end Query;

  ----------------------------------------------------------------------------
  --
  --  Message: display an information message box and echo the message to
  --           the standard output.
  --
  procedure Message (Text : in String) is
  begin
    Ada.Text_io.Put_Line ("Message: " & Text);
    Show_Message (Text, "Message");
  end Message;

  ----------------------------------------------------------------------------
  --
  --  Warning: display an information message box and echo the message to
  --           the standard output.
  --
  procedure Warning (Text : in String) is
  begin
    Ada.Text_io.Put_Line ("Warning: " & Text);
    Show_Warning (Text, "Warning");
  end Warning;

  ----------------------------------------------------------------------------
  --
  --                S T R I N G   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a string, echoing the input to the
  --       standard output. The input string is returned as the result
  --       of the function, or Input_Cancelled is raised if the Cancel
  --       button is pressed.
  --
  function Get (Prompt  : in  String := "Enter your text:";
                Default : in  String := "")
           return String is
    D : Dialog_Type  := Dialog (300, 150, "Input required", Cancel);
    P : Label_Type   := Label   (D, (15,15), -30, 30, Prompt);
    E : Editbox_Type := Editbox (D, (15,45), -30, 20, Default);
    Y : Button_Type  := Button  (D, (55,80), 80, 25, "OK", OK, True);
    N : Button_Type  := Button  (D, (165,80), 80, 25, "Cancel", Cancel);
  begin
    -- Echo the prompt, then execute the dialog

    Ada.Text_io.Put (Prompt & ' ');
    if Execute(D) = OK then

      -- OK pressed: get the string from the dialog's editbox, echo it
      -- to the standard output and return it

      declare
        S : constant String := Get_Text(E);
      begin
        Ada.Text_io.Put_Line(S);
        return S;
      end;

    else

      -- Cancel pressed: raise an exception

      raise Input_Cancelled;

    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a string, echoing the input to the
  --       standard output. The string and its length are returned in
  --       the parameters provided.
  --
  procedure Get (Item    : out String;
                 Length  : out Natural;
                 Prompt  : in  String := "Enter your text:";
                 Default : in  String := "") is
    S : constant String := Get(Prompt,Default);
  begin
    if S'Length < Item'Length then

      -- String will fit into variable provided, so copy it padded
      -- with spaces

      Length := S'Length;
      Item := (others => ' ');
      Item(Item'First..Item'First+Length-1) := S;
      Ada.Text_io.Put_Line(S);

    else

      -- String is too long, so truncate it to fit the size available

      Length := Item'Length;
      Item := S(S'First..S'First+Length-1);
      Ada.Text_io.Put_Line(Item);

    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a character and newline to the standard output.
  --
  procedure Put_Line (Item : in Character) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a character and newline to the specified file.
  --
  procedure Put_Line (File : in Ada.Text_IO.File_Type;
                      Item : in Character) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --               I N T E G E R   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get an Integer value and echo it to the
  --       standard output.
  --
  procedure Get (Item   : out Integer;
                 Prompt : in  String := "Enter an integer:") is
  begin
    loop
      begin
        Item := Integer'Value(Get(Prompt,""));
        return;
      exception
        when Constraint_Error =>
          GtkJewl.IO.Error("You must enter an integer.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get an Integer value using the supplied
  --       default value and echo it to the standard output.
  --
  procedure Get (Item    : out Integer;
                 Default : in  Integer;
                 Prompt  : in  String := "Enter an integer:") is
  begin
    loop
      begin
        Item := Integer'Value(Get(Prompt,Trim(Integer'Image(Default))));
        return;
      exception
        when Constraint_Error =>
          GtkJewl.IO.Error("You must enter an integer.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get an Integer value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Integer) is
    package IO is new Ada.Text_IO.Integer_IO (Integer);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write an Integer value to the standard output.
  --
  procedure Put (Item : in Integer) is
  begin
    Ada.Text_io.Put (To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write an Integer value to the specified file.
  --
  procedure Put (File : in Ada.Text_IO.File_Type;
                 Item : in Integer) is
  begin
    Ada.Text_io.Put (File, To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write an Integer value and newline to the standard output.
  --
  procedure Put_Line (Item : in Integer) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write an Integer value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Integer) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert an Integer value to a string.
  --
  function To_String (Item : Integer) return String is
  begin
    return Trim(Integer'Image(Item));
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and an Integer value.
  --
  function "&" (Left  : String;
                Right : Integer) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate an Integer value and a string.
  --
  function "&" (Left  : Integer;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --                 F L O A T   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Float value and echo it to the standard
  --       output.
  --
  procedure Get (Item   : out Float;
                 Prompt : in  String := "Enter a number:") is
  begin
    loop
      begin
        Item := Float'Value(Get(Prompt,""));
        return;
      exception
        when Constraint_Error =>
          GtkJewl.IO.Error("You must enter a number.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Float value using the supplied default
  --       value and echo it to the standard output.
  --
  procedure Get (Item    : out Float;
                 Default : in  Float;
                 Prompt  : in  String := "Enter a number:") is
  begin
    loop
      begin
        Item := Float'Value(Get(Prompt,To_String(Default)));
        return;
      exception
        when Constraint_Error =>
          GtkJewl.IO.Error("You must enter a number.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get a Float value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Float) is
    package IO is new Ada.Text_IO.Float_IO (Float);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Float value to the standard output.
  --
  procedure Put (Item : in  Float) is
  begin
    Ada.Text_io.Put(To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Float value to the specified file.
  --
  procedure Put (File : in File_Type;
                 Item : in Float) is
  begin
    Ada.Text_io.Put(File,To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Float value and newline to the standard output.
  --
  procedure Put_Line (Item : in Float) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Float value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Float) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert a Float value to a string.
  --
  function To_String (Item : Float) return String is
    S : String(1..Float'Width);
    N : Integer;
  begin
    Ada.Float_Text_IO.Put(S,Item,Exp=>0);
    for I in reverse S'Range loop
      N := I;
      exit when S(I) /= '0';
    end loop;
    if S(N) = '.' then
      N := N + 1;
    end if;
    return Trim(S(1..N));
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and a Float value.
  --
  function "&" (Left  : String;
                Right : Float) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a Float value and a string.
  --
  function "&" (Left  : Float;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --               B O O L E A N   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Boolean value and echo it to the
  --       standard output. The dialog box uses Yes, No and Cancel
  --       buttons; Yes and No return true and false respectively,
  --       and Cancel raises an Input_Cancelled exception.
  --
  procedure Get (Item   : out Boolean;
                 Prompt : in  String := "Yes or no?") is
    D : Dialog_Type  := Dialog (330, 115, "Input required", Cancel);
    P : Label_Type   := Label   (D, (15,15), -30, 30, Prompt);
    Y : Button_Type  := Button  (D, (20,55), 80, 25, "&Yes", Yes, True);
    N : Button_Type  := Button  (D, (115,55), 80, 25, "&No", No);
    C : Button_Type  := Button  (D, (215,55), 80, 25, "Cancel", Cancel);
    E : Character;
  begin
    Ada.Text_io.Put (Prompt & ' ');
    E := Execute(D);
    if E /= Cancel then
      Item := (E = OK);
      Put (Item);
      New_Line;
    else
      raise Input_Cancelled;
    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Boolean value using the supplied
  --       default and echo it to the standard output. The dialog box
  --       uses a checkbox set to the default value.
  --
  procedure Get (Item    : out Boolean;
                 Default : in  Boolean;
                 Prompt  : in  String := "Yes") is
    D : Dialog_Type   := Dialog (350, 115, "Input required", Cancel);
    C : Checkbox_Type := Checkbox (D, (15,15), -30, 30, Prompt);
    Y : Button_Type   := Button  (D, (70,55), 80, 25, "OK", OK, True);
    N : Button_Type   := Button  (D, (185,55), 80, 25, "Cancel", Cancel);
  begin
    Ada.Text_io.Put(Prompt & ' ');
    Set_State (C, Default);
    if Execute(D) = OK then
      Item := Get_State (C);
      Put(Item);
      New_Line;
    else
      raise Input_Cancelled;
    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get a Boolean value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Boolean) is
    package IO is new Ada.Text_IO.Enumeration_IO (Boolean);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Boolean value to the standard output.
  --
  procedure Put (Item : in  Boolean) is
  begin
    Ada.Text_io.Put(To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Boolean value to the specified file.
  --
  procedure Put (File : in File_Type;
                 Item : in Boolean) is
  begin
    Ada.Text_io.Put(File,To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Boolean value and newline to the standard output.
  --
  procedure Put_Line (Item : in Boolean) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Boolean value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Boolean) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert a Boolean value to a string.
  --
  function To_String (Item : Boolean) return String is
  begin
    return Boolean'Image(Item);
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and a Boolean value.
  --
  function "&" (Left  : String;
                Right : Boolean) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a Boolean value and a string.
  --
  function "&" (Left  : Boolean;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --   G E N E R I C   E N U M E R A T I O N   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Enumeration_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value using the supplied
    --       default and echo it to the standard output. The dialog box uses
    --       a combobox filled with a list of the possible values. This is
    --       an internal procedure used to implement the two Get procedures
    --       below.
    --
    procedure Get (Prompt  : in  String;
                   Default : in  Integer;
                   Item    : out Item_Type) is
      D : Dialog_Type   := Dialog (300, 150, "Input required", Cancel);
      P : Label_Type    := Label    (D, (15,15), -30, 30, Prompt);
      C : Combobox_Type := Combobox (D, (15,45), -30);
      Y : Button_Type   := Button   (D, (45,85), 80, 25, "OK", OK, True);
      N : Button_Type   := Button   (D, (155,85), 80, 25, "Cancel", Cancel);
    begin
      -- Fill the combo box with the list of values

      for N in Item_Type'Range loop
        Append_Line (C, Item_Type'Image(N));
      end loop;

      -- Select the default value if there is one (Default < 0 if not)

      if Default >= 0 then
        Select_Line (C, Default+1);
      end if;

      -- Echo the prompt to the standard output and execute the dialog

      Ada.Text_io.Put(Prompt & ' ');
      if Execute(D) = OK then

        -- OK pressed: get the selection and convert it to the target type,
        -- then echo it to the standard output

        Item := Item_Type'Val(Get_Line(C)-1);
        Put(Item);
        New_Line;

      else

        -- Cancel pressed: raise an exception

        raise Input_Cancelled;

      end if;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value and echo it to the
    --       standard output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Choose a value:") is
    begin
      Get (Prompt, -1, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value using the supplied
    --       default and echo it to the standard output.
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Choose a value:") is
    begin
      Get (Prompt,
           Item_Type'Pos(Default)-Item_Type'Pos(Item_Type'First),
           Item);
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get an enumerated value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Enumeration_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write an enumerated value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_io.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write an enumerated value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_io.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write an enumerated value and newline to the standard
    --            output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write an enumerated value and newline to the specified
    --            file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert an enumerated value to a string.
    --
    function To_String (Item : Item_Type) return String is
    begin
      return Item_Type'Image(Item);
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and an enumerated value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate an enumerated value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Enumeration_IO;

  ----------------------------------------------------------------------------
  --
  --       G E N E R I C   I N T E G E R   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Integer_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value and echo it to the standard
    --       output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Enter an integer:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,""));
          return;
        exception
          when Constraint_Error =>
            GtkJewl.IO.Error("You must enter an integer between " &
                              Trim(Item_Type'Image(Item_Type'First)) &
                              " and " &
                              Trim(Item_Type'Image(Item_Type'Last)));
        end;
      end loop;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value using the supplied default
    --       and echo it to the standard output
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter an integer:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,Trim(Item_Type'Image(Default))));
          return;
        exception
          when Constraint_Error =>
            GtkJewl.IO.Error("You must enter an integer between " &
                              To_String(Item_Type'First) &
                              " and " &
                              To_String(Item_Type'Last));
        end;
      end loop;
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get a value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Integer_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_io.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_io.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the standard output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the specified file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert a value to a string.
    --
    function To_String (Item : Item_Type) return String is
    begin
      return Trim(Item_Type'Image(Item));
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and a value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Integer_IO;

  ----------------------------------------------------------------------------
  --
  --          G E N E R I C   F L O A T   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Float_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value and echo it to the standard
    --       output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Enter a number:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,""));
          return;
        exception
          when Constraint_Error =>
            GtkJewl.IO.Error("You must enter a number between " &
                              To_String(Item_Type'First) &
                              " and " &
                              To_String(Item_Type'Last));
        end;
      end loop;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value using the supplied default
    --       and echo it to the standard output.
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter a number:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,
                                      To_String(Float(Default))));
          return;
        exception
          when Constraint_Error =>
            GtkJewl.IO.Error("You must enter a number between " &
                              To_String(Float(Item_Type'First)) &
                              " and " &
                              To_String(Float(Item_Type'Last)));
        end;
      end loop;
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get a value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Float_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_io.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_io.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the standard output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the specified file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert a value to a string.
    --
    function To_String (Item : Item_Type) return String is
      S : String(1..Item_Type'Width);
      N : Integer;
      package Float_IO is new Ada.Text_IO.Float_IO (Item_Type);
    begin
      Float_IO.Put(S,Item,Exp=>0);
      for I in reverse S'Range loop
        N := I;
        exit when S(I) /= '0';
      end loop;
      if S(N) = '.' then
        N := N + 1;
      end if;
      return Trim(S(1..N));
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and a value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Float_IO;

  ----------------------------------------------------------------------------
  --
  --  Open: use a dialog box to get the name of the file to open and
  --        open it for input.
  --
  procedure Open (File  : in out File_Type;
                  Title : in String := "Select input file") is
    D : Open_Dialog_Type := Open_Dialog (Title);
  begin
    if Execute(D) then
      Ada.Text_io.Put_Line("Opening " & Get_Name(D));
      Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File,
                              Name => Get_Name(D));
    else
      raise Input_Cancelled;
    end if;
  end Open;

  ----------------------------------------------------------------------------
  --
  --  Create: use a dialog box to get the name of the file to open and
  --          open it for output.
  --
  procedure Create (File  : in out File_Type;
                    Title : in String := "Select output file") is
    D : Save_Dialog_Type := Save_Dialog (Title,True);
  begin
    if Execute(D) then
      Ada.Text_io.Put_Line("Creating " & Get_Name(D));
      Ada.Text_IO.Create (File, Name => Get_Name(D));
    else
      raise Input_Cancelled;
    end if;
  end Create;

  ----------------------------------------------------------------------------
  --
  --  Append: use a dialog box to get the name of the file to open and
  --          open it for appending.
  --
  procedure Append (File  : in out File_Type;
                    Title : in String := "Select output file") is
    D : Save_Dialog_Type := Save_Dialog (Title, False);
  begin
    if Execute(D) then
      Ada.Text_io.Put_Line("Appending to " & Get_Name(D));
      begin
        Ada.Text_IO.Open (File, Mode => Ada.Text_IO.Append_File,
                                Name => Get_Name(D));
      exception
        when Ada.Text_IO.Name_Error =>
          Ada.Text_IO.Create (File, Name => Get_Name(D));
      end;
    else
      raise Input_Cancelled;
    end if;
  end Append;

end GtkJewl.IO;
