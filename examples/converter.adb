------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Simple_Windows application: a temperature converter.       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: converter.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: converter.adb $
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- * Calculator, Converter and Minipad amended to use GtkJewl.Simple_Windows
-- * Show_Image and Pong added
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with GtkJewl.Simple_Windows;
use  GtkJewl.Simple_Windows;
procedure Converter is
  My_Frame : Frame_Type := Frame (200, 150, "Converter", 'Q');
  My_Menu  : Menu_Type  := Menu (My_Frame, "&Direction");
  C_To_F   : Menuitem_Type := Menuitem (My_Menu, "&C to F", 'C');
  F_To_C   : Menuitem_Type := Menuitem (My_Menu, "&F to C", 'F');
  Value    : Editbox_Type :=
             Editbox (My_Frame, (10,10), 110, 20, "0");
  Do_It    : Button_Type :=
             Button (My_Frame, (40,40), 80, 25, "Convert", 'X');
  Result   : Label_Type :=
             Label (My_Frame, (10,75), 140, 20, "", Centre);
  To_F : Boolean := True;
  I, J : Integer;
begin
  loop
    case Next_Command is
      when 'Q' =>
        Close (My_Frame);
        exit;
      when 'C' =>
        To_F := True;
      when 'F' =>
        To_F := False;
      when 'X' =>
        I := Integer'Value(Get_Text(Value));
        if To_F then
          J := I * 9 / 5 + 32;
          Set_Text (Result, Integer'Image(I) & "C =" &
                            Integer'Image(J) & "F");
        else
          J := (I - 32) * 5 / 9;
          Set_Text (Result, Integer'Image(I) & "F =" &
                            Integer'Image(J) & "C");
        end if;
      when others =>
        null;
    end case;
  end loop;
end Converter;
