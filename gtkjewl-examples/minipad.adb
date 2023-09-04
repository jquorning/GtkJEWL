------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Simple_Windows application: a simple text editor.          --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: minipad.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: minipad.adb $
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

with GtkJewl.Simple_Windows, Ada.Text_IO;
use  GtkJewl.Simple_Windows, Ada.Text_IO;
procedure Minipad is

  Main : Frame_Type := Frame (400, 400, "Minipad", 'Q');
  Text : Memo_Type  := Memo (Main, (0,0), 0, 0, Font("Courier New",9));
  File : Menu_Type  := Menu (Main, "&File");
         New_Menu   : Menuitem_Type := Menuitem (File, "&New",   'N');
         Open_Menu  : Menuitem_Type := Menuitem (File, "&Open",  'O');
         Save_Menu  : Menuitem_Type := Menuitem (File, "&Save",  'S');
         Sep_1      : Menuitem_Type := Separator (File);
         Exit_Menu  : Menuitem_Type := Menuitem (File, "E&xit",  'Q');
  Edit : Menu_Type  := Menu (Main, "&Edit");
         Undo_Menu  : Menuitem_Type := Menuitem (Edit, "&Undo",  'Z');
         Sep_2      : Menuitem_Type := Separator (Edit);
         Cut_Menu   : Menuitem_Type := Menuitem (Edit, "Cu&t",   'X');
         Copy_Menu  : Menuitem_Type := Menuitem (Edit, "&Copy",  'C');
         Paste_Menu : Menuitem_Type := Menuitem (Edit, "&Paste", 'V');
         Sep_3      : Menuitem_Type := Separator (Edit);
         Font_Menu  : Menuitem_Type := Menuitem (Edit, "Set &Font...", 'F');

  FD : Font_Dialog_Type := Font_Dialog;
  OD : Open_Dialog_Type := Open_Dialog ("Open file");
  SD : Save_Dialog_Type := Save_Dialog ("Save as...");
  
  procedure Open_File is
    S : String (1..1000);
    L : Natural;
    F : Ada.Text_IO.File_Type;
    B : Boolean;
  begin
    if Execute(OD) then
      Ada.Text_IO.Open (F, Mode => Ada.Text_IO.In_File,
                           Name => Get_Name(OD));
      Delete_All (Text);
      while not Ada.Text_IO.End_Of_File(F) loop
        Ada.Text_IO.Get_Line (F, S, L);
        Append_Line (Text, S(1..L));
      end loop;
      Ada.Text_IO.Close (F);
      B := Modified (Text);
      Select_Line (Text, 1);
      Show_Selection (Text);
    end if;
  end Open_File;

  procedure Save_File is
    F : Ada.Text_IO.File_Type;
  begin
    Set_Name (SD, Get_Name(OD));
    if Execute(SD) then
      Ada.Text_IO.Create (F, Name => Get_Name(SD));
      for I in 1 .. Get_Count(Text) loop
        Ada.Text_IO.Put_Line (F, Get_Text(Text,I));
      end loop;
      Ada.Text_IO.Close (F);
    end if;
  end Save_File;

begin
  loop
    case Next_Command is
      when 'N' =>
        if Modified(Text) then
          if Show_Query ("Text has been modified -- save file?") then
            Save_File;
          end if;
        end if;
        Delete_All (Text);
      when 'O' =>
        if Modified(Text) then
          if Show_Query ("Text has been modified -- save file?") then
            Save_File;
          end if;
        end if;
        Open_File;    
      when 'S' =>
        if Modified(Text) or else
           Show_Query("Text not modified -- save anyway?")
        then
          Save_File;
        end if;
      when 'Z' =>
        Undo_Change (Text);
      when 'X' =>
        Cut_Selection (Text);
      when 'C' =>
        Copy_Selection (Text);
      when 'V' =>
        Paste_Selection (Text);
      when 'F' =>
        Set_Font (FD, Get_Font(Text));
        if Execute(FD) then
          Set_Font (Text, Get_Font(FD));
        end if;
      when 'Q' =>
        if Modified(Text) then
          if Show_Query ("Text has been modified -- save file?") then
            Save_File;
          end if;
        end if;
        exit;
      when others =>
        null;
    end case;
  end loop;
end Minipad;
