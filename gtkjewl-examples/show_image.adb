------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Simple_Windows application: a simple bitmap viewer.         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: show_image.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: show_image.adb $
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
------------------------------------------------------------------------------

with GtkJewl.Simple_Windows;
use  GtkJewl.Simple_Windows;
procedure Show_Image is
  F : Frame_Type := Frame(300,300,"Test",'X');
      M : Menu_Type := Menu (F, "&File");
          O : Menuitem_Type := Menuitem (M, "&Open", 'O');
          X : Menuitem_Type := Menuitem (M, "E&xit", 'X');
      C : Canvas_Type := Canvas (F, (0,0), 0, 0, 'R');

  Open : Open_Dialog_Type := Open_Dialog ("Select bitmap");
  Img  : Image_Type;   -- this is a bitmap
  From : Point_Type;
  To   : Point_Type;
begin
  Add_Filter (Open, "Bitmap files (*.bmp)", "*.bmp");
  Add_Filter (Open, "All files (*.*)", "*.*");
  Save (C);
  loop
    case Next_Command is
      when 'O' =>
        if Execute (Open) then
          Img := Image (Get_Name(Open));
          if Valid(Img) then
            Set_Size (F, Width  => Width(Img)  + Frame_Width,
                         Height => Height(Img) + Frame_Height + Menu_Height);
            Show_Message ("Bitmap:" & Integer'Image(Width(Img)) &
                          " x" & Integer'Image(Height(Img)) &
                          " -- drag to resize");
            Restore(C);
            Draw_Image(C, (0,0), Img);
          else
            Play_Sound ("error.wav");
            Show_Message (Get_Name(Open) & " is not a valid bitmap file");
          end if;
        end if;
      when 'X' =>
        Close(F);
        exit;
      when 'R' =>
        while Mouse_Down(C) loop
          From := Start_Point(C);
          if Mouse_Moved(C) then
            To := End_Point(C);
            Restore(C);
            Draw_Image (C, From, To, Img);
          end if;
        end loop;
      when others =>
        null;
    end case;
  end loop;
end Show_Image;
