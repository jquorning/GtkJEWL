------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Windows application: a font tester.                        --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: font_test.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: font_test.adb $
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

with GtkJewl.Windows, GtkJewl.IO;
use GtkJewl.IO;
procedure Font_Test is
  type F_Command is (Quit, Set, Pick);
  package F_IO is new GtkJewl.Windows(F_Command); use F_IO;
  F1 : Frame_Type := Frame(500,300,"Font test",Quit);
  L1 : Label_Type := Label(F1,(0,10),0,45,"Font Sample",Centre);
  L2 : Label_Type := Label(F1,(10,60),100,25,"Font name:",Right);
  E2 : Editbox_Type := Editbox (F1, (120,60), 100, 25);
  L3 : Label_Type := Label(F1,(10,90),100,25,"Font size:",Right);
  E3 : Editbox_Type := Editbox (F1, (120,90), 100, 25);
  C1 : Checkbox_Type := Checkbox (F1, (10,120), 200, 25, "Bold");
  C2 : Checkbox_Type := Checkbox (F1, (10,150), 200, 25, "Italic");
  B1 : Button_Type := Button (F1, (10,210), 100, 25, "Set", Set);
  B2 : Button_Type := Button (F1, (150,210), 100, 25, "Pick", Pick);
  D : Font_Dialog_Type := Font_Dialog;
begin
  loop
    declare
      F : constant Font_Type := Get_Font(L1);
    begin
      Set_Text (E2, F.Name);
      Set_Text (E3, Integer'Image(F.Size));
      Set_State(C1, F.Bold);
      Set_State(C2, F.Italic);
    end;
    case Next_Command is
      when Quit => exit;
      when Set =>  Set_Font (L1, Font(Get_Text(E2), Integer'Value(Get_Text(E3)),
                                      Get_State(C1), Get_State(C2)));
      when Pick =>
        Set_Font (D, Get_Font(L1));
        if Execute(D) then
          Set_Font(L1, Get_Font(D));
        end if;
    end case;
  end loop;
end Font_Test;
