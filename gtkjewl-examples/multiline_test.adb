------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Windows application: a tester for multiline controls.      --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: multiline_test.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: multiline_test.adb $
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

with GtkJewl.Windows;
procedure Multiline_Test is
  type M_Command is (Quit, Pick, Set, Append, Insert, Delete, Erase, Show,
                     List, Combo, Memo, Cut, Copy, Paste, Undo, Scroll);
  package M_IO is new GtkJewl.Windows (M_Command); use M_IO;
  type T is access Multiline_Type'Class;
  F1 : Frame_Type := Frame (600,400,"Multiline Test", Quit);
    M1 : Menu_Type := Menu (F1, "Commands");
      C1 : Menuitem_Type := Menuitem (M1, "Select Line", Pick);
      C2 : Menuitem_Type := Menuitem (M1, "Set Text", Set);
      C3 : Menuitem_Type := Menuitem (M1, "Append Line", Append);
      C4 : Menuitem_Type := Menuitem (M1, "Insert Line", Insert);
      C5 : Menuitem_Type := Menuitem (M1, "Delete Line", Delete);
      C6 : Menuitem_Type := Menuitem (M1, "Delete All", Erase);
    M2 : Menu_Type := Menu (F1, "Memo");
      C7  : Menuitem_Type := Menuitem (M2, "Cut", Cut);
      C8  : Menuitem_Type := Menuitem (M2, "Copy", Copy);
      C9  : Menuitem_Type := Menuitem (M2, "Paste", Paste);
      C10 : Menuitem_Type := Menuitem (M2, "Undo", Undo);
      C11 : Menuitem_Type := Menuitem (M2, "Show cursor", Scroll);
    L1 : Label_Type := Label (F1, (20,20), 200, 20, "Line number:", Right);
    E1 : Editbox_Type := Editbox (F1, (225,20), 300, 20);
    L2 : Label_Type := Label (F1, (20,50), 200, 20, "Text of line:", Right);
    E2 : Editbox_Type := Editbox (F1, (225,50), 300, 20);
    X1 : T;
    B1 : Button_Type := Button (F1, (20,310), 100, 25, "Show", Show);
    L3 : Label_Type := Label (F1, (20,20), 100, 20, "");
    L4 : Label_Type := Label (F1, (20,42), 100, 20, "");
    L5 : Label_Type := Label (F1, (20,64), 100, 20, "");
    L6 : Label_Type := Label (F1, (20,86), 100, 20, "");
  D1 : Dialog_Type := Dialog (290, 70, "Select multiline type", Quit);
    BN1 : Button_Type := Button (D1, (10,10), 80, 25, "Listbox", List);
    BN2 : Button_Type := Button (D1, (100,10), 80, 25, "Combobox", Combo);
    BN3 : Button_Type := Button (D1, (190,10), 80, 25, "Memo", Memo);
begin
  case Execute(D1) is
    when List =>
      X1 := new Listbox_Type'(Listbox (F1, (20,110), 500, 180));
      Disable(C7);
      Disable(C8);
      Disable(C9);
      Disable(C10);
      Disable(C11);
    when Combo =>
      X1 := new Combobox_Type'(Combobox (F1, (20,110), 500));
      Disable(C7);
      Disable(C8);
      Disable(C9);
      Disable(C10);
      Disable(C11);
    when Memo =>
      X1 := new Memo_Type'(Memo (F1, (20,110), 500, 180));
    when others =>
      return;
  end case;
  while Valid(F1) loop
    Set_Text (E1, Integer'Image(Get_Line(X1.all)));
    Set_Text (E2, Get_Text(X1.all,Get_Line(X1.all)));
    Set_Text (L3, "Lines:" & Integer'Image(Get_Count(X1.all)));
    if X1.all in Memo_Type'Class then
      Set_Text (L4, "Column: " & Integer'Image(Get_Column(Memo_Type'Class(X1.all))));
      Set_Text (L5, "Modified: " & Boolean'Image(Modified(Memo_Type'Class(X1.all))));
      Set_Text (L6, "Length: " & Integer'Image(Get_Length(Memo_Type'Class(X1.all))));
    end if;
    case Next_Command is
      when Quit =>
        exit;
      when Pick =>
        Select_Line(X1.all,Integer'Value(Get_Text(E1)));
      when Set =>
        Set_Text(X1.all,Get_Text(E2),Integer'Value(Get_Text(E1)));
      when Append =>
        Append_Line(X1.all,Get_Text(E2));
      when Insert =>
        Insert_Line(X1.all,Get_Text(E2),Integer'Value(Get_Text(E1)));
      when Delete =>
        Delete_Line(X1.all,Integer'Value(Get_Text(E1)));
      when Erase =>
        Delete_All(X1.all);
      when Cut =>
        Cut_Selection(Memo_Type'Class(X1.all));
      when Copy =>
        Copy_Selection(Memo_Type'Class(X1.all));
      when Paste =>
        Paste_Selection(Memo_Type'Class(X1.all));
      when Undo =>
        Undo_Change(Memo_Type'Class(X1.all));
      when Scroll =>
        Show_Selection(Memo_Type'Class(X1.all));
      when others =>
        null;
    end case;
  end loop;
end Multiline_Test;
