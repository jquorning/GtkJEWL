------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Windows application: a simple sketchpad application.       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: sketch.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: sketch.adb $
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
procedure Sketch is
  type Cmd is (Quit, Draw, Undo, Pen, Fill, No_Fill, Background,
               Line, Rectangle, Ellipse, OK, Cancel, Set_Pen_Colour);
  package Sketch_Windows is new GtkJewl.Windows(Cmd); use Sketch_Windows;
  
  Sketch_Frame : Frame_Type := Frame (300,400, "Sketchpad", Quit);
    Shape_Menu : Menu_Type  := Menu (Sketch_Frame, "&Shape");
      Menu_L : Menuitem_Type := Menuitem (Shape_Menu, "&Line", Line);
      Menu_R : Menuitem_Type := Menuitem (Shape_Menu, "&Rectangle", Rectangle);
      Menu_E : Menuitem_Type := Menuitem (Shape_Menu, "&Ellipse", Ellipse);
      Sep    : Menuitem_Type := Separator(Shape_Menu);
      Menu_U : Menuitem_Type := Menuitem (Shape_Menu, "&Undo draw", Undo);
    Tools_Menu : Menu_Type  := Menu (Sketch_Frame, "&Tools");
      Menu_P : Menuitem_Type := Menuitem (Tools_Menu, "Select &pen", Pen); 
      Menu_F : Menuitem_Type := Menuitem (Tools_Menu, "Set &fill colour", Fill);
      Menu_N : Menuitem_Type := Menuitem (Tools_Menu, "Fill shapes", No_Fill);
      Menu_B : Menuitem_Type := Menuitem (Tools_Menu, "Set &background colour",
                                                      Background);
    Surface : Canvas_Type := Canvas (Sketch_Frame, (0,0), 0, 0, Draw);

  Pen_Dialog : Dialog_Type := Dialog (365, 125, "Pen properties", Cancel);
    Pen_Label_1   : Label_Type := Label (Pen_Dialog, (20,20), 100, 20,
                                                     "Pen width:", Right);
    Pen_Edit      : Editbox_Type := Editbox (Pen_Dialog, (125,20), 50, 20);
    Pen_Label_2   : Label_Type := Label (Pen_Dialog, (190,20), 100, 20,
                                                     "Pen colour:", Right);
    Pen_Canvas    : Canvas_Type := Canvas (Pen_Dialog, (295,20), 50, 20);
    OK_Button     : Button_Type := Button (Pen_Dialog, (10,60), 100, 25,
                                                       "OK", OK, True);
    Cancel_Button : Button_Type := Button (Pen_Dialog, (130,60), 100, 25,
                                                       "Cancel", Cancel);
    Colour_Button : Button_Type := Button (Pen_Dialog, (250,60), 100, 25,
                                                       "Set colour...",
                                                       Set_Pen_Colour);

  Fill_Colour : Colour_Dialog_Type := Colour_Dialog;
  Pen_Colour  : Colour_Dialog_Type := Colour_Dialog;
  BG_Colour   : Colour_Dialog_Type := Colour_Dialog;

  Current_Tool : Cmd := Line;
  Pen_Width    : Natural := 1;

  procedure Draw_Shape is
  begin
    case Current_Tool is
      when Line =>
        Draw_Line (Surface, Start_Point(Surface),
                            End_Point(Surface),Paint=>False);
      when Rectangle =>
        Draw_Rectangle (Surface, Start_Point(Surface),
                                 End_Point(Surface),Paint=>False);
      when Ellipse =>
        Draw_Ellipse (Surface, Start_Point(Surface),
                               End_Point(Surface),Paint=>False);
      when others =>
        Show_Error ("Invalid tool (" & Cmd'Image(Current_Tool) & ")");
    end case;
  end Draw_Shape;

begin
  Set_Colour (Fill_Colour, White);
  Set_Colour (Pen_Colour,  Black);
  Set_Colour (BG_Colour,   White);
  Set_State  (Menu_L, True);
  
  loop
    Enable (Menu_F, Get_State(Menu_N));
    case Next_Command is
      when Quit =>
        exit;
        
      when Draw =>
        Save (Surface);
        while Mouse_Down(Surface) loop
          if Mouse_Moved(Surface) then
            Restore (Surface,Paint=>False);
            Draw_Shape;
            Force_Draw(Surface);
          end if;
        end loop;

      when Undo =>
        Restore (Surface);

      when Pen =>
        loop
          Set_Colour (Pen_Canvas, Get_Colour(Pen_Colour));
          Set_Text (Pen_Edit, Integer'Image(Pen_Width));
          case Execute(Pen_Dialog) is
            when OK =>
              begin
                Pen_Width := Natural'Value(Get_Text(Pen_Edit));
                Set_Pen (Surface, Get_Colour(Pen_Colour), Pen_Width);
                exit;
              exception
                when Constraint_Error =>
                  Show_Error ("Pen width must be a positive number!");
              end;
            when Cancel =>
              exit;
            when Set_Pen_Colour =>
              if Execute(Pen_Colour) then
                null;
              end if;
            when others =>
              null;
          end case;
        end loop;

      when Fill =>
        if Execute(Fill_Colour) then
          Set_Fill (Surface, Get_Colour(Fill_Colour));
        end if;

      when No_Fill =>
        Set_State (Menu_N, not Get_State(Menu_N));
        if Get_State(Menu_N) then
          Set_Fill (Surface, Get_Colour(Fill_Colour));
        else
          Set_Fill (Surface);
        end if;

      when Background =>
        if Execute(BG_Colour) then
          Set_Colour (Surface, Get_Colour(BG_Colour));
        end if;

      when Line =>
        Current_Tool := Line;
        Set_State (Menu_L, True);
        Set_State (Menu_R, False);
        Set_State (Menu_E, False);

      when Rectangle =>
        Current_Tool := Rectangle;
        Set_State (Menu_L, False);
        Set_State (Menu_R, True);
        Set_State (Menu_E, False);

      when Ellipse =>
        Current_Tool := Ellipse;
        Set_State (Menu_L, False);
        Set_State (Menu_R, False);
        Set_State (Menu_E, True);

      when OK | Cancel | Set_Pen_Colour =>
        null;

    end case;
  end loop;
end Sketch;
