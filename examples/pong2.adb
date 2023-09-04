------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Simple_Windows application: a simple Pong game.            --
--                                                                          --
--   Copyright (C) John English 2001. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: pong.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: pong.adb $
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

procedure Pong is

  F : Frame_Type  := Frame (300, 250, "Pong!", 'Q');
  C : Canvas_Type := Canvas (F, (0,0), 0, 0, 'X', 'X');
  
  P : Integer := Get_Width(C)/2 - 40;
  W : Integer;
  H : Integer;
  B : Point_Type := (P + 40, Get_Height(C) - 25);
  X : Integer := 5;
  Y : Integer := -10;
  
begin
  Show_Message ("Use Z key to move paddle left, X to move right.");
  Set_Fill(C,Black);
  Save(C);
  loop
    W := Get_Width(C);
    H := Get_Height(C);
    delay 0.05;
    case Key_Code(C) is
      when 'Z' | 'z' =>
        if P > 10 then
          P := P - 15;
        end if;
      when 'X' | 'x' =>
        if P < W - 90 then
          P := P + 15;
        end if;
      when others =>
        null;
    end case;
    Restore(C, Paint=>False);
    Draw_Rectangle(C,(P,H-20),(P+80,H-15), Paint=>False);
    Draw_Circle(C,B,5, Paint=>False);
    Force_Draw(C);  
    if B.Y < 5 or (B.Y > H-25 and B.X >= P and B.X < P+80) then
      Y := -Y;
    elsif B.Y > H then
      Show_Message("Game over!");
      exit;
    end if;
    if B.X < 5 or B.X >= W-5 then
      X := -X;
    end if;
    B := B + (X,Y);
  end loop;
  Close(F);
exception
  when others =>
    Close(F);
end Pong;
