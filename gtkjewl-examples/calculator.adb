------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.Simple_Windows application: a simple calculator.           --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: calculator.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: calculator.adb $
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
procedure Calculator is
  F : Frame_Type := Frame (150, 200, "Calculator", 'Q',
                           Font("Sans",7,Bold=>True));
  EB : Editbox_Type := Editbox(F, (10,10), 110, 20, "0");
  B0 : Button_Type := Button(F, ( 40,130), 20, 20, "0", '0');
  B1 : Button_Type := Button(F, ( 10,100), 20, 20, "1", '1');
  B2 : Button_Type := Button(F, ( 40,100), 20, 20, "2", '2');
  B3 : Button_Type := Button(F, ( 70,100), 20, 20, "3", '3');
  B4 : Button_Type := Button(F, ( 10, 70), 20, 20, "4", '4');
  B5 : Button_Type := Button(F, ( 40, 70), 20, 20, "5", '5');
  B6 : Button_Type := Button(F, ( 70, 70), 20, 20, "6", '6');
  B7 : Button_Type := Button(F, ( 10, 40), 20, 20, "7", '7');
  B8 : Button_Type := Button(F, ( 40, 40), 20, 20, "8", '8');
  B9 : Button_Type := Button(F, ( 70, 40), 20, 20, "9", '9');
  BC : Button_Type := Button(F, ( 10,130), 20, 20, "C", 'C');
  BA : Button_Type := Button(F, (100, 40), 20, 20, "+", '+');
  BS : Button_Type := Button(F, (100, 70), 20, 20, "-", '-', False, Font("Arial",15));
  BM : Button_Type := Button(F, (100,100), 20, 20, "x", '*');
  BD : Button_Type := Button(F, (100,130), 20, 20, "/", '/');
  BQ : Button_Type := Button(F, ( 70,130), 20, 20, "=", '=');

  Operator : Character := '=';
  Left     : Integer   := 0;
  Right    : Integer   := 0;
  Command  : Character;

begin
  loop
    begin
      Command := Next_Command;
      case Command is
        when '0' .. '9' =>
          Right := Right * 10 +
                   (Character'Pos(Command)-Character'Pos('0'));
          Set_Text (EB, Integer'Image(Right));
        when '+' | '-' | '*' | '/' | '=' =>
          Right := Integer'Value(Get_Text(EB));
          case Operator is
            when '+'    => Left := Left + Right;
            when '-'    => Left := Left - Right;
            when '*'    => Left := Left * Right;
            when '/'    => Left := Left / Right;
            when '='    => Left := Right;
            when others => null;
          end case;
          Operator := Command;
          Right := 0;
          Set_Text (EB, Integer'Image(Left));
        when 'C' =>
          Operator := '=';
          Left  := 0;
          Right := 0;
          Set_Text (EB, "0");
        when 'Q' =>
          exit;
        when others =>
          null;
      end case;
    exception
      when Constraint_Error =>
        Operator := '=';
        Left  := 0;
        Right := 0;
        Set_Text (EB, "*** ERROR ***");
    end;
  end loop;
end Calculator;
