------------------------------------------------------------------------------
--                                                                          --
--   Sample GtkJewl.IO application: a simple guessing game.                    --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: guess_me.adb 1.6 2001/11/02 16:00:00 JE Exp JE $
------------------------------------------------------------------------------
--
-- $Log: guess_me.adb $
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

with GtkJewl.IO; use GtkJewl.IO;
procedure Guess_Me is
  Secret   : Integer := 42;
  Guess    : Integer;
  Attempts : Integer := 0;
begin
  Get (Secret, "Enter a secret number between 1 and 1000:");
  if Secret not in 1..1000 then
    if not Query ("Number out of range: shall I let the sucker carry on?") then
      return;
    end if;
  end if;
  Message ("You have 10 attempts to find the secret number between 1 and 1000!");
  for I in 1..10 loop
    Get (Guess, "Attempt " & I & ": Enter a number between 1 and 1000:");
    if Guess in 1..1000 then
      if Guess = Secret then
        exit;
      elsif Guess < Secret then
        Message ("Too low! Try again!");
      else
        Message ("Too high! Try again!");
      end if;
    else
      Error ("Your guess must be between 1 and 1000!");
    end if;
  end loop;
  if Guess = Secret then
    Message ("Congratulations! The secret number was " & Secret & "!");
  else
    Message ("Bad luck! The secret number was " & Secret & "!");
  end if;
exception
  when Input_Cancelled =>
    Message ("You gave up! The secret number was " & Secret & "!");
end Guess_Me;
