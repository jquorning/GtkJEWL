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

with Ada.Text_Io;

package GtkJewl.IO is

  ----------------------------------------------------------------------------
  --
  -- Some local names for types from Ada.Text_IO
  --
  subtype Positive_Count is Ada.Text_IO.Positive_Count;
  subtype File_Type      is Ada.Text_IO.File_Type;

  ----------------------------------------------------------------------------
  --
  -- Exceptions that this package might raise
  --
  Input_Cancelled : exception;      -- the user cancelled an input dialog

  ----------------------------------------------------------------------------
  --
  -- Routines to display message boxes
  --
  procedure Error   (Text : in String);                 -- an error message
  function  Query   (Text : in String) return Boolean;  -- a yes/no query
  procedure Message (Text : in String);                 -- an information message
  procedure Warning (Text : in String);                 -- an information message

  ----------------------------------------------------------------------------
  --
  -- Output a newline, as in Ada.Text_IO
  --
  procedure New_Line (Spacing : in Ada.Text_IO.Positive_Count := 1)
            renames Ada.Text_IO.New_Line;

  ----------------------------------------------------------------------------
  --
  -- Opening and closing files, using standard file dialogs to get filenames
  --
  procedure Open   (File  : in out File_Type;   -- open an existing file
                    Title : in String := "Select input file");
  procedure Create (File  : in out File_Type;   -- create a new file
                    Title : in String := "Select output file");
  procedure Append (File  : in out File_Type;   -- append to existing/new file
                    Title : in String := "Select output file");
  procedure Close  (File   : in out Ada.Text_IO.File_Type)
                                renames Ada.Text_IO.Close;
                                                -- close an open file

  ----------------------------------------------------------------------------
  --
  --  Standard file positioning operations and queries, as in Ada.Text_IO
  --
  procedure New_Line    (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.New_Line;
  procedure Skip_Line   (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.Skip_Line;
  function  End_Of_Line (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_Line;
  function  End_Of_File (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_File;

  ----------------------------------------------------------------------------
  --
  --      P R I M A R Y   I N P U T / O U T P U T   R O U T I N E S
  --
  --                         ---- Input ----
  --
  --  For each scalar type (with generic packages for enumerations, integral
  --  types and floating point types, and support provided as standard for
  --  String, Character, Integer, Float, and Boolean):
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Default => Y,       -- with an initial default value of Y (optional)
  --       Prompt  => Z);      -- displaying Z as a prompt (optional),
  --
  --  Get (File    => F,
  --       Item    => X);      -- get a value into X from file F
  --
  --  The prompt is always a string; the default value depends on the type of
  --  data involved. In the case of String there is a potential ambiguity as
  --  the prompt and default values are both strings. It is recommended that
  --  the Prompt and Default parameters are always specified as "Prompt=>X"
  --  and "Default=>X" to avoid confusion.
  --
  --  Strings are a bit different: there is a function which gets a String
  --  (unconstrained) and a procedure which gets a value into a String but
  --  which also returns the length of the string in Length:
  --
  --  S := Get (Prompt  => Y,  -- optional
  --            Default => Z); -- optional
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Length  => L,       -- whose actual length is L,
  --       Prompt  => Y,       -- displaying Y as a prompt (optional),
  --       Default => Z);      -- with an initial default value of Z (optional)
  --
  --  Get (File    => F,
  --       Item    => X,       -- get a value into X from file F,
  --       Length  => L);      -- whose actual length is L
  --
  --                        ---- Output ----
  --
  --  Values can be output to the standard output or a file, with or without
  --  a following newline:
  --
  --  Put (Item => X);         -- write the value of X on the standard output
  --  Put (File => F,
  --       Item => X);         -- write the value of X on the file F
  --
  --  Put_Line (Item => X);    -- write X and a newline on the standard output
  --  Put_Line (File => F,
  --            Item => X);    -- write X and a newline on the file F
  --
  --                    ---- Type conversion ----
  --
  --  S := To_String(X)        -- convert a (non-string) type to a String
  --  S := S & X;              -- concatenate String and X
  --  S := X & S;              -- concatenate X and String
  --
  ----------------------------------------------------------------------------
  --
  --  String and Character input
  --
  function  Get (Prompt  : in  String := "Enter your text:";
                 Default : in  String := "")
                 return String;     -- display a dialog with a label (Prompt)
                                    -- and an editbox (initial value Default)
                                    -- and return the contents of the editbox
                                    -- as a String (unconstrained)

  procedure Get (Item    : out String;
                 Length  : out Natural;
                 Prompt  : in  String := "Enter your text:";
                 Default : in  String := "");
                                    -- use the same edit dialog to get a string
                                    -- into a variable, the maximum length being
                                    -- limited by the size of Item and the actual
                                    -- length being stored in Length, with an
                                    -- initial default value
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out Character)
                                renames Ada.Text_IO.Get;
                                    -- get a character from a file, from Text_IO
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out String;
                 Length  : out Natural)
                                renames Ada.Text_IO.Get_Line;
                                    -- get a string and its length from a file,
                                    -- from Text_IO

  ----------------------------------------------------------------------------
  --  Renamings for consistency with Ada.Text_IO
  ----------------------------------------------------------------------------

  procedure Get_Line (Item    : out String;
                      Length  : out Natural     ;
                      Prompt  : in  String := "Enter your text:";
                      Default : in  String := "")  renames Get;

  procedure Get_Line (File    : in Ada.Text_IO.File_Type;
                      Item    : out String;
                      Length  : out Natural)       renames Get;

  ----------------------------------------------------------------------------
  --
  --  String and Character output
  --
  procedure Put (Item    : in String)
                                renames Ada.Text_io.Put;
                                    -- output a string
  procedure Put (Item    : in Character)
                                renames Ada.Text_io.Put;
                                    -- output a character
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in Character)
                                renames Ada.Text_io.Put;
                                    -- output a character to a file
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in String)
                                renames Ada.Text_io.Put;
                                    -- output a string to a file

  ----------------------------------------------------------------------------
  --
  --  String and Character output, with newlines
  --
  procedure Put_Line (Item : in String)
                                renames Ada.Text_io.Put_Line;
                                    -- output a string and newline
  procedure Put_Line (Item : in Character);
                                    -- output a character and newline
  procedure Put_Line (File : in Ada.Text_IO.File_Type;
                      Item : in String)
                                renames Ada.Text_io.Put_Line;
                                    -- output a string and newline to a file
  procedure Put_Line (File : in File_Type;
                      Item : in Character);
                                    -- output a character and newline to a file

  ----------------------------------------------------------------------------
  --
  --               INTEGER INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Integer input
  --
  procedure Get (Item    : out Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Integer;
                 Default : in  Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Integer);
                                    -- read an Integer from a file

  ----------------------------------------------------------------------------
  --
  --  Integer output
  --
  procedure Put (Item    : in Integer);
                                    -- output an Integer
  procedure Put (File    : in File_Type;
                 Item    : in Integer);
                                    -- output an Integer to a file

  ----------------------------------------------------------------------------
  --
  --  Integer output, with newlines
  --
  procedure Put_Line (Item : in Integer);
                                    -- output an Integer and a newline
  procedure Put_Line (File : in File_Type;
                      Item : in Integer);
                                    -- output an Integer and a newline to a file

  ----------------------------------------------------------------------------
  --
  --  Integer conversion routines
  --
  function  To_String (Item : Integer) return String;
                                    -- convert an integer to a string

  function  "&" (Left    : String;
                 Right   : Integer)    return String;
                                    -- concatenate String & Integer
  function  "&" (Left    : Integer;
                 Right   : String)     return String;
                                    -- concatenate Integer & String

  ----------------------------------------------------------------------------
  --
  --                FLOAT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Float input
  --
  procedure Get (Item    : out Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Float;
                 Default : in  Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Float);
                                    -- read a Float from a file

  ----------------------------------------------------------------------------
  --
  --  Float output
  --
  procedure Put (Item    : in  Float);
  procedure Put (File    : in  File_Type;
                 Item    : in  Float);

  ----------------------------------------------------------------------------
  --
  --  Float output, with newlines
  --
  procedure Put_Line (Item : in Float);
  procedure Put_Line (File : in File_Type;
                      Item : in Float);

  ----------------------------------------------------------------------------
  --
  --   Float conversion routines
  --
  function  To_String (Item : Float) return String;

  function  "&" (Left    : String;
                 Right   : Float)    return String;
  function  "&" (Left    : Float;
                 Right   : String)   return String;

  ----------------------------------------------------------------------------
  --
  --               BOOLEAN INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Boolean input
  --
  procedure Get (Item    : out Boolean;
                 Prompt  : in  String := "Yes or no?");
                                    -- display a dialog with Yes/No/Cancel
                                    -- buttons
  procedure Get (Item    : out Boolean;
                 Default : in  Boolean;
                 Prompt  : in  String := "Yes");
                                    -- display a dialog with a checkbox
                                    -- initialised as specified by Default
  procedure Get (File    : in  File_Type;
                 Item    : out Boolean);
                                    -- read a Boolean from a file

  ----------------------------------------------------------------------------
  --
  --  Boolean output
  --
  procedure Put (Item    : in Boolean);
  procedure Put (File    : in File_Type;
                 Item    : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean output, with newlines
  --
  procedure Put_Line (Item : in Boolean);
  procedure Put_Line (File : in File_Type;
                      Item : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean conversion routines
  --
  function  To_String (Item : Boolean) return String;

  function  "&" (Left    : String;
                 Right   : Boolean)    return String;
  function  "&" (Left    : Boolean;
                 Right   : String)     return String;

  ----------------------------------------------------------------------------
  --
  --             ENUMERATION INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is (<>);
  package Enumeration_IO is

    --------------------------------------------------------------------------
    --
    --  Enumeration input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a combobox listing all values of
                                    -- type Item_Type
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display the same dialog with the combobox
                                    -- initialised to Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Enumeration output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Enumeration_IO;

  ----------------------------------------------------------------------------
  --
  --         GENERIC INTEGRAL INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is range <>;
  package Integer_IO is

    --------------------------------------------------------------------------
    --
    --  Generic integral input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- integral value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic integral output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic integral output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    -- Generic integral conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Integer_IO;

  ----------------------------------------------------------------------------
  --
  --       GENERIC FLOATING-POINT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is digits <>;
  package Float_IO is

    --------------------------------------------------------------------------
    --
    --  Generic floating-point input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- floating-point value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Float_IO;

end GtkJewl.IO;
