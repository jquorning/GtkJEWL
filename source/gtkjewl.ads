------------------------------------------------------------------------------
--
--         	                  G T K J E W L
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

with Ada.Finalization;

package GtkJewl is

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  These types are used elsewhere throughout this package library:
  --
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and style
  --                    options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Angle_Type     is mod 360;
  subtype Colour_Range   is Integer range 0..255;
  type    Colour_Type    is record
                              Red    : Colour_Range;
                              Green  : Colour_Range;
                              Blue   : Colour_Range;
  end record;
  type    Font_Type_Record (Length : Natural)
                            is tagged record
                                Name   : String (1..Length);
                                Size   : Positive;
                                Bold   : Boolean := False;
                                Italic : Boolean := False;
                            end record;
    type Font_Type is access Font_Type_Record'Class;
  
  type    Point_Type    is record
                              X,Y : Integer;
                            end record;
  type    Point_List     is array (Positive range <>) of Point_Type;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  Colour operations:
  --    Light    : Generate a lightened version of a colour, e.g. Light(Red).
  --    Dark     : Generate a darkened version of a colour, e.g. Dark(Green).
  --
  --  Font operations:
  --    Font     : Generate a font with the specified properties.
  --    Name     : Get the name of the font typeface.
  --    Size     : Get the font size in points.
  --    Bold     : Test if the font is bold.
  --    Italic   : Test if the font is italic.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle.
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : Colour_Type) return Colour_Type;
  function Dark     (Colour : Colour_Type) return Colour_Type;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean  := False;
                     Italic : Boolean  := False)
                                           return Font_Type;
  function Font2String (Font: Font_Type) return String;
  function String2Font (Fstr: String) return Font_Type;

  function Name     (Font   : Font_Type)   return String;
  function Size     (Font   : Font_Type)   return Natural;
  function Bold     (Font   : Font_Type)   return Boolean;
  function Italic   (Font   : Font_Type)   return Boolean;

  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type)  return Point_Type;
  function Inside   (Point  : Point_Type;
                     From   : Point_Type;
                     To     : Point_Type)  return Boolean;
  function "+"      (P1, P2 : Point_Type)  return Point_Type;
  function "-"      (P1, P2 : Point_Type)  return Point_Type;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype Color_Range is Colour_Range;
  subtype Color_Type  is Colour_Type;

private
        
  ----------------------------------------------------------------------------
  --  The private part of this package also provides a convenient place to
  --  declare the reference-counted types used by the generic package
  --  GtkJewl.Windows. Although they could be declared in the generic
  --  package GtkJewl.Windows, the use of a controlled type in a generic
  --  package would make it necessary to make all instantiations of the
  --  package at library level, a restriction which would probably baffle
  --  those for whom this software is intended.
  ----------------------------------------------------------------------------
  --
  --  A type which includes a reference count:
  --
  type Reference_Counted_Type is tagged limited
    record
      Count : Natural := 1;
    end record;

  ----------------------------------------------------------------------------
  --
  --  A primitive operation called when the last reference is deleted:
  --
  procedure Cleanup (Object : in out Reference_Counted_Type);

  ----------------------------------------------------------------------------
  --
  --  A pointer to the class of reference-counted types:
  --
  type Reference_Counted_Ptr is access all Reference_Counted_Type'Class;

  ----------------------------------------------------------------------------
  --
  --  A record which contains a pointer to a reference-counted object:
  --
  type Controlled_Type is new Ada.Finalization.Controlled with
    record
      Pointer : Reference_Counted_Ptr;
    end record;

  ----------------------------------------------------------------------------
  --
  --  Controlled operations:
  --
  procedure Finalize (Object : in out Controlled_Type);
  procedure Adjust   (Object : in out Controlled_Type);

end GtkJewl;
