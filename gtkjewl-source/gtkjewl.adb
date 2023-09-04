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


with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with ada.Text_IO; use Ada.Text_IO;

with Pango.font;use Pango.font;
with Pango.Enums;use Pango.Enums;
with Glib; use GLib;

package body GtkJewl is

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  ----------------------------------------------------------------------------
  --
  --  Light: generate a lightened version of a given colour by increasing the
  --         intensity of each hue by 50%.
  --
  function Light (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => (Colour.Red+Colour_Range'Last+1)/2,
            Green => (Colour.Green+Colour_Range'Last+1)/2,
            Blue  => (Colour.Blue+Colour_Range'Last+1)/2);
  end Light;

  ----------------------------------------------------------------------------
  --
  --  Dark: generate a darkened version of a given colour by decreasing the
  --        intensity of each hue by 50%.
  --
  function Dark (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => Colour.Red/2,
            Green => Colour.Green/2,
            Blue  => Colour.Blue/2);
  end Dark;

  ----------------------------------------------------------------------------
  --
  --  Font: create a Font_Type structure which has the length of the font
  --        name as a discriminant.
  --
  function Font  (Name   : String;
                  Size   : Positive;
                  Bold   : Boolean  := False;
                  Italic : Boolean  := False) return Font_Type is
    F : Font_Type:=new Font_Type_Record(Name'Length);
  begin
    F.Name   := Name;
    F.Size   := Size;
    F.Bold   := Bold;
    F.Italic := Italic;
    return F;
  end Font;


  ----------------------------------------------------------------------------
  --
  --  Font2String: get the Font_Type and return the name of a font's typeface.
  --
  function Font2String (Font: Font_Type) return String is
     package Mypos is new Integer_Io(Integer);use Mypos;
     Str : String(1..300);
     Aux : String(1..5);
     slen: integer;
  begin
     if Font = null then
        return "";
     end if;
     slen:=Font.Name'length;
     Str(1..slen):=Font.Name;
     if Font.Bold then
        slen:=slen+5;
        Str(1..slen):=Str(1..slen-5) & " Bold";
     end if;
     if Font.Italic then
        slen:=slen+7;
        Str(1..slen):=Str(1 .. slen-7) & " Italic";
     end if;
     Mypos.Put(Aux,Integer(Font.Size),10);
     slen:=slen+aux'length+1;

     Str(1..Slen):=Str(1..Slen-Aux'Length-1) & " " & Aux;
     declare
        S:String:=Str(1..Slen);
     begin
       return S;
     end;
  end Font2string;
  ----------------------------------------------------------------------------
  --
  --  InStr: Busca un patron en un string
  --
  function Instr(Str1:in String ; Patron:in String) return Integer is
     I1 : Integer;
     I2 : Integer;
  begin
     for I in 1 .. Str1'Length loop
        I2:=1;I1:=I;
        while ( (I1<=Str1'Length) and then (I2<=Patron'Length) and then (Str1(I1)=Patron(I2)) ) loop
              I1:=I1+1;
              I2:=I2+1;
        end loop;
        if I2 > Patron'Length then
           return I;
        end if;
     end loop;
     return 0;
  end Instr;
  ----------------------------------------------------------------------------
  --
  --  String2Font: get the String and return the Font_Type of a font's typeface.
  --
  function String2Font (Fstr: String) return Font_Type is
      F : Pango.Font.Pango_Font_Description;
  begin
     F:=From_String(Fstr);
     declare
        FontNew:Font_Type:=new Font_Type_Record(Get_Family(F)'Length);
     begin
        FontNew.Name:=Get_Family(F);
        if InStr(Fstr,"Bold") > 0 then
           FontNew.Bold:=True;
        else 
           FontNew.Bold:=False;
        end if;
        if InStr(Fstr,"Italic") > 0 then
           FontNew.Italic:=True;
        else
           FontNew.Italic:=False;
        end if;
        FontNew.size:=Positive(Get_Size(F)/pango_scale);
        return FontNew;      
     end;
  end String2Font;

  ----------------------------------------------------------------------------
  --
  --  Name: get the name of a font's typeface.
  --
  function Name (Font : Font_Type) return String is
  begin
    return Font.Name;
  end Name;

  ----------------------------------------------------------------------------
  --
  --  Size: get the size of a font in points.
  --
  function Size (Font : Font_Type) return Natural is
  begin
    return Font.Size;
  end Size;

  ----------------------------------------------------------------------------
  --
  --  Bold: True is the specified font is bold.
  --
  function Bold (Font : Font_Type) return Boolean is
  begin
    return Font.Bold;
  end Bold;

  ----------------------------------------------------------------------------
  --
  --  Italic: True is the specified font is italic.
  --
  function Italic (Font : Font_Type) return Boolean is
  begin
    return Font.Italic;
  end Italic;

  ----------------------------------------------------------------------------
  --
  --  Endpoint: calculate the endpoint of a line drawn from a specified origin
  --            for a given length at a given angle.
  --
  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type) return Point_Type is
  begin
    return (From.X + Integer(Float(Length)*Sin(Float(Angle),360.0)),
            From.Y - Integer(Float(Length)*Cos(Float(Angle),360.0)));
  end Endpoint;

  ----------------------------------------------------------------------------
  --
  --  Inside: test if Point is inside the rectangle defined by From and To,
  --          bearing in mind that any two opposite corners can be given
  --          (not necessarily top left and bottom right).
  --
  function Inside (Point : Point_Type;
                   From  : Point_Type;
                   To    : Point_Type) return Boolean is
  begin
    return Point.X >= Integer'Min(From.X,To.X) and
           Point.X <= Integer'Max(From.X,To.X) and
           Point.Y >= Integer'Min(From.Y,To.Y) and
           Point.Y <= Integer'Max(From.Y,To.Y);
  end Inside;

  ----------------------------------------------------------------------------
  --
  --  "+": add two points (P1.X + P2.X, P1.Y + P2.Y).
  --
  function "+" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X+P2.X, Y => P1.Y+P2.Y);
  end "+";

  ----------------------------------------------------------------------------
  --
  --  "-": subtract two points (P1.X - P2.X, P1.Y - P2.Y).
  --
  function "-" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X-P2.X, Y => P1.Y-P2.Y);
  end "-";

  ----------------------------------------------------------------------------
  --
  --                I N T E R N A L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Free: deallocate a reference-counted object.
  --
  procedure Free is new Ada.Unchecked_Deallocation
                                (Reference_Counted_Type'Class,
                                 Reference_Counted_Ptr);

  procedure FreeFont is new Ada.Unchecked_Deallocation
                                (Font_Type_Record'Class,
                                 Font_Type);

  ----------------------------------------------------------------------------
  --
  --  Cleanup: the finalisation primitive for reference-counted types.
  --           Override this for derived types to do something useful.
  --
  procedure Cleanup (Object : in out Reference_Counted_Type) is
  begin
    null;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Finalize: decrement the reference count when an object containing
  --            a pointer to a reference-counted object is destroyed.
  --            When the reference count reaches zero, finalise the
  --            reference-counted object and free its memory.
  --
  procedure Finalize (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      if Object.Pointer.Count > 0 then
        Object.Pointer.Count := Object.Pointer.Count - 1;
        if Object.Pointer.Count = 0 then
          Cleanup (Object.Pointer.all);
          Free (Object.Pointer);
        end if;
      end if;
    end if;
  end Finalize;

  ----------------------------------------------------------------------------
  --  Adjust: bump the reference count when copying an object containing a
  --          pointer to a reference-counted object. Do nothing if the
  --          pointer is null.
  --
  procedure Adjust (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      Object.Pointer.Count := Object.Pointer.Count + 1;
    end if;
  end Adjust;

end GtkJewl;
