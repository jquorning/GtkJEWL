------------------------------------------------------------------------------
--
--         G T K J E W L . D O U B L E _ B U F F E R
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

with Glib;             use Glib;
with Gtk.Drawing_Area;
with Gdk.Pixmap;
with Gdk.Drawable;

package GtkJewl.Double_Buffer is

   type Gtk_Double_Buffer_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Gtk_Double_Buffer is access all Gtk_Double_Buffer_Record'Class;

   procedure Gtk_New (Buffer : out Gtk_Double_Buffer);
   procedure Initialize (Buffer : access Gtk_Double_Buffer_Record'Class);

   function Get_Pixmap (Buffer : access Gtk_Double_Buffer_Record)
                       return Gdk.Drawable.Gdk_Drawable;
   --  Gives access to the buffer. This is the only widget on which you
   --  should draw (any drawing done directly on Buffer will be lost).

   procedure Clear (Buffer : access Gtk_Double_Buffer_Record);

private

   type Gtk_Double_Buffer_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
   record
      Pixmap : Gdk.Pixmap.Gdk_Pixmap := Gdk.Pixmap.Null_Pixmap;
      Max_Height:Gint:=0;
      Max_Width :Gint:=0;
   end record;

end GtkJewl.Double_Buffer;
