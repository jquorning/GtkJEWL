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

with Glib;            use Glib;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Style;       use Gtk.Style;
with Gdk.Drawable;    use Gdk.Drawable;
with Gdk.Event;       use Gdk.Event;
with Gdk.Window;      use Gdk.Window;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gtk.Drawing_Area;use Gtk.Drawing_Area;
with Gtk.Handlers;    use Gtk.Handlers;
with Gdk.Gc;          use Gdk.Gc;
with Gdk.Color;       use Gdk.Color;
with Gtk.Widget;      use Gtk.Widget;
pragma Elaborate_All (Gtk.Handlers);

with Ada.Text_Io;

package body GtkJewl.Double_Buffer is

   package Event_Cb is new Gtk.Handlers.Return_Callback
      (
      Widget_Type => Gtk_Double_Buffer_Record, 
      Return_Type => Boolean);

   package Void_Cb is new Gtk.Handlers.Callback
      (Widget_Type => Gtk_Double_Buffer_Record);

   function Configure (
         Buffer : access Gtk_Double_Buffer_Record'Class; 
         Event  :        Gdk.Event.Gdk_Event             ) 
     return Boolean; 
   --  Callback when the buffer is resized

   function Expose (
         Buffer : access Gtk_Double_Buffer_Record'Class; 
         Event  :        Gdk.Event.Gdk_Event             ) 
     return Boolean; 
   --  Callback when the window needs to redraw itself on the screen

   function Create_Internal_Pixmap (
         Buffer : access Gtk_Double_Buffer_Record'Class ) 
     return Gdk.Pixmap.Gdk_Pixmap; 
   --  Create one of the internal pixmaps used by the buffer, with the
   --  correct size.

   procedure Destroy_Cb (
         Buffer : access Gtk_Double_Buffer_Record'Class ); 
   --  Called when the widget is destroyed, to free up some memory

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (
         Buffer :    out Gtk_Double_Buffer ) is 
   begin
      Buffer := new Gtk_Double_Buffer_Record;
      Double_Buffer.Initialize (Buffer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (
         Buffer : access Gtk_Double_Buffer_Record'Class ) is 
      Style : Gtk_Style;  
      Color : Gdk_Color;  
   begin
      Gtk.Drawing_Area.Initialize (Buffer);
      Set_Events(Gtk_Widget(Buffer),0);

      --  Note: the pixmap is created in the resize routine
      --  and the GC is created in the Realize_Cb routine.
      Gtk.Widget.Set_Double_Buffered(Gtk_Widget(Buffer),True);

      --  Set White for initial background color
      Style:=Gtk.Style.Copy(Gtk.Widget.Get_Style(Gtk_Widget(Buffer)));
      Gdk.Color.Set_Rgb(Color, 65535, 65535, 65535);
      gtk.Style.Set_Foreground(Style,State_Normal,Color);
      Gtk.Style.Set_Background(Style,State_Normal,Color);
      Gtk.Widget.Set_Style(Gtk_Widget(Buffer),Style);

         --  Connect the signals
      Event_Cb.Connect (Buffer, "configure_event",
            Event_Cb.To_Marshaller (Configure'access));
      Event_Cb.Connect (Buffer, "expose_event",
            Event_Cb.To_Marshaller (Expose'access));
      Void_Cb.Connect (Buffer, "destroy",
            Void_Cb.To_Marshaller (Destroy_Cb'access));

      Set_Events(Gtk_Widget(Buffer),All_Events_Mask);

   end Initialize;

      ----------------------------
      -- Create_Internal_Pixmap --
      ----------------------------

   function Create_Internal_Pixmap (
         Buffer : access Gtk_Double_Buffer_Record'Class ) 
     return Gdk.Pixmap.Gdk_Pixmap is 
      Pix : Gdk.Pixmap.Gdk_Pixmap:=Gdk.Pixmap.Null_Pixmap;
      use gdk.pixmap;
   begin
      --If size change, create a new back buffer
      if Get_Allocation_Width (Buffer) > Buffer.Max_Width or Get_Allocation_Height (Buffer) > Buffer.Max_Height then
         Buffer.Max_Width:=Get_Allocation_Width (Buffer);
         Buffer.Max_Height:=Get_Allocation_Height (Buffer);
         Gdk.Pixmap.Gdk_New (Pix, Get_Window (Buffer), Gint(Buffer.Max_Width), Gint(Buffer.Max_Height));
      end if;
      return Pix;
   end Create_Internal_Pixmap;

      ---------------
      -- Configure --
      ---------------

   function Configure (
         Buffer : access Gtk_Double_Buffer_Record'Class; 
         Event  :        Gdk.Event.Gdk_Event             ) 
     return Boolean is 
         Old_Pixmap : Gdk.Pixmap.Gdk_Pixmap:=Buffer.Pixmap;
         use type Gdk.Pixmap.Gdk_Pixmap;
         Width  : Gint;
         Height : Gint;
   begin 
      --Give a new pixmap, if the size change
      Buffer.Pixmap := Create_Internal_Pixmap (Buffer);
      if Buffer.Pixmap /= Gdk.Pixmap.Null_Pixmap then
         if Old_Pixmap /= Gdk.Pixmap.Null_Pixmap then
            --If new pixmap created, clear and copy the older content
            Clear(Buffer);
            Get_Size (Gdk_Window (Old_Pixmap), Width, Height);
            Gdk.Drawable.Draw_Drawable
            (Buffer.Pixmap,
               Get_Foreground_Gc (Get_Style (Buffer), State_Normal),
               Src      => Old_Pixmap,
               Xdest    => 0,
               Ydest    => 0,
               Xsrc     => 0,
               Ysrc     => 0,
               Width    => Width,
               Height   => Height);
            --Unref the old back buffer
            Gdk.Pixmap.Unref (Old_Pixmap);
         end if;
      else
         --Return to the old pixmap if no size changed
         Buffer.Pixmap:= Old_Pixmap;
      end if;

      return False;
   end Configure;

      ----------------
      -- Destroy_Cb --
      ----------------

   procedure Destroy_Cb (
         Buffer : access Gtk_Double_Buffer_Record'Class ) is 
         use type Gdk.Pixmap.Gdk_Pixmap;
      begin
         if Buffer.Pixmap /= null then
            Gdk.Pixmap.Unref (Buffer.Pixmap);
         end if;
      end Destroy_Cb;

      ------------
      -- Expose --
      ------------

   function Expose (
         Buffer : access Gtk_Double_Buffer_Record'Class; 
         Event  :        Gdk.Event.Gdk_Event             ) 
     return Boolean is 
         Area : Gdk.Rectangle.Gdk_Rectangle := Gdk.Event.Get_Area (Event);
   begin
      --  Copy the double buffer to the screen
      Gdk.Drawable.Draw_Drawable
            (Get_Window (Buffer),
            Get_Foreground_Gc (Get_Style (Buffer), State_Normal),
            Src      => Buffer.Pixmap,
            Xdest    => Area.X,
            Ydest    => Area.Y,
            Xsrc     => Area.X,
            Ysrc     => Area.Y,
            Width    => Gint (Area.Width),
            Height   => Gint (Area.Height));
      return False;
   end Expose;

      ----------------
      -- Get_Pixmap --
      ----------------

   function Get_Pixmap (
         Buffer : access Gtk_Double_Buffer_Record ) 
     return Gdk.Drawable.Gdk_Drawable is 
   begin
         return Gdk.Drawable.Gdk_Drawable (Buffer.Pixmap);
   end Get_Pixmap;

      -----------
      -- Clear --
      -----------

   procedure Clear (
        Buffer : access Gtk_Double_Buffer_Record ) is 
         Width  : Gint;
         Height : Gint;
   begin
      Get_Size (Gdk_Window (Buffer.Pixmap), Width, Height);
      --Draw a rectangle with background color
      Gdk.Drawable.Draw_Rectangle
         (Buffer.Pixmap,
            Get_Background_Gc (Get_Style (Buffer),State_Normal),
            Filled   => True,
            X        => 0,
            Y        => 0,
            Width    => Width,
            Height   => Height);

   end Clear;

end GtkJewl.Double_Buffer;
