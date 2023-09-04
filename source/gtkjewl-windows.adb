------------------------------------------------------------------------------
--
--                     G T K J E W L . W I N D O W S
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

with GtkJewl.Window_Implementation;
use GtkJewl.Window_Implementation;
with GtkJewl.Canvas_Implementation;
use GtkJewl.Canvas_Implementation;
with Gtkjewl.Double_Buffer;
use GtkJewl.Double_Buffer;

with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Tags;
use Ada.Tags;
with Ada.Streams.Stream_Io;
use Ada.Streams.Stream_Io;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;


with ada.Text_Io;

with Gtk.Button;            use Gtk.Button;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Radio_Button;      use Gtk.Radio_Button;
with Gtk.Toggle_Button;     use Gtk.Toggle_Button;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Gentry;            use Gtk.Gentry;
with Gtk.Misc;              use Gtk.Misc;
with Gtk.Image;             use Gtk.Image;

with Gtk.Label;             use Gtk.Label;
with gtk.accel_label;       use gtk.accel_label;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Main;              use Gtk.Main;
with Gtk.Handlers;          use Gtk.Handlers;
with gtk.marshallers;       use gtk.marshallers;
with Gtk.Object;            use Gtk.Object;
with Gtk.Widget;            use Gtk.Widget;
with Gdk.Window;            use Gdk.Window;
with Gdk.Window_attr;       use Gdk.Window_attr;
with Gtk.Window;            use Gtk.Window;
with Gtk.Fixed;             use Gtk.Fixed;
with Gtk.Container;         use Gtk.Container;
with gtk.bin;               use gtk.bin;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Menu_bar;          use Gtk.Menu_bar;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Check_Menu_Item;   use Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item;   use Gtk.Radio_Menu_Item;
with Gtk.separator_menu_item;   use Gtk.Separator_Menu_Item;
with Gtk.Menu_shell;        use Gtk.Menu_shell;
with Glib;                  use Glib;
with Glib.convert;          use Glib.convert;
with Glib.Error;            use Glib.Error;
with Glib.Object;           use Glib.Object;
with Glib.unicode;          use Glib.unicode;
with Gdk;               use Gdk;
with gdk.types;         use gdk.types;
with gdk.types.keysyms; use gdk.types.keysyms;
with Gdk.rectangle;     use Gdk.rectangle;
with Gdk.bitmap;        use Gdk.bitmap;
with Gdk.Pixmap;        use Gdk.Pixmap;
with Gdk.PixBuf;        use Gdk.PixBuf;
with Gdk.Color;         use Gdk.Color;
with Gtk.Box;           use Gtk.Box;
with Gtk.Dialog;        use Gtk.Dialog;
with Gtk.Pixmap;        use Gtk.Pixmap;
with Gtk.List;          use Gtk.List;
with Gtk.List_Item;        use Gtk.List_Item;
with Gtk.Gentry;           use Gtk.Gentry;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Text_buffer;      use Gtk.Text_buffer;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Text_View;        use Gtk.Text_View;
with gtk.text_tag;         use gtk.text_tag;
with gtk.text_tag_table;   use gtk.text_tag_table;
with gtk.clipboard;        use gtk.clipboard;
with Glib.Glist;           use Glib.Glist; 

use Gtk.Widget.Widget_List;
with Gtk.Adjustment;       use Gtk.Adjustment;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;

with Gtk.Drawing_Area;   use Gtk.Drawing_Area;
with Gdk.Drawable;       use Gdk.Drawable;
with Gdk.Gc;             use Gdk.Gc;
with Gdk.Font;           use Gdk.Font;
with Gdk.Event;          use Gdk.Event;

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Gtkada.Pixmaps;    use Gtkada.Pixmaps;
with Gtkada.Intl;       use Gtkada.Intl;
with Gtkada.Dialogs;    use Gtkada.Dialogs;

with Gtk.Style;         use Gtk.Style;
with Gtk.Alignment;     use Gtk.Alignment;
with Gtk.Stock;         use Gtk.Stock;

with Gtk.Font_Selection;         use Gtk.Font_Selection;
with gtk.color_selection;        use gtk.color_selection;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;
with Gtk.File_Selection;         use Gtk.File_Selection;

with Gtk.Progress;      use Gtk.Progress;
with Gtk.Progress_Bar;  use Gtk.Progress_Bar;
with Gtk.Grange;        use Gtk.Grange;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Scrollbar;     use Gtk.Scrollbar;
with Gtk.Spin_Button;   use Gtk.Spin_Button;

with gtk.tooltips;      use gtk.tooltips;

with Pango.enums;
use Pango.enums;
with Pango.Font;
use Pango.Font;
with Pango.Context;
use Pango.Context;

with Gtk.Type_Conversion;

package body GtkJewl.Windows is

   use type System.Address;
   use type Ada.Streams.Stream_Element_Offset;

   ----------------------------------------------------------------------------
   --  End-of-line string
   ----------------------------------------------------------------------------
   Eol : constant String := Ascii.Cr & Ascii.Lf;  

   --Variable that keeps a leader to I complete created object
   LastObject:Window_Ptr;
  
   --that it replaces in string a character by another one
   --Used for "Alt + key"
   function Replace (Str:String; C1:Character;C2:Character) return String is
      Outstr:String(1..Str'Length);
      i2:integer:=1;
   begin
      for I in 1 .. Str'Length loop
         if Str(I)=C1 then
            Outstr(I2):=C2;
         else
            outStr(I2):=Str(I);
         end if;
         I2:=I2+1;
      end loop;
      return outstr;
   end Replace;
   
  --Functions to transfrom pointers
  function To_Window_Ptr is new Ada.Unchecked_Conversion(System.Address,Window_Ptr);
  function To_SystemAddress is new Ada.Unchecked_Conversion(Window_Ptr,System.Address);

   ----------------------------------------------------------------------------
   --
   --              M I S C E L L A N E O U S   R O U T I N E S
   --
   ----------------------------------------------------------------------------
   --
   --  Show_Error: display a message box with an OK button and stop sign.
   --
   procedure Show_Error (
         Text  : in     String;           
         Title : in     String := "Error" ) is 
        I : gtkada.dialogs.Message_Dialog_Buttons;
   begin
      I:=Gtkada.Dialogs.Message_Dialog(
         Msg            => Locale_To_UTF8(Text),
         Dialog_Type    => Gtkada.dialogs.Error,
         Buttons        => Button_OK,
         Default_Button => Button_OK,
         Help_Msg       => "",
         Title          => Title,
         Justification  => gtk.enums.Justify_Center,
         Parent         => Window_Info.Active_Window);
   end Show_Error;

   ----------------------------------------------------------------------------
   --
   --  Show_Query: display a message box with Yes/No buttons and a question
   --              mark.
   --
   function Show_Query (
         Text  : in     String;           
         Title : in     String := "Query" ) 
     return Boolean is 
   begin
      return Gtkada.Dialogs.Message_Dialog(
         Msg            => Locale_To_UTF8(Text),
         Dialog_Type    => Gtkada.dialogs.Confirmation,
         Buttons        => Button_Yes or Button_No,
         Default_Button => Button_Yes,
         Help_Msg       => "",
         Title          => Title,
         Justification  => gtk.enums.Justify_Center,
         Parent         => Window_Info.Active_Window) = gtkada.dialogs.Button_Yes;
   end Show_Query;

   ----------------------------------------------------------------------------
   --
   --  Show_Message: display a message box with an OK button and an
   --                information sign.
   --
   procedure Show_Message (
         Text  : in     String;             
         Title : in     String := "Message" ) is 
      I : Gtkada.dialogs.Message_Dialog_Buttons;
   begin
      I:=Gtkada.Dialogs.Message_Dialog(
         Msg            => Locale_To_UTF8(Text),
         Dialog_Type    => Gtkada.dialogs.Information,
         Buttons        => Button_OK,
         Default_Button => Button_OK,
         Help_Msg       => "",
         Title          => Title,
         Justification  => gtk.enums.Justify_Center,
         Parent         => Window_Info.Active_Window);
   end Show_Message;

   ----------------------------------------------------------------------------
   --
   --  Show_Warning: display a message box with an OK button and an
   --                warning sign.
   --
   procedure Show_Warning (
         Text  : in     String;             
         Title : in     String := "Warning" ) is 
      I : Gtkada.dialogs.Message_Dialog_Buttons;
   begin
      I:=Gtkada.Dialogs.Message_Dialog(
         Msg            => Locale_To_UTF8(Text),
         Dialog_Type    => Gtkada.dialogs.Warning,
         Buttons        => Button_OK,
         Default_Button => Button_OK,
         Help_Msg       => "",
         Title          => Title,
         Justification  => gtk.enums.Justify_Center,
         Parent         => Window_Info.Active_Window);
   end Show_Warning;

   ----------------------------------------------------------------------------
   --  FUNCTIONALITY LOSE
   --  Play_Sound: play a sound held in a wave file.
   --
   procedure Play_Sound (
         Sound : in     String ) is 
   begin
      null;
   end Play_Sound;

   ----------------------------------------------------------------------------
   --
   --  Screen_Width: It obtains the wide one of the screen in pixels.
   --
   function Screen_Width return Natural is 
      x,y,width,height,depth:Gint;
   begin
      --We obtain values of geometry of the main controller of screen
      Gdk.Window.Get_Geometry(
         Window => Null_Window,
         X      => x,
         Y      => y,
         Width  => width,
         Height => height,
         Depth  => Depth);
      --Return the width   
      return Natural(width);
   end Screen_Width;

   ----------------------------------------------------------------------------
   --
   --  Screen_Height: it obtains the height of the screen in pixels.
   --
   function Screen_Height return Natural is 
      x,y,width,height,depth:Gint;
   begin
      --We obtain values of geometry of the main controller of screen
      Gdk.Window.Get_Geometry(
         Window => Null_Window,
         X      => x,
         Y      => y,
         Width  => width,
         Height => height,
         Depth  => depth);
      --Return height  
      return Natural(height);
   end Screen_Height;

   ----------------------------------------------------------------------------
   --
   --                I N T E R N A L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Get_Internals: check that a window has been initialised and return a
   --                 pointer to its Window_Internals structure, or raise an
   --                 Invalid_Window exception. Additional parameters are
   --                 used to generate a meaningful message to accompany
   --                 the exception.
   --
   function Get_Internals (
         Window    : in     Window_Type'Class; 
         Operation : in     String             ) 
     return Window_Ptr is 
   begin
      if Window.Internals.Pointer = null then
         Raise_Exception (Invalid_Window'Identity,
            External_Tag(Window'Tag) &
            ": window not initialised in call to " & Operation);
      end if;
      return Window_Ptr(Window.Internals.Pointer);
   end Get_Internals;

   ----------------------------------------------------------------------------
   --
   --  Get_Internals: check that a common dialog has been initialised and
   --                 return a pointer to its Common_Dialog_Internals, or
   --                 raise an Invalid_Window exception. Additional parameters
   --                 are used to generate a meaningful message to accompany
   --                 the exception.
   --
   function Get_Internals (
         Dialog    : in     Common_Dialog_Type'Class; 
         Operation : in     String                    ) 
     return Common_Dialog_Ptr is 
   begin
      if Dialog.Internals.Pointer = null then
         Raise_Exception (Invalid_Window'Identity,
            External_Tag(Dialog'Tag) &
            ": dialog not initialised in call to " & Operation);
      end if;
      return Common_Dialog_Ptr(Dialog.Internals.Pointer);
   end Get_Internals;

   ----------------------------------------------------------------------------
   --
   --  Add: add an object to the end of a canvas drawing list and invalidate
   --       the canvas window so it will be repainted. The second parameter
   --       enables the actual operation name to be passed to Get_Internals.
   --       Repaint parameter force the redrawing of canvas or not
   --
   procedure Add (
         Canvas    : in     Canvas_Type;      
         Operation : in     String;           
         Object    : in     Canvas_Object_Ptr;
         Repaint   : in     Boolean := True ) is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, Operation));  
   begin
      C.Monitor.Add (Object);
      if Repaint then
         C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
      end if;
   end;

   ----------------------------------------------------------------------------
   --
   --  Fill_Child: create a child window with specified characteristics.
   --                The last parameter enables the actual operation name
   --                to be passed to Get_Internals.
   --
   procedure Fill_Child (
         Window    : in out Window_Type'Class;    
         Parent    : in     Container_Type'Class; 
         Origin    : in     Point_Type;           
         Width     : in     Integer;              
         Height    : in     Integer;              
         Font      : in     Font_Type;            
         Id        : in     Integer;              
         Operation : in     String                ) is 
      P : Container_Ptr := Container_Ptr (Get_Internals (Parent, Operation));  
      X : Window_Ptr    := Get_Internals (Window, Operation);  
   begin
      -- Fill in links to parent and siblings
      X.Parent := P;
      if P.Last = null then
         P.First := Window.Internals;
      else
         P.Last.Next := Window.Internals;
      end if;
      P.Last := X;
      
      -- Fill in the command code associated with this window
      X.Action := Id;

      -- Fill in the window dimensions
      X.Top    := Origin.Y;
      X.Left   := Origin.X;
      X.Height := Height;
      X.Width  := Width;

      -- Create the font, or use the parent font if no font name is given
      Set_Font (Window, Font);
      
   end Fill_Child;

   ----------------------------------------------------------------------------
   --
   --                   I M A G E   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Image: load a bitmap image from a specified file.
   --
   function Image (Name : String ) return Image_Type is 
      Img:Image_Type;
      Pointer : Image_Ptr:= new Image_Internals;
      Error : Gerror:=null;
   begin
      Gdk_New_From_File (Pointer.Img, Name, Error);
      if Error /= null then
         Error_Free (Error);
         if Pointer.Img /=null then
            Unref(Pointer.Img);
         end if;
      else   
         -- Fill in image structure
         Pointer.Width  := Natural(Get_Width(Pointer.Img));
         Pointer.Height := Natural(Get_Height(Pointer.Img));
         Img.Internals.Pointer := Reference_Counted_Ptr(Pointer);
      end if;
      
    return Img;
   end Image;

   ----------------------------------------------------------------------------
   --
   --  Valid: get the width of the specified image.
   --
   function Valid (Image : Image_Type ) return Boolean is 
   begin
      return Image.Internals.Pointer /= null;
   end Valid;

   ----------------------------------------------------------------------------
   --
   --  Width: get the width of the specified image.
   --
   function Width (Image : Image_Type ) return Natural is    
   begin
      if Valid(Image) then
         return Image_Internals(Image.Internals.Pointer.All).Width;
      else
         return 0;
      end if;
   end Width;

   ----------------------------------------------------------------------------
   --
   --  Height: get the height of the specified image.
   --
   function Height (Image : Image_Type ) return Natural is 
   begin
      if Valid(Image) then
         return Image_Internals(Image.Internals.Pointer.All).Height;
      else
         return 0;
      end if;
   end Height;

   ----------------------------------------------------------------------------
   --
   --                  W I N D O W   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Show: make a window visible or invisible, bringing visible windows to
   --        the foreground.
   --
   procedure Show (
         Window  : in     Window_Type;        
         Visible : in     Boolean     := True ) is 
      P : Window_Ptr := Get_Internals (Window, "Show");
   begin
      if Visible then
         Show(P.widget);
      else
         Hide(P.widget);
      end if;
   end Show;

   ----------------------------------------------------------------------------
   --
   --  Hide: use Show (above) to hide a window.
   --
   procedure Hide (Window : in     Window_Type ) is 
      P : Window_Ptr := Get_Internals (Window, "Hide");
   begin
      Show (Window, False);
   end Hide;

   ----------------------------------------------------------------------------
   --
   --  Focus: give the input focus to the specified window.
   --
   procedure Focus (Window : in     Window_Type ) is 
      P : Window_Ptr := Get_Internals (Window, "Focus");
   begin
      Gtk.Widget.Grab_Focus(P.widget);
   end Focus;

   ----------------------------------------------------------------------------
   --
   --  Visible: test if a window is visible.
   --
   function Visible (Window : Window_Type ) return Boolean is 
    P : Window_Ptr := Get_Internals (Window, "Visible");
  begin
      return gtk.widget.Visible_is_set(P.widget);
   end Visible;

   ----------------------------------------------------------------------------
   --
   --  Get_Origin: get the coordinates of a window's top left corner.
   --
   function Get_Origin (Window : Window_Type ) return Point_Type is 
      X,Y:Gint;
      P : Window_Ptr := Get_Internals (Window, "Get_Origin");
   begin
      if (P.Tipo = Type_Frame or P.Tipo = Type_Dialog) then
        return (P.Left - Frame_Border_Constant, P.Top - Frame_Title_Constant);
      else
         X:=Gtk.widget.Get_Allocation_X(P.widget)-Gint(2);
         Y:=Gtk.Widget.Get_Allocation_Y(P.Widget)-Gint(2);
         return (Integer(X),Integer(Y));
      end if;
   end Get_Origin;

   ----------------------------------------------------------------------------
   --
   --  Get_Width: get the width of a window.
   --
   function Get_Width (Window : Window_Type ) return Natural is 
      width:Allocation_int;
       P : Window_Ptr := Get_Internals (Window, "Get_Width");
     begin
      if (P.Tipo = Type_Frame or P.Tipo = Type_Dialog) then
         return Natural(P.Width + Frame_Width_Constant);
      else
         Width:=Gtk.Widget.Get_Allocation_Width(P.Widget);    
         return Natural (width);
      end if;
   end Get_Width;

   ----------------------------------------------------------------------------
   --
   --  Get_Height: get the height of a window.
   --
   function Get_Height (Window : Window_Type ) return Natural is 
      height:Allocation_int;
      P : Window_Ptr := Get_Internals (Window, "Get_Height");
   begin
      if (P.Tipo = Type_Frame or P.Tipo = Type_Dialog) then
         return Natural(P.Height + Frame_Height_Constant);
      else
         Height:=Gtk.Widget.Get_Allocation_Height(P.Widget);    
         return Natural (Height);
      end if;
   end Get_Height;


   procedure Get_Actual_Bounds (
         Parent : in     Container_Ptr; 
         Top    : in out Integer;    
         Left   : in out Integer;    
         Width  : in out Integer;    
         Height : in out Integer;    
         Resize :    out Boolean     ) is 
     Pw,Ph : Gint;            
   begin
      Resize := Top < 0 or Left < 0 or Width <= 0 or Height <= 0;
      if Resize then
         if Parent = null then
            --Parent is Null, only in Frame and Dialog
            Pw:=Gint(Screen_Width);
            PH:=Gint(Screen_Height);
         else
            if Parent.Tipo = Type_Frame or Parent.Tipo = Type_Dialog then
               --Sizes of the client area in Frame or Dialog
               Pw:=Gint(Parent.Width);
               Ph:=Gint(Parent.Height);
            else   --Panel
               Pw:=Gtk.Widget.Get_Allocation_Width(Parent.Widget);     
               Ph:=Gtk.Widget.Get_Allocation_Height(Parent.Widget);    
            end if;            
            if Parent.Tipo = Type_Frame then
               if Gtk.Widget.Visible_Is_Set(Gtk_Widget(Main_Window_Ptr(Window_Ptr(Parent)).Menubar)) then
                  Ph:=Ph-Gint(Menu_Height_Constant);
               end if;
            end if;
         end if;
         if Left < 0 then
            Left := Left + Integer(Pw);
         end if;

         -- Recalculate top relative to parent bounds if necessary

         if Top < 0 then
            Top := Top + Integer(Ph);
         end if;

         -- Recalculate width relative to parent bounds if necessary

         if Width <= 0 then
            Width := Width + Integer(Pw);
         end if;

         -- Recalculate Height relative to parent bounds if necessary

         if Height <= 0 then
            Height := Height + Integer(Ph);
         end if;
      end if;

   end Get_Actual_Bounds;
   ----------------------------------------------------------------------------
   --
   --  Set_Origin: set the coordinates of a window's top left corner.
   --
   procedure Set_Origin_Internals (
         Window   : in     Window_Ptr; 
         Parent   : in     Container_Ptr; 
         Origin   : in     Point_Type;
         ForceSet : in Boolean := False   ) is 
      alloc : gtk.widget.Gtk_Allocation;
      X,Y,W,H : Integer;
      B:Boolean;
   begin
      X:=Origin.X;
      Y:=Origin.Y;
      W:=1;
      H:=1;
      Get_Actual_Bounds(Parent,Y,X,W,H,B);
      if B or Forceset then
         case Window.Tipo is
            when Type_frame | Type_Dialog =>
               Gdk.Window.Move(Get_Window(Window.Widget), 
                  Gint(Window.Left - Frame_Border_Constant),
                  Gint(Window.Top - Frame_Title_Constant));
            when Type_Menu | Type_Menuitem |Type_Action_Menuitem | Type_Check_Menuitem | Type_Radio_Menuitem =>
               return;
            when others =>
               Gtk.Fixed.Move(
                  Fixed  => Parent.Fix,
                  Widget => Window.Widget,
                  X      => Gint(X),
                  Y      => Gint(Y));
               Alloc.X:=Gint(X);
               Alloc.Y:=Gint(Y);
               Alloc.Height:=Gtk.Widget.Get_Allocation_Height(Window.Widget);
               Alloc.Width:=Gtk.Widget.Get_Allocation_Width(Window.Widget);
               Size_Allocate(Window.Widget,Alloc);
          end case;
      end if;
   end Set_Origin_Internals;

   procedure Set_Origin (
         Window : in     Window_Type; 
         Origin : in     Point_Type   ) is 
      P : Window_Ptr := Get_Internals (Window, "Set_Origin");
      C : Container_Ptr := Container_Ptr(P.Parent);
   begin
      if (P.Tipo = Type_Frame or P.Tipo = Type_Dialog) then
         P.Left:=Origin.X + Frame_Border_Constant;
         P.Top:=Origin.Y + Frame_Title_Constant;
      else
         P.Left:=Origin.X;
         P.Top:=Origin.Y;
      end if;         
      Set_Origin_Internals(P,C,Origin,True);
   end Set_Origin;
   ----------------------------------------------------------------------------
   --
   --  Set_Size: set the width and height of a window.
   --
   procedure Set_Size_Internals (
         Window   : in     Window_Ptr;      
         Parent   : in     Container_Ptr;
         Width    : in     Integer     := 0; 
         Height   : in     Integer     := 0;  
         ForceSet : in Boolean := False) is 
      W : Integer    := Width;  
      H : Integer    := Height;
      Alloc: Gtk.Widget.Gtk_Allocation;
      X,Y : Integer;
      B:Boolean;
      WPtr : Window_Ptr;
   begin
      X:=0;
      Y:=0;
      Get_Actual_Bounds(Parent,Y,X,W,H,B);
      if B or ForceSet then
         case Window.Tipo is
            when Type_Frame | Type_Dialog =>
               Gdk.Window.Move_Resize(Get_Window(Window.Widget), 
                  Gint(Window.Left - Frame_Border_Constant),
                  Gint(Window.Top - Frame_Title_Constant),
                  Gint(W), Gint(H));   
            when Type_Menu | Type_Menuitem |Type_Action_Menuitem | Type_Check_Menuitem | Type_Radio_Menuitem =>
               return;
            when others =>
               if W < 0 then
                  W:=0;
               end if;
               if H < 0 then
                  H:=0;
               end if;
            
               Gtk.Widget.Set_Size_Request(Window.Widget,Gint(W),Gint(H));
      
               Alloc.X:=gtk.widget.Get_Allocation_X(Window.Widget);
               Alloc.Y:=gtk.widget.Get_Allocation_Y(Window.Widget);
               Alloc.Height:=Gint(H);
               Alloc.Width:=Gint(W);
               
               Size_Allocate(Window.Widget,Alloc);
         end case;

         Wptr:=Window_Ptr(Window.First.Pointer);
         while Wptr /= null loop
            Set_Origin_Internals(Wptr, Container_Ptr(Window), (Wptr.Left,Wptr.Top));
            Set_Size_Internals(Wptr, Container_Ptr(Window), Wptr.Width, Wptr.Height);
            Wptr:=Window_Ptr(Wptr.Next.Pointer);
         end loop;
      end if;
   end Set_Size_Internals;

   procedure Set_Size (
         Window : in     Window_Type;      
         Width  : in     Natural     := 0; 
         Height : in     Natural     := 0  ) is 
      P : Window_Ptr := Get_Internals (Window, "Set_Size");
      C : Container_Ptr := Container_Ptr(P.Parent);
      W : Integer    := Width;  
      H : Integer    := Height;
   begin
      if Width = 0 then
         W := P.Width;   --Original value, for maintain relative sizes
      end if;    
      if Height = 0 then
         H := P.Height;  --Original value, for maintain relative sizes
      end if;
      
      case P.Tipo is
         when Type_Frame =>
         declare
            Cevent: Gdk.Event.Gdk_Event;
            B:Boolean;
         begin
            Do_Events;
            --Generate "configure_event" to resize the window and his dependents
            Allocate(Cevent,gdk.event.Configure,Get_Window(P.Widget));
            Set_X(Cevent,Gdouble(P.Left));
            Set_Y(Cevent,Gdouble(P.Top));
            if (W - Frame_Width_Min) < 0 then
               W:=Frame_Width_Min +1;
            end if;
            if (H - Frame_Height_Constant) < 0 then
               H:=Frame_Height_Constant+1;
            end if;
            Set_Width(Cevent,Gint(W - Frame_Width_Constant));
            Set_Height(Cevent,Gint(H - Frame_Height_Constant));
            B:=Gtk.Widget.Event(Widget => P.Widget,Event  => Cevent);
            if Is_Created(Cevent) then
               Free(Cevent);
            end if;
         end;
         when Type_Dialog | Type_Menu | Type_Menuitem |Type_Action_Menuitem | Type_Check_Menuitem | Type_Radio_Menuitem =>
            return;
         when others =>
            P.Width:=W;
            P.Height:=H;
            Set_Size_Internals(P,C,W,H,True);
      end case;
   end Set_Size;
   ----------------------------------------------------------------------------
   --
   --  Set_Font: change the font associated with a window and invalidate it
   --            so that it will be repainted.
   --
   procedure FreeFont is new Ada.Unchecked_Deallocation
                                (Font_Type_Record'Class,
                                Font_Type);
   procedure Set_Font (
         Window : in     Window_Type; 
         Font   : in     Font_Type    ) is 
      F : Pango.Font.Pango_Font_Description;
      P : Window_Ptr := Get_Internals (Window, "Set_Font");
      gfont: Gdk.font.Gdk_Font;
   begin
      if Font /= null then 
         if Font.Name'Length > 0 then
            Gfont:=Gdk.Font.From_Description(From_String(Font2string(Font)));
            if Gfont /= null then
               F:=From_String(Font2string(Font));         
               P.Font := String2font(To_String(F));
            else
               P.Font := null;
               F:=From_String(Font2string(Get_Font (Window)));
            end if;
         else  
            F:=From_String(Font2string(Get_Font (Window)));        
            if P.Font /= null then
               Freefont (P.Font);
               P.Font:=null;
            end if;
         end if;     
         if P.Label_Widget /= null then
            Modify_Font(P.Label_Widget,F);
         end if;
         Modify_Font(P.Widget,F);
         Queue_draw(P.Widget);
      end if;
   end Set_Font;

   ----------------------------------------------------------------------------
   --
   --  Get_Font: build a Font_Type structure for a window's current font.
   --
   function Get_Font (
         Window : Window_Type ) 
   return Font_Type is 
      P : Window_Ptr := Get_Internals (Window, "Get_Font");
   begin
      --Search for the first font scaling the object tree
      while P.Font = null loop
          P := Window_Ptr(P.Parent);
          exit when P = null;
      end loop;
      --If don't exist, default font is selected
      if P = null or else P.Font = null then
         return Default_Font;
      else
         return P.Font;  
      end if;
   end Get_Font;


   ----------------------------------------------------------------------------
   --
   --            C O N T A I N E R   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Get_Length: get the length of the text in a container.
   --
   function Get_Length (
         Container : Container_Type ) 
   return Natural is 
      P : Window_Ptr := Get_Internals (Container, "Get_Length");
      S : String := Get_Text(Container);
   begin
      return S'Length;
   end Get_Length;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text from a container.
   --
   function Get_Text (
         Container : Container_Type ) 
     return String is 
      P : Window_Ptr := Get_Internals (Container, "Get_Text");
   begin
      case P.Tipo is
         when Type_Frame | Type_Dialog =>
            return Locale_From_UTF8(Gtk.window.Get_Title(Gtk_Window(P.Widget)));
         when Type_Panel | Type_Menu =>
            return Locale_From_UTF8(Gtk.Label.Get_Text(Gtk_Label(P.Label_Widget)));
         when others =>
            return "";
      end case;         
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text from a container into a fixed-length
   --            string variable.
   --
   procedure Get_Text (
         Container : in     Container_Type; 
         Text      :    out String;            
         Length    :    out Natural            ) is 
      S : constant String := Get_Text (Container);  
   begin
      if S'Length > Text'Length then
         Text := S(S'First..S'First+Text'Length-1);
         Length := Text'Length;
      else
         Text(Text'First..Text'First+S'Length-1) := S;
         Length := S'Length;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Set_Text: store the specified text in a container.
   --
   procedure Set_Text (
         Container : in     Container_Type; 
         Text      : in     String             ) is 
      P : Window_Ptr := Get_Internals (Container, "Set_Text");
   begin
      case P.Tipo is
         when Type_Frame | Type_Dialog =>
            Gtk.window.Set_Title(Gtk_Window(P.Widget),Locale_To_UTF8(Text));
         when Type_Panel | Type_Menu =>
            Gtk.Label.Set_Text(Gtk_Label(P.Label_Widget),Locale_To_UTF8(Text));
         when others =>
            null;      
      end case;         
   end Set_Text;



   ----------------------------------------------------------------------------
   --
   --                   F R A M E   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --Function that generates the command of closing of the windows and dialogs, without destroying them
   function On_Window_Delete_Event (Object : access Gtk_Window_Record'Class;Window:System.Address) return Boolean is 
      P : Window_Ptr := To_Window_Ptr(Window);
   begin
      if P.Action >= 0 then
         Window_Info.Set_Command(P.Action);
      end if;
      --Return True, so that window is not eliminated
      return True;
   end On_Window_Delete_Event;
   
   --Procedure that the command associated to the sent event generates
   procedure Send_Event (Object : access Gtk_Widget_Record'Class;Window:System.Address ) is 
      P : Window_Ptr := To_Window_Ptr(Window);
   begin
      if P.Action >= 0 then
         Window_Info.Set_Command(P.Action);
      end if;
   end Send_Event;

   --Procedure that command generates when a window receives focus
  function On_Focus_In_Event(
     Window        : access Gtk_Window_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     FocusInWin    : Gtk_Window)
        return Boolean is
  begin
     Window_Info.Set_Active_Window(Gtk_Window(FocusInWin));
     return false;
  end On_Focus_In_Event;
  
  
  --Function that generates the resize and replacing of the elements with relative references
  function On_Frame_Configure_Event(
     Widget        : access Gtk_Window_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     WindowPtr     : System.Address)
        return Boolean is
     P : Window_Ptr := To_Window_Ptr(Windowptr);
  begin  
     if Gint(Gdk.Event.Get_X(Event)) /= Gint(P.Left) or Gint(Gdk.Event.Get_Y(Event)) /= Gint(P.Top) then
        P.Left:=Integer(Get_X(Event));
        P.Top :=Integer(Get_Y(Event));
     end if;

     if Gdk.Event.Get_Width(Event) /= Gint(P.Width) or Gdk.Event.Get_Height(Event) /= Gint(P.Height) then
        P.Width  := Integer(Gdk.Event.Get_Width(Event));
        P.Height := Integer(Gdk.Event.Get_Height(Event));
        if P.Height <= 0 then
               P.Height:=1;
        end if;
        if P.Width <= 0 then
               P.Width:=1;
        end if;
        Set_Size_Internals(P, null, P.Width, P.Height,True);
     end if;
     --Ptopagate event
     return False;        
  end On_Frame_Configure_Event;


   --
   --  Frame: construct a frame with the specified characteristics.
   --
   function Frame (
         Origin  : Point_Type;                  
         Width   : Positive;                    
         Height  : Positive;                    
         Title   : String;                      
         Command : Command_Type;                
         Font    : Font_Type    := Default_Font ) 
     return Frame_Type is 
      W : Frame_Type;  
      M : Main_Window_Ptr := new Main_Window_Internals;
      P : Window_Ptr      := Window_Ptr(M);
      Alloc : Gtk_Allocation;
   begin
       -- Set up the Window_Internals structure for a window with default
       -- placement
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      
      P.Action := Command_Type'Pos(Command);
      P.Top    := Origin.Y + Frame_Title_Constant;
      P.Left   := Origin.X + Frame_Border_Constant;
      P.Width  := Width - Frame_Width_Constant;
      P.Height := Height - Frame_Height_Constant;
      
      P.Tipo:=Type_Frame;
      
      --Create the object
      Gtk_New (Gtk_Window(P.Widget),Window_Toplevel);
      Set_Events(Gtk_Widget(P.Widget),All_Events_Mask);
      --if no active window set, set it
      if Window_Info.Active_Window = null then
         declare
            D : Gtk_Window;
         begin
            d:=Gtk_Window(P.Widget);
            Window_Info.Set_Active_Window(D);
            Window_Info.Get_Dialog(D);
         end;
      end if;
      --Asign the title
      Set_Title(Gtk_Window(P.Widget),Locale_To_UTF8(Title));
      --No modal, and Normal window
      Set_Modal(Gtk_Window(P.Widget), False);
      Set_Type_Hint(Gtk_Window(P.Widget),Gdk.Window.Window_Type_Hint_Normal);
      Set_Policy(
         Window       => Gtk_Window(P.Widget),
         Allow_Shrink => true,
         Allow_Grow   => true,
         Auto_Shrink  => True);
      --Initial position
      Set_Uposition(P.Widget,Gint(P.Left),Gint(P.Top));
      --Initial size
      Gtk.Window.Resize(
         Window => Gtk_Window(P.Widget),
         Width  => Gint(P.Width),
         Height => Gint(P.Height));
       -- Set the font now that the frame exists
      Set_Font (W, Font);
      
      Alloc.X:=Gint(P.Left);
      Alloc.Y:=Gint(P.Top);
      Alloc.Height:=Gint(P.Height);
      Alloc.Width:=Gint(P.Width);
      Size_Allocate(P.Widget,Alloc);
      --Principal box
      Gtk_New_Vbox (M.Vbox, False, 0);
      Add (Gtk_Window(P.Widget),M.Vbox);
      show(M.Vbox);
      --Menubar
      Gtk_New (M.Menubar);
      Set_USize (M.MenuBar, 0,Gint(Menu_Height_Constant));
      Set_Border_Width(M.Menubar,0);
      Pack_Start (M.Vbox, M.MenuBar, False, False, 0);
      hide(M.MenuBar);

      --Fix container
      Gtk_New(M.Box);
      Set_Shadow_Type(M.Box,Gtk.Enums.Shadow_In);
      Gtk_New(M.Fix);
      Pack_End(M.Vbox,M.Box,True,True,0);
      Add (M.Box,M.Fix);
      show(M.Fix);
      show(M.Box);    
      --Asign handlers to events
      Window_Cmd_Return_Callback.Connect( Gtk_Window(P.Widget),"delete_event",Window_Cmd_Return_Callback.To_Marshaller (On_Window_Delete_Event'access),To_SystemAddress(P));
      Window_GtkWindow_Return_Callback.Connect( Gtk_Window(P.Widget),"focus_in_event",Window_GtkWindow_Return_Callback.To_Marshaller (On_Focus_In_Event'access),Gtk_Window(P.Widget));
      Window_Address_Return_Callback.connect( Gtk_Window(P.Widget),"configure_event",Window_Address_Return_Callback.To_Marshaller (On_Frame_Configure_Event'access),To_SystemAddress(P));
      
      --Show all
      Show(P.Widget);

      return W;
   end Frame;

   ----------------------------------------------------------------------------
   --
   --  Frame: construct a frame with the specified characteristics and
   --         default placement.
   --
   function Frame (
         Width   : Positive;                    
         Height  : Positive;                    
         Title   : String;                      
         Command : Command_Type;                
         Font    : Font_Type    := Default_Font ) 
     return Frame_Type is 
      P : Point_Type;  
   begin
      --Default position, center in the screen
      P.X:=Screen_Width/2-Width/2;
      P.Y:=Screen_Height/2 -Height/2;
      return Frame (P,Width, Height, Title, Command, Font);
   end Frame;

   ----------------------------------------------------------------------------
   --
   --  Close: close and destroy a frame.
   --
   procedure Close ( Frame : in out Frame_Type ) is 
      P : Window_Ptr := Get_Internals (Frame, "Close");
  begin
      if Valid(Frame) then
        Hide(Frame);
        Frame.Internals.Pointer:=null;
      end if;
   end Close;

   ----------------------------------------------------------------------------
   --
   --  Valid: test if a frame is valid (i.e. if the window exists).
   --
   function Valid (
         Frame : Frame_Type ) 
         return Boolean is  
    P : Window_Ptr := Window_Ptr(Frame.Internals.Pointer);
   begin
    return P /= null and then P.Tipo = Type_Frame;
   end Valid;

   ----------------------------------------------------------------------------
   --
   --  Frame_Width: return the width of a frame's border.
   --
   function Frame_Width return Natural is 
   begin
        return Frame_Width_Constant;
   end Frame_Width;

   ----------------------------------------------------------------------------
   --
   --  Frame_Height: return the height of a frame's border.
   --
   function Frame_Height return Natural is 
   begin
         return Frame_Height_Constant;
   end Frame_Height;
   
   --------------------------------------------------------------------------
   --
   --  Do_Events: execute events pending
   --
   procedure Do_Events is 
      Dead:boolean;
   begin
      while Gtk.Main.Events_Pending loop
         Dead := Gtk.Main.Main_Iteration(False);
      end loop;
   end Do_Events;
   
   ----------------------------------------------------------------------------
   --
   --  Next_Command: ask the info monitor for the next command.
   --
   function Next_Command return Command_Type is 
      Cmd : Natural;  
      Dead:Boolean;
   begin
      while (not Window_Info.Test_Command) loop
         Dead:=Gtk.Main.Main_Iteration(True);
      end loop;
      Window_Info.Get_Command (Cmd);

      return Command_Type'Val(Cmd);
   end Next_Command;

   ----------------------------------------------------------------------------
   --
   --  Command_Ready: ask the info monitor if there is a command pending.
   --
   function Command_Ready return Boolean is 
   begin
      Do_Events;
      return Window_Info.Test_Command;
   end Command_Ready;

   ----------------------------------------------------------------------------
   --
   --                  D I A L O G   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Dialog: create a top-level dialog window.
   --
   function Dialog (
         Width   : Positive;                    
         Height  : Positive;                    
         Title   : String;                      
         Command : Command_Type;                
         Font    : Font_Type    := Default_Font ) 
     return Dialog_Type is 
      W : Dialog_Type;  
      X : Integer         := Integer (Screen_Width) / 2 - Width/2;  
      Y : Integer         := Integer (Screen_Height) / 2 - Height/2;  
      M : Main_Window_Ptr  := new Main_Window_Internals;
      P : Window_Ptr       := Window_Ptr(M);
  begin
    -- Set up the Window_Internals structure for a centred window

      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Action := Command_Type'Pos(Command);
      P.Top    := Y + Frame_Title_Constant;
      P.Left   := X + Frame_Border_Constant;
      P.Width  := Width-Frame_Width;
      P.Height := Height-Frame_Height;

      P.Tipo:=Type_Dialog;
     
      --Create object
      Gtk_New (Gtk_Window(P.Widget));
      Set_Events(Gtk_Widget(P.Widget),All_Events_Mask);
      --Asign title
      Set_Title(Gtk_Window(P.Widget),Locale_To_UTF8(Title));
      --Modal window, and Dialog aspect
      Set_Modal(Gtk_Window(P.Widget), True);
      Set_Type_Hint(Gtk_Window(P.Widget),Gdk.Window.Window_Type_Hint_Dialog);
      Set_Policy(
         Window       => Gtk_Window(P.Widget),
         Allow_Shrink => false,
         Allow_Grow   => false,
        Auto_Shrink  => False);
      --Initial size
      if P.Width < 0 then
        P.Width := 0;
      end if;  
      if P.Height < 0 then
        P.Height := 0;
      end if;  
      Set_Size_Request(Gtk_Window(P.Widget),Gint(P.Width),Gint(P.Height));

      Set_Resizable(Gtk_Window(P.Widget),False);
      gtk.window.Set_Destroy_With_Parent(Gtk_Window(P.Widget),True);
      --Initial position
      Set_Uposition(P.Widget,Gint(P.Left),Gint(P.Top));

      Set_Font (W, Font);
      --Inicialize fix container
      Gtk_New(M.Fix);
      Gtk.Container.Add(
        Container => Gtk_Container(P.Widget),
        Widget    => M.Fix);
      Show(M.Fix);
      
      --Asign handlers to events
      Window_Cmd_Return_Callback.Connect( Gtk_Window(P.Widget),"delete_event",Window_Cmd_Return_Callback.To_Marshaller (On_Window_Delete_Event'access),To_SystemAddress(P));
     
       --Show all, and hide all
      Show(P.Widget);
      Hide(P.Widget);      
     --Asign decorations for dialog aspect   
     Gdk.Window.Set_Decorations(
        Window => Get_Window(P.Widget),
        Decorations => Decor_Title);
     Gdk.Window.Set_Functions(
        Window => Get_Window(P.Widget),
        Functions => Func_Move + Func_Close);      
     --Modal type
     Gdk.Window.Set_Modal_Hint(
        Window => Get_Window(P.Widget),
        Modal  => True);
     --Don't Resize
     declare
        Geo : Gdk_Geometry;
     begin
        Geo.Min_Width := Gint(P.Width);
        Geo.Min_Height  := Gint(P.Height);
        Geo.Max_Width   := Gint(P.Width);
        Geo.Max_Height  := Gint(P.Height);
        Geo.Base_Width  := Gint(0);
        Geo.Base_Height := Gint(0);
        Geo.Width_Inc   := Gint(1);
        Geo.Height_Inc  := Gint(1);
        Geo.Min_Aspect  := Gdouble(1);
        Geo.Max_Aspect  := Gdouble(1);
        Geo.Win_Gravity := Gravity_Static;
        Gdk.Window.Set_Geometry_Hints(
           Window   => Get_Window(P.Widget),
           Geometry => Geo,
           Flags    =>  Gdk_Hint_Min_Size + Gdk_Hint_Max_Size+ Gdk_Hint_Aspect + Gdk_Hint_Resize_Inc); 
     end;
      -- Return the window object
      return W;
   end Dialog;

   ----------------------------------------------------------------------------
   --
   --  Execute: run a dialog until it issues a command. Note that dialogs
   --           are hidden rather than destroyed so that closing a dialog
   --           them won't make any attached controls disappear.
   --
   function Execute (
         Dialog : in     Dialog_Type ) 
     return Command_Type is 
      C : Command_Type;  
      P : Window_Ptr := Get_Internals (Dialog, "Execute");
      D : Gtk_window;  
   begin
        D:=Window_Info.Active_Window;

      if D /= null then
         Gtk.Window.Set_Transient_For(Gtk_Window(P.Widget),D);
      end if;  
      -- Make the window visible and bring it to the foreground
      Show(P.Widget);

      -- Wait for a command (which must be from this dialog, as dialog
      -- windows disable all other windows belonging to the application)
      C:=Next_Command;
      
      -- Hide the dialog window and return the command code
      Hide(P.Widget);

      return C;
   end Execute;

   ----------------------------------------------------------------------------
   --
   --  Dialog_Width: return the width of a dialog's border.
   --
   function Dialog_Width return Natural is 
   begin
      return Frame_Width;
   end Dialog_Width;

   ----------------------------------------------------------------------------
   --
   --  Dialog_Height: return the height of a dialog's border.
   --
   function Dialog_Height return Natural is 
   begin
      return Frame_Height;
   end Dialog_Height;

   ----------------------------------------------------------------------------
   --
   --                   P A N E L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Panel: create a panel (which is actually a Windows groupbox if it has
   --         a title, or a static control with an etched border if not).
   --
   function Panel (
         Parent : Container_Type'Class;               
         Origin : Point_Type;                         
         Width  : Integer;                            
         Height : Integer;                            
         Title  : String               := "";         
         Font   : Font_Type            := Parent_Font ) 
     return Panel_Type is 
      W : Panel_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals(Parent,"Panel"));
      P : Container_Ptr := new Container_Internals;
      Label:Gtk_Widget;
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Panel;
      --Create object
      Gtk_New(Gtk_Frame(P.Widget));
      Gtk_New(Gtk_Label(Label),Locale_To_UTF8(Title));
      Set_Label_Widget(Gtk_Frame(P.Widget),Label);
      show(Label);
      Set_Shadow_Type(Gtk_Frame(P.Widget), gtk.enums.Shadow_Etched_In);
      --Include the parent fix container
      Put(C.Fix,Gtk_Frame(P.Widget),Gint(Origin.X),Gint(Origin.Y));
      --Asign the proper fix container
      Gtk_New(P.Fix);
      Add(Gtk_Frame(P.Widget),Gtk_Widget(P.Fix));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Panel");

      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      

      Show(P.Fix);
      
      P.Label_Widget:=Label;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      Show(Gtk_Frame(P.Widget));         
      return W;
   end Panel;

   ----------------------------------------------------------------------------
   --
   --                    M E N U   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Menu: create a menu attached to a frame.
   --
   function Menu (
         Parent : Frame_Type'Class; 
         Text   : String            ) 
     return Menu_Type is 
      M : Menu_Type;  
      P : Main_Window_Ptr := Main_Window_Ptr(Get_Internals (Parent, "Menu"));
      W : Container_Ptr := new Container_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Menu;
      --In case we have created some element of screen before it appears the menu, it has to be recalculated
      if not Visible_Is_Set(P.Menubar) then
         Show(P.Menubar);
         Set_Size_Internals(Window_Ptr(P), null, P.Width, P.Height,true);
      end if;   
      Gtk_New_With_Mnemonic(Gtk_Menu_Item(W.widget),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      --Label related
      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);
      Append(Gtk_Menu_Shell(P.Menubar), Gtk_Menu_Item(W.Widget));
      Gtk_New(W.Menu);
      Set_Submenu(Gtk_Menu_Item(W.Widget), W.Menu);
      show(W.Widget);
      Show(W.Menu);

      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,-1,"Menu");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Menu;

   ----------------------------------------------------------------------------
   --
   --  Menu: create a menu attached to another menu.
   --
   function Menu (
         Parent : Menu_Type'Class; 
         Text   : String           ) 
     return Menu_Type is 
      M : Menu_Type;  
      P : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Menu"));
      W : Container_Ptr := new Container_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Menu;  
      Gtk_New_With_Mnemonic(Gtk_Menu_Item(W.Widget),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      --Label related
      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      --Left Align
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);
      --Add to menu bar
      Append(Gtk_Menu_Shell(P.Menu), Gtk_Menu_Item(W.Widget));
      --Generate associated submenu
      Gtk_New(W.Menu);
      Set_Submenu(Gtk_Menu_Item(W.Widget), W.Menu);
      show(W.Widget);
      Show(W.Menu);

      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,-1,"Menu");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Menu;

   ----------------------------------------------------------------------------
   --
   --  Menu_Height: return the height of a menubar.
   --
   function Menu_Height return Natural is 
   begin
      return Menu_Height_Constant;
    end Menu_Height;

   ----------------------------------------------------------------------------
   --
   --                 C O N T R O L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Enable: enable or disable a control.
   --
   procedure Enable (
         Control : in     Control_Type;        
         Enabled : in     Boolean      := True ) is 
      P : Window_Ptr := Get_Internals (Control, "Enable");  
   begin
      Gtk.Widget.Set_Sensitive(P.widget, Enabled);
   end Enable;

   ----------------------------------------------------------------------------
   --
   --  Disable: use Enable (above) to disable a control.
   --
   procedure Disable (
         Control : in     Control_Type ) is 
      P : Window_Ptr := Get_Internals (Control, "Disable");  
   begin
      Enable (Control_Type'Class(Control), False);
   end Disable;

   ----------------------------------------------------------------------------
   --
   --  Enabled: test if a control is enabled.
   --
   function Enabled (
         Control : Control_Type ) 
     return Boolean is 
      P : Window_Ptr := Get_Internals (Control, "Enabled");  
   begin
      return gtk.widget.Get_State(P.widget) /= gtk.enums.State_Insensitive;
   end Enabled;

   ----------------------------------------------------------------------------
   --
   --            T E X T   C O N T R O L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Get_Length: get the length of the text in a text control.
   --
   function Get_Length (
         Control : Text_Control_Type ) 
   return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Length");
      S : String := Get_Text(Control);
   begin
      return S'Length;
   end Get_Length;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text from a text control.
   --
   function Get_Text (
         Control : Text_Control_Type ) 
     return String is 
      P : Window_Ptr := Get_Internals (Control, "Get_Text");
   begin
      case P.Tipo is
         when Type_Button | Type_Action_Menuitem | Type_Menuitem | Type_Checkbox | Type_Radiobutton | Type_Check_Menuitem | Type_Radio_Menuitem =>
            return Locale_From_UTF8(Gtk.Label.Get_Text(Gtk_Label(P.Label_Widget)));
         when Type_Label =>
            return Locale_From_UTF8(Gtk.Label.Get_Text(Gtk_Label(P.Widget)));
         when Type_Editbox =>
            return Locale_From_UTF8(Gtk.gentry.Get_Text(Gtk_GEntry(P.Widget)));
         when others =>
            return "";
      end case;         
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text from a text control into a fixed-length
   --            string variable.
   --
   procedure Get_Text (
         Control : in     Text_Control_Type; 
         Text    :    out String;            
         Length  :    out Natural            ) is 
      S : constant String := Get_Text (Control);  
   begin
      if S'Length > Text'Length then
         Text := S(S'First..S'First+Text'Length-1);
         Length := Text'Length;
      else
         Text(Text'First..Text'First+S'Length-1) := S;
         Length := S'Length;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Set_Text: store the specified text in a text control.
   --
   procedure Set_Text (
         Control : in     Text_Control_Type; 
         Text    : in     String             ) is 
      P : Window_Ptr := Get_Internals (Control, "Set_Text");
   begin
      case P.Tipo is
         when Type_Button | Type_Action_Menuitem | Type_Menuitem | Type_Checkbox | Type_Radiobutton | Type_Check_Menuitem | Type_Radio_Menuitem =>
            Gtk.Label.Set_Text_With_Mnemonic(Gtk_Label(P.Label_Widget), Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
         when Type_Label =>
            Gtk.Label.Set_Text(Gtk_Label(P.Widget),Locale_To_UTF8(Text));
         when Type_Editbox =>
            Gtk.gentry.Set_Text(Gtk_GEntry(P.Widget),Locale_To_UTF8(Text));
         when others =>
            null;      
      end case;         
   end Set_Text;

   ----------------------------------------------------------------------------
   --
   --                 B U T T O N   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Button: create a button as specified.
   --
   function Button (
         Parent    : Container_Type'Class;               
         Origin    : Point_Type;                         
         Width     : Integer;                            
         Height    : Integer;                            
         Text      : in String;                             
         Command   : Command_Type;                       
         Default   : Boolean              := False;      
         Font      : Font_Type            := Parent_Font;
         imageFile : in String :="" ) 
     return Button_Type is 
      W : Button_Type;  
      P : Window_Ptr  := new Window_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Button")); 
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Button;
      --Create object button
      Gtk_New_With_Mnemonic(Gtk_Button(P.Widget), Locale_To_UTF8(Replace(Replace(UTF8_String(Text),'_','-'),'&','_')));
      P.Label_Widget:=Get_Child(Gtk_Bin(P.Widget));
      Set_Use_Underline (Gtk_Button(P.Widget), True);
      if not Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)+Can_Focus);
      end if;      
      --Add image if required
      declare
         Box : Gtk_Box;
         Imagen : Gtk_Image;
      begin
         if Trim(Imagefile,Ada.Strings.Both)'Length > 0 then
            Gtk.Image.Gtk_New(Imagen,imageFile);
            gtk.box.Gtk_New_Hbox(Box,false,0);
            Remove(Gtk_Container(Gtk_Bin(P.Widget)),P.Label_Widget);
            Add(gtk_button(P.Widget),Box);
            Pack_Start(Gtk_Box(Box), Imagen,true,true,0);
            Gtk.Label.Gtk_New_With_Mnemonic(Gtk_Label(P.Label_Widget), Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
            Pack_End(Gtk_Box(Box), Gtk_Label(P.Label_Widget),True,True,0);
         end if;
      end;
      -----------------------------
      --Asign handlers to events
      Widget_Cmd_Callback.Connect( P.Widget,"clicked",Widget_Cmd_Callback.To_Marshaller (Send_Event'access),To_SystemAddress(P));
      --Include button in parent fix container
      Put(C.Fix,Gtk_Button(P.Widget),Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,Command_Type'Pos(Command),"Button");
      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      

      Show_all(P.Widget);         
      if Default then
         Set_Flags(P.Widget,Can_Default+Can_Focus);
         Grab_Default(P.Widget);
         Focus(W);
      end if;

      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Button;

   ----------------------------------------------------------------------------
   --
   --                  L A B E L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Label: create a label as specified.
   --
   function Label (
         Parent : Container_Type'Class;               
         Origin : Point_Type;                         
         Width  : Integer;                            
         Height : Integer;                            
         Text   : String;                             
         Align  : Alignment_Type       := Left;       
         Font   : Font_Type            := Parent_Font ) 
     return Label_Type is 
      W   : Label_Type; 
      P : Window_Ptr  := new Window_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Label"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Label;
      --Create label object
      Gtk_New (Gtk_Label(P.Widget), Locale_To_UTF8(Text));
      Set_Line_Wrap (Gtk_Label(P.Widget), False);
      Set_Selectable (Gtk_Label(P.Widget), False);
      Set_Use_Markup (Gtk_Label(P.Widget), False);
      Set_Use_Underline (Gtk_Label(P.Widget), False);
      --Align text in label
      if Align = Right then
         Set_Justify (Gtk_Label(P.Widget), Justify_Right);
         Set_Alignment(Gtk_Misc(P.Widget),1.0,0.5);
      elsif Align = Centre then
         Set_Justify (Gtk_Label(P.Widget), Justify_Center);
         Set_Alignment(Gtk_Misc(P.Widget),0.5,0.5);
      else
         Set_Justify (Gtk_Label(P.Widget), Justify_Left);
         Set_Alignment(Gtk_Misc(P.Widget),0.0,0.5);
      end if;
      --Include in parent fix container
      Put(C.Fix,Gtk_Label(P.Widget),Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Label");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      

      P.Label_Widget:=Null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      Show(Gtk_Label(P.Widget));         
      return W;
   end Label;

   ----------------------------------------------------------------------------
   --
   --                E D I T B O X   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --Procedure to detect the change in editbox
   procedure Editbox_Change(Object : access Gtk_Editable_Record'Class;Edit : System.Address) is
      P : Window_Ptr := To_Window_Ptr (Edit);
   begin
      P.Changed:=True;
   end EditBox_Change;


   --
   --  Editbox: create an editbox as specified.
   --
   function Editbox (
         Parent   : Container_Type'Class;               
         Origin   : Point_Type;                         
         Width    : Integer;                            
         Height   : Integer;                            
         Text     : String               := "";         
         Password : Boolean              := False;      
         Font     : Font_Type            := Parent_Font ) 
     return Editbox_Type is 
      W : Editbox_Type;  
      P : Window_Ptr  := new Window_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Editbox"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Editbox;
      --Create editbox object
      Gtk_New (Gtk_Gentry(P.Widget));
      
      if not Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)+Can_Focus);
      end if;
      
      P.Changed:=false;
      Set_Editable(Gtk_Gentry(P.Widget), True);
      Set_Text(Gtk_Gentry(P.Widget), Locale_To_UTF8(Text));
      Set_Visibility(Gtk_Gentry(P.Widget), not Password);
      
      Put(C.Fix,Gtk_Gentry(P.Widget),Gint(Origin.X),Gint(Origin.Y));
         
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Editbox");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      
      --Handler to events
      Editbox_Changed_cb.Connect( Gtk_Editable(P.Widget),"changed",Editbox_Changed_Cb.To_Marshaller (Editbox_Change'Unrestricted_Access),To_SystemAddress(P));
      Show(Gtk_Gentry(P.Widget));  

      P.Label_Widget:=Null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Editbox;

   ----------------------------------------------------------------------------
   --
   --  Modified: test if the user has modified the editbox since the last
   --            time this function was called.
   --
   function Modified (
         Editbox : Editbox_Type ) 
      return Boolean is 
    P : Window_Ptr := Get_Internals(Editbox, "Modified");
    B : Boolean;
   begin
      B:=P.Changed;
      P.changed:=False;
      return B;
   end Modified;

   ----------------------------------------------------------------------------
   --
   --         B O O L E A N   C O N T R O L   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Get_State: test if a Boolean control is checked.
   --
   function Get_State (
         Control : Boolean_Control_Type ) 
     return Boolean is 
    P : Window_Ptr := Get_Internals (Control, "Get_State");
   begin
      case P.Tipo is
         when Type_Checkbox =>
            return Get_Active(Gtk_Toggle_Button(P.Widget));
         when Type_RadioButton =>
            return Get_Active(Gtk_Toggle_Button(P.Widget));
         when Type_Menuitem | Type_Check_Menuitem | Type_Radio_Menuitem  =>
            return Get_Active(Gtk_Check_Menu_Item(P.Widget));
         when others =>
            return false;
      end case;
   end Get_State;

   ----------------------------------------------------------------------------
   --
   --  Set_State: set the state of a Boolean control as specified.
   --
   procedure Set_State (
         Control : in     Boolean_Control_Type; 
         State   : in     Boolean               ) is  
      P : Window_Ptr := Get_Internals (Control, "Set_State");
   begin
      case P.Tipo is
         when Type_Checkbox =>
            Set_Active(Gtk_Toggle_Button(P.Widget),State);
         when Type_RadioButton =>
            Set_Active(Gtk_Toggle_Button(P.Widget),State);
         when Type_Menuitem | Type_Check_Menuitem | Type_Radio_Menuitem =>
            P.Changed:=True;
            Set_Active(Gtk_Check_Menu_Item(P.Widget),State);
            P.Changed:=False;
         when others =>
            null;
      end case;
   end Set_State;
   
   --Procedure to detect change in object state 
   procedure On_Change_State_Event(
     Widget        : access Gtk_Widget_Record'Class;
        Windowptr     : System.Address) is
     P : Window_Ptr := To_Window_Ptr(Windowptr);
   begin
     if not P.Changed then
        if P.Action >=0 then
           Window_Info.Set_Command(Natural(P.Action));
        end if;
     end if;
   end On_Change_State_Event;



   ----------------------------------------------------------------------------
   --
   --                M E N U I T E M   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --Function to capture the release event on Menuitem
     function On_Menuitem_Button_Release_Event(
     Widget        : access Gtk_Widget_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     WindowPtr     : System.Address)
           return Boolean is
        P : Window_Ptr := To_Window_Ptr(Windowptr);
        Parent : Window_Ptr;
        Main   : Main_Window_Ptr;
     begin
        Parent:=P;
        while Parent/=null and then Parent.Tipo /= Type_Frame loop
           Deselect(Gtk_Menu_Item(Parent.Widget));
           Parent:=Window_Ptr(Parent.Parent);
        end loop;
        Main:=Main_Window_Ptr(Parent);
        Deactivate(Gtk_Menu_Shell(Main.Menubar));
        if P.Action >= 0 then
           Window_Info.Set_Command(P.Action);
        end if;
        return true;
     end On_Menuitem_Button_Release_Event;
     
   ----------------------------------------------------------------------------
   --
   --  Menuitem: create a menuitem.
   --
   function Menuitem (
         Parent  : Menu_Type'Class; 
         Text    : String;          
         Command : Command_Type     ) 
     return Menuitem_Type is 
      M : Menuitem_Type;  
    P : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Menuitem"));
    W : Window_Ptr := new Window_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Menuitem;
      Gtk_New_With_Mnemonic(Gtk_Check_Menu_Item(W.Widget), Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);

      MenuItem_Return_Callback.Connect(W.Widget,"button_release_event",MenuItem_Return_Callback.To_Marshaller (On_Menuitem_Button_Release_Event'access),To_SystemAddress(W));
      Widget_Cmd_Callback.Connect(W.Widget,"activate",Widget_Cmd_Callback.To_Marshaller (On_Change_State_Event'access),To_SystemAddress(W));
      Append(Gtk_Menu_Shell(P.Menu), Gtk_Menu_Item(W.Widget));
      Show(W.Widget);

      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,Command_Type'Pos(Command),"Menuitem");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Menuitem;

   ----------------------------------------------------------------------------
   --
   --                A C T I O N _ M E N U I T E M   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Action_Menuitem: create a menuitem without mark, only sent event
   --
   function Action_Menuitem (
         Parent  : Menu_Type'Class; 
         Text    : String;          
         Command : Command_Type     ) 
     return Action_Menuitem_Type is 
      M : Action_Menuitem_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Action_Menuitem"));
      W : Window_Ptr := new Window_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Action_Menuitem;
      Gtk_New_With_Mnemonic(Gtk_Menu_Item(W.Widget),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);
      Widget_Cmd_Callback.Connect(W.Widget,"activate",Widget_Cmd_Callback.To_Marshaller (Send_Event'access),To_SystemAddress(W));
      Append(Gtk_Menu_Shell(C.Menu), Gtk_Menu_Item(W.Widget));
      Show(W.Widget);

      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,Command_Type'Pos(Command),"Action_Menuitem");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Action_Menuitem;

   ----------------------------------------------------------------------------
   --
   --                C H E C K _ M E N U I T E M   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Check_Menuitem: create a menuitem with check.
   --
  function Check_Menuitem (
         Parent  : Menu_Type'Class; 
         Text    : String;          
         Command : Command_Type;                             
         Checked : Boolean              := False) 
     return Check_Menuitem_Type is 
      M : Check_Menuitem_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Check_Menuitem"));
      W : Window_Ptr := new Window_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Check_Menuitem;
      Gtk_New_With_Mnemonic(Gtk_Check_Menu_Item(W.Widget),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);
      gtk.check_menu_item.Set_Active(Gtk_Check_Menu_Item(W.Widget),checked);
      Widget_Cmd_Callback.Connect(W.Widget,"activate",Widget_Cmd_Callback.To_Marshaller (On_Change_State_Event'access),To_SystemAddress(W));
      Append(Gtk_Menu_Shell(C.Menu), Gtk_Menu_Item(W.Widget));
      Show(W.Widget);

      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,Command_Type'Pos(Command),"Check_Menuitem");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Check_Menuitem;

   ----------------------------------------------------------------------------
   --
   --                R A D I O _ M E N U I T E M   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Radio_Menuitem: create a menuitem with radiogroup mark
   --
   function Radio_Menuitem (
         Parent  : Menu_Type'Class;               
         Text    : String;          
         Command : Command_Type;                             
         Checked : Boolean              := False) 
     return Radio_Menuitem_Type is 
      M : Radio_Menuitem_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Check_Menuitem"));
      W : Window_Ptr := new Window_Internals;
   begin
      M.Internals.Pointer := Reference_Counted_Ptr(W);
      W.Tipo:=Type_Radio_Menuitem;
      --Evaluate if is the first (create group) or if the nth of the group (add to group)
      if LastObject.Tipo/=Type_Radio_Menuitem then
         Gtk_New_With_Mnemonic(Gtk_Radio_Menu_Item(W.Widget),Widget_Slist.Null_List,Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      else
         Gtk_New_With_Mnemonic(Gtk_Radio_Menu_Item(W.Widget),Get_Group(Gtk_Radio_Menu_item(LastObject.Widget)),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
      end if;
      Set_Right_Justify (Gtk_Menu_Item(W.Widget), False);
      gtk.check_menu_item.Set_Active(Gtk_Check_Menu_Item(W.Widget),checked);
      Widget_Cmd_Callback.Connect(W.Widget,"activate",Widget_Cmd_Callback.To_Marshaller (On_Change_State_Event'access),To_SystemAddress(W));
      Append(Gtk_Menu_Shell(C.Menu), Gtk_Menu_Item(W.Widget));
      Show(W.Widget);

      W.Label_Widget:=Get_Child(Gtk_Bin(W.Widget));
      Fill_Child(M,Parent,(0,0),1,1,Parent_Font,Command_Type'Pos(Command),"Check_Menuitem");
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Radio_Menuitem;
   ----------------------------------------------------------------------------
   --
   --  Separator: create a separator for a menu.
   --
   function Separator (
         Parent : Menu_Type'Class ) 
     return Menuitem_Type is 
      M : Menuitem_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Separator"));
      W : Window_Ptr := new Window_Internals;
  begin
     M.Internals.Pointer := Reference_Counted_Ptr(W);
     W.Action:=-1;
      W.Tipo:=Type_Separator;
      Gtk_New(Gtk_Separator_Menu_Item(W.Widget));
      Add (C.Menu, W.Widget);
      Show(W.Widget);
      LastObject:=Window_Ptr(M.Internals.Pointer);
      return M;
   end Separator;

   ----------------------------------------------------------------------------
   --
   --               C H E C K B O X   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Checkbox: create a checkbox with the specified initial state.
   --
   function Checkbox (
         Parent  : Container_Type'Class;               
         Origin  : Point_Type;                         
         Width   : Integer;                            
         Height  : Integer;                            
         Text    : String;                             
         Checked : Boolean              := False;      
         Font    : Font_Type            := Parent_Font ) 
     return Checkbox_Type is 
      W : Checkbox_Type;  
    C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Checkbox"));
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Checkbox;
      Gtk_New_With_Mnemonic (Gtk_Check_Button(P.Widget),Locale_To_UTF8(replace(replace(Text,'_','-'),'&','_')));
      gtk.toggle_button.Set_Active(Gtk_toggle_button(P.Widget), checked);

      Put(C.Fix,Gtk_Check_Button(P.Widget),Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Checkbox");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      

     Show(Gtk_Check_Button(P.Widget));         
      P.Label_Widget:=Get_Child(Gtk_Bin(P.Widget));
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
      
   end Checkbox;

   ----------------------------------------------------------------------------
   --
   --            R A D I O B U T T O N   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Radiobutton: create a radiobutton with the specified initial state.
   --
   function Radiobutton (
         Parent  : Container_Type'Class;               
         Origin  : Point_Type;                         
         Width   : Integer;                            
         Height  : Integer;                            
         Text    : String;                             
         Checked : Boolean              := False;      
         Font    : Font_Type            := Parent_Font ) 
     return Radiobutton_Type is 
      W : Radiobutton_Type; 
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Radiobutton"));
      P : Window_Ptr       :=      new Window_Internals;
   begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
      
      if C.Tipo /= Type_Menu then
         P.Tipo:=Type_Radiobutton;
         --Evaluate if is the first (create group) or if the nth of the group (add to group)
         if LastObject.Tipo/=Type_RadioButton then
            Gtk_New_With_Mnemonic (Gtk_Radio_Button(P.Widget),Widget_Slist.Null_List,Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
         else
            Gtk_New_With_Mnemonic (Gtk_Radio_Button(P.Widget),Get_Group(Gtk_Radio_Button(LastObject.Widget)),Locale_To_UTF8(Replace(Replace(Text,'_','-'),'&','_')));
         end if;
         Set_Size_Request(P.Widget,Gint(Width),Gint(Height));

         gtk.toggle_button.Set_Active(Gtk_toggle_button(P.Widget), checked);
   
         Put(C.Fix,Gtk_Radio_Button(P.Widget),Gint(Origin.X),Gint(Origin.Y));
         Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Radiobutton");

         Set_Origin_Internals(P,C,Origin,True);
         Set_Size_Internals(P,C,Width,Height,True);      
    
         Show(Gtk_Check_Button(P.Widget));  
      end if;
      
      P.Label_Widget:=Get_Child(Gtk_Bin(P.Widget));
      LastObject:=Window_Ptr(W.Internals.Pointer);
      Set_State (W, Checked);
      return W;
   end Radiobutton;

   ----------------------------------------------------------------------------
   --
   --              M U L T I L I N E   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text of a specified line into a fixed-length
   --            string variable by dispatching to the appropriate
   --            Get_Text function.
   --
   procedure Get_Text (
         Control : in     Multiline_Type;      
         Line    : in     Natural        := 0; 
         Text    :    out String;              
         Length  :    out Natural              ) is 
      S : constant String := Get_Text (Multiline_Type'Class (Control), Line);  
   begin
      if S'Length > Text'Length then
         Text := S(S'First..S'First+Text'Length-1);
         Length := Text'Length;
      else
         Text(Text'First..Text'First+S'Length-1) := S;
         Length := S'Length;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Get_Actual_Line: convert a line number in a multiline control
   --                   (which may be zero or out-of-range) to an
   --                   absolute line number (internal use only).
   --
   function Get_Actual_Line (
         Control : in     Multiline_Type'Class; 
         Line    : in     Natural;              
         Name    : in     String                ) 
     return Natural is 
      L : Natural := Line;  
   begin
      if L > Get_Count(Control) then
         Raise_Exception (Constraint_Error'Identity,
            External_Tag(Control'Tag) &
            ": Line number out of range in " & Name);
      end if;
      if L = 0 then
         L := Get_Line(Control);
      end if;
      return L;
   end Get_Actual_Line;

   ----------------------------------------------------------------------------
   --
   --                L I S T B O X   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Listbox: create a listbox.
   --
   function Listbox (
         Parent : Container_Type'Class;               
         Origin : Point_Type;                         
         Width  : Integer;                            
         Height : Integer;                            
         Font   : Font_Type            := Parent_Font ) 
     return Listbox_Type is 
      W : Listbox_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals(Parent,"Listbox"));
      P : Window_Ptr   := new Window_Internals;
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Listbox;
      --Generate the scrolled window, that list go inside
      Gtk_New (Gtk_Scrolled_Window(P.Widget));
      Set_Border_Width (Gtk_Container(P.widget), Border_Width => 0);
      Set_Shadow_Type(Gtk_Scrolled_Window(P.Widget),Shadow_None);
      Set_Policy (Gtk_Scrolled_Window(P.Widget),
                  H_Scrollbar_Policy => Policy_Automatic,
                  V_Scrollbar_Policy => Policy_Automatic);
      --Create the list
      Gtk_New(Gtk_List(P.Label_Widget));
      Set_Selection_Mode(Gtk_List(P.Label_Widget),Selection_Single);
      --Add to the scrolled_window
      Add_With_Viewport (Gtk_Scrolled_Window(P.Widget), Gtk_List(P.Label_Widget));
      Set_Focus_Vadjustment (Gtk_List(P.Label_Widget), Get_Vadjustment (Gtk_Scrolled_Window(P.Widget)));
      Set_Focus_Hadjustment (Gtk_List(P.Label_Widget), Get_Hadjustment (Gtk_Scrolled_Window(P.Widget)));
      Put(C.Fix,P.widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Listbox");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      

      show(P.Label_Widget);
      show(P.Widget);

      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Listbox;

   ----------------------------------------------------------------------------
   --
   --  Get_Count: get the number of lines in the listbox.
   --
   function Get_Count (
         Control : Listbox_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Count");  
   begin
      return Natural(Length(Get_Children(Gtk_List(P.Label_Widget)))+1);
   end Get_Count;

   ----------------------------------------------------------------------------
   --
   --  Get_Line: get the number of the current line (0 if no line is
   --            selected).
   --
   function Get_Line (
         Control : Listbox_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Line");
      Sel : Widget_List.Glist;  
   begin
      Sel := Get_Selection(Gtk_List(P.Label_Widget));
      if Integer(Length(Sel)) = 0 then
         --Don't have selection
         return 0;
      end if;
      return Natural(Index(Get_Children(Gtk_List(P.Label_Widget)),Get_Data(Sel))+1);
   end Get_Line;

   ----------------------------------------------------------------------------
   --
   --  Get_Length: get the length of the specified line (0 if no line
   --              is selected).
   --
   function Get_Length (
         Control : Listbox_Type;     
         Line    : Natural      := 0 ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Length");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Length");
   begin
      if L > 0 then
         declare
            S : constant String := Get_Text(Control,L);
         begin
            return Natural(S'Length);
         end;
      end if;
      return 0;
   end Get_Length;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text of the specified line (the empty string if
   --            the current line is specified and no line is selected).
   --
   function Get_Text (
         Control : Listbox_Type;     
         Line    : Natural      := 0 ) 
     return String is 
      P : Window_Ptr := Get_Internals (Control, "Get_Text");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Text");  
   begin
      if L = 0 then
         return "";
      else
         L:=L-1;
         declare
            Item : Gtk_Widget:=Get_Data(Nth(Get_Children(Gtk_List(P.Label_Widget)),Guint(L))); 
            S : constant String := Get_Text(Gtk_Label(Get_Child(Gtk_Bin(Item))));
         begin
              return Locale_To_UTF8(S);
         end;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Set_Text: set the text of the specified line (delete the current
   --            line and insert its replacement).
   --
   procedure Set_Text (
         Control : in     Listbox_Type;     
         Text    : in     String;           
         Line    : in     Natural      := 0 ) is 
      L : Natural := Get_Actual_Line (Control, Line, "Set_Text");  
   begin
      Delete_Line (Control, L);
      Insert_Line (Control, Text, L);
   end Set_Text;

   ----------------------------------------------------------------------------
   --
   --  Select_Line: set the line number for the current selection (deselect
   --               all lines if the line number is 0).
   --
   procedure Select_Line (
         Control : in     Listbox_Type;     
         Line    : in     Natural      := 0 ) is 
      P : Window_Ptr := Get_Internals (Control, "Select_Line");
      LineSel : Natural;  
   begin
      if Line > Get_Count(Control) then
         Raise_Exception (Constraint_Error'Identity,
            External_Tag(Multiline_Type'Class(Control)'Tag) &
            ": Line number out of range in Select_Line");
      end if;
      if Line = 0 then
         LineSel:=Get_Line(Control);
         if LineSel /= 0 then
            Unselect_Item(Gtk_List(P.Label_Widget),Gint(LineSel));
         end if;
      else
         Select_Item(Gtk_List(P.Label_Widget),Gint(Line-1));
      end if;
   end Select_Line;

   ----------------------------------------------------------------------------
   --
   --  Append_Line: add a line containing the specified line to the end
   --               of the listbox.
   --
   procedure Append_Line (
         Control : in     Listbox_Type; 
         Text    : in     String        ) is 
      use Gtk.widget.Widget_List;
      P : Window_Ptr   := Get_Internals (Control, "Append_Line");   
      Item : Gtk_List_Item;
      List : Widget_List.Glist;
   begin
      Gtk_New(Gtk_List_Item(Item),Locale_To_UTF8(Text));
      Modify_Font(Get_Child(Gtk_Bin(Item)),From_String(Font2String(Get_Font(Control))));
      Show(Item);
      Widget_List.Append(List,Gtk_Widget(Item));
      Append_Items(Gtk_List(P.Label_Widget),List);
   end Append_Line;

   ----------------------------------------------------------------------------
   --
   --  Insert_Line: insert a new line above the specified line. If the real
   --               line number is zero (no current line), append the line
   --               as above.
   --
   procedure Insert_Line (
         Control : in     Listbox_Type;     
         Text    : in     String;           
         Line    : in     Natural      := 0 ) is 
      P : Window_Ptr   := Get_Internals (Control, "Insert_Line");  
      L : Natural      := Get_Actual_Line (Control, Line, "Insert_Line");  
      Item : Gtk_List_Item;
      List : Widget_List.GList;
   begin
      if L = 0 then
         Append_Line (Control, Text);
      else
         L:=L-1;
         Gtk_New(Gtk_List_Item(Item),Locale_To_UTF8(Text));
         Modify_Font(Get_Child(Gtk_Bin(Item)),From_String(Font2String(Get_Font(Control))));         
         Show(Item);
         Widget_List.Append(List,Gtk_Widget(Item));
         Insert_Items(Gtk_List(P.Label_Widget),List,Gint(L));
      end if;
   end Insert_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_Line: delete the specified line.
   --
   procedure Delete_Line (
         Control : in     Listbox_Type;     
         Line    : in     Natural      := 0 ) is 
      P : Window_Ptr := Get_Internals (Control, "Delete_Line");  
      L : Natural    := Get_Actual_Line (Control, Line, "Delete_Line");  
   begin
      --We avoided error if there is nothing no selected or line nonexisting
      if L /= 0 then
         L:=L-1;
         Clear_Items(Gtk_List(P.Label_Widget),Gint(L),Gint(L+1));
      end if;
   end Delete_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_All: delete all lines in the listbox.
   --
   procedure Delete_All (
         Control : in     Listbox_Type ) is 
      P : Window_Ptr := Get_Internals (Control, "Delete_All");  
      Len : Natural;
   begin
      Len := Get_Count(Control);
      --Don't eliminate an empty list
      if Len > 0 then
         Clear_Items(Gtk_List(P.Label_Widget),Gint(0),Gint(Len-1));
      end if;
   end Delete_All;

   ----------------------------------------------------------------------------
   --
   --               C O M B O B O X   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Combobox: create a combobox.
   --
   function Combobox (
         Parent   : Container_Type'Class;               
         Origin   : Point_Type;                         
         Width    : Integer;                            
         Editable : Boolean              := True;       
         Font     : Font_Type            := Parent_Font) 
     return Combobox_Type is 
      W : Combobox_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals(Parent,"Combobox"));
      P : Window_Ptr    := new Window_Internals;
      Height : Integer  := Combobox_Height_Constant;
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Combobox;
      Gtk_New(Gtk_Combo(P.Widget));
      Set_Editable(Get_Entry(Gtk_Combo(P.Widget)),Editable);
      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Combobox");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);      

      Show(P.Widget);

      P.Label_Widget:=Gtk_Widget(Get_Entry(Gtk_Combo(P.Widget)));
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Combobox;

   ----------------------------------------------------------------------------
   --
   --  Get_Count: get the number of lines in the combobox.
   --
   function Get_Count (
         Control : Combobox_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Count");  
   begin
      return Natural(Length(Get_Children(Get_List(Gtk_Combo(P.Widget))))+1);
   end Get_Count;

   ----------------------------------------------------------------------------
   --
   --  Get_Line: get the number of the current line (0 if no line is
   --            selected, or if the text in the editbox part of the
   --            control is not a string selected from the listbox
   --            part).
   --
   function Get_Line (
         Control : Combobox_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Line");  
      Sel : Widget_List.Glist;  
   begin
      Sel := Get_Selection(Get_List(Gtk_Combo(P.Widget)));
      if Integer(Length(Sel)) = 0 then
         --Don't have selection
         return 0;
      end if;
      return Natural(Index(Get_Children(Get_List(Gtk_Combo(P.Widget))),Get_Data(Sel))+1);
   end Get_Line;

   ----------------------------------------------------------------------------
   --
   --  Get_Length: get the length of the specified line (0 if no line
   --              is selected).
   --
   function Get_Length (
         Control : Combobox_Type;     
         Line    : Natural       := 0 ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Length");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Length");  
   begin
      
      if L > 0 then
         declare
            S : constant String := Get_Text(Control,L);
         begin
            return Natural(S'Length);
         end;
      end if; 
      return 0;  
   end Get_Length;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text of the specified line (the text of the editbox
   --            part of the control if the line number is 0).
   --
   function Get_Text (
         Control : Combobox_Type;     
         Line    : Natural       := 0 ) 
     return String is 
      P : Window_Ptr := Get_Internals (Control, "Get_Text");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Text");  
   begin
      if L = 0 then
         return Locale_To_UTF8(Get_Text(Get_Entry(Gtk_Combo(P.Widget))));
      else
         L:=L-1;
         declare
            Item : Gtk_Widget:=Get_Data(Nth(Get_Children(Get_List(Gtk_Combo(P.Widget))),Guint(L))); 
            S : constant String := Get_Text(Gtk_Label(Get_Child(Gtk_Bin(Item))));
         begin
              return Locale_To_UTF8(S);
         end;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Set_Text: set the text of the specified line (delete the current
   --            line and insert its replacement).
   --
   procedure Set_Text (
         Control : in     Combobox_Type;     
         Text    : in     String;            
         Line    : in     Natural       := 0 ) is 
      L : Natural := Get_Actual_Line (Control, Line, "Set_Text");  
   begin
      Delete_Line (Control, L);
      if L > Get_Count(Control) then
         L := 0;
      end if;
      Insert_Line (Control, Text, L);
   end Set_Text;

   ----------------------------------------------------------------------------
   --
   --  Select_Line: set the line number for the current selection (deselect
   --               all lines if the line number is 0).
   --
   procedure Select_Line (
         Control : in     Combobox_Type;     
         Line    : in     Natural       := 0 ) is 
      P : Window_Ptr := Get_Internals (Control, "Select_Line");  
      L : Natural    := Get_Actual_Line (Control, Line, "Select_Line");  
      LineSel : Natural;  
   begin
      if Line > Get_Count(Control) then
         Raise_Exception (Constraint_Error'Identity,
            External_Tag(Multiline_Type'Class(Control)'Tag) &
            ": Line number out of range in Select_Line");
      end if;
      if Line = 0 then
         LineSel:=Get_Line(Control);
         if LineSel /= 0 then
            Unselect_Item(Get_List(Gtk_Combo(P.Widget)),Gint(Linesel));
         end if;
         Set_Text(Get_Entry(Gtk_Combo(P.Widget)),"");
      else
         
         Select_Item(Get_List(Gtk_Combo(P.Widget)),Gint(Line-1));
         Set_Text(Get_Entry(Gtk_Combo(P.Widget)),Get_Text(Control,0));
      end if;
   end Select_Line;

   ----------------------------------------------------------------------------
   --
   --  Append_Line: add a line containing the specified line to the end
   --               of the listbox part of the combobox.
   --
   procedure Append_Line (
         Control : in     Combobox_Type; 
         Text    : in     String         ) is 
      P : Window_Ptr   := Get_Internals (Control, "Append_Line");   
      Item : Gtk_List_Item;
      List : Widget_List.Glist;
   begin
      Gtk_New(Gtk_List_Item(Item),Locale_To_UTF8(Text));
      Modify_Font(Get_Child(Gtk_Bin(Item)),From_String(Font2String(Get_Font(Control))));
      Show(Item);
      Widget_List.Append(List,Gtk_Widget(Item));
      Append_Items(Get_List(Gtk_Combo(P.Widget)),List);
   end Append_Line;

   ----------------------------------------------------------------------------
   --
   --  Insert_Line: insert a new line above the specified line. If the real
   --
   procedure Insert_Line (
         Control : in     Combobox_Type;     
         Text    : in     String;            
         Line    : in     Natural       := 0 ) is 
      P : Window_Ptr   := Get_Internals (Control, "Insert_Line");  
      L : Natural      := Get_Actual_Line (Control, Line, "Insert_Line");    
      Item : Gtk_List_Item;
      List : Widget_List.Glist;
   begin
      if L = 0 then
         Append_Line (Control, Text);
      else
         L:=L-1;
         Gtk_New(Gtk_List_Item(Item),Locale_To_UTF8(Text));
         Modify_Font(Get_Child(Gtk_Bin(Item)),From_String(Font2String(Get_Font(Control))));         
         Show(Item);
         Widget_List.Append(List,Gtk_Widget(Item));
         Insert_Items(Get_List(Gtk_Combo(P.Widget)),List,Gint(L));         
      end if;
   end Insert_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_Line: delete the specified line.
   --
   procedure Delete_Line (
         Control : in     Combobox_Type;     
         Line    : in     Natural       := 0 ) is 
      P : Window_Ptr := Get_Internals (Control, "Delete_Line");  
      L : Natural    := Get_Actual_Line (Control, Line, "Delete_Line");  
   begin
      if L = 0 then
         Select_Line (Control);
      else
         --We avoided error if there is nothing no selected or line nonexisting
         L:=L-1;
         Clear_Items(Get_List(Gtk_Combo(P.Widget)),Gint(L),Gint(L+1));
      end if;
   end Delete_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_All: delete all lines in the combobox.
   --
   procedure Delete_All (
         Control : in     Combobox_Type ) is 
      P : Window_Ptr := Get_Internals (Control, "Delete_All");  
      Len : Natural;
   begin
      Len := Get_Count(Control);
      --Don't eliminate an empty list
      if Len > 0 then
         Clear_Items(Get_List(Gtk_Combo(P.Widget)),Gint(0),Gint(Len-1));
      end if;      
   end Delete_All;

   ----------------------------------------------------------------------------
   --
   --                   M E M O   O P E R A T I O N S
   --
   --  Memos are slightly peculiar because Windows always reports them as
   --  having at least one line, even when they're completely empty. I've
   --  decided that a blank last line won't count as a line -- a CR/LF at
   --  the end of a line is part of the line it ends, and only lines with
   --  characters in them should count. So there.
   --
   ----------------------------------------------------------------------------
   --
   --  Last_Line: returns character index of start of last line (for internal
   --             use only).
   --
   function Last_Line (
         Memo : in     Memo_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Memo, "Last_Line");  
      L : Natural;  
      Iter : Gtk_Text_Iter;
      Buff : Gtk_Text_Buffer;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      Get_Iter_At_Line(Buff,Iter,Get_Line_Count(Buff)-1);
      L:=Natural(Gtk.Text_Iter.Get_Offset(Iter));
      return L;
   end Last_Line;

   ----------------------------------------------------------------------------
   --
   --  Length: returns length of memo text (for internal use only).
   --
   function Length (
         Memo : in     Memo_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Memo, "Length");  
      L : Natural;  
      Buff : Gtk_Text_Buffer;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      L:=Natural(Get_Char_Count(Buff));
      return L;
   end Length;


   procedure Textbuff_Event (Object : access Gtk_Text_Buffer_Record'Class; Param : System.Address) is
      P : Window_Ptr := To_Window_Ptr(Param);
      bufstart, bufend : Gtk_Text_Iter;
   begin
      gtk.text_buffer.Get_Start_Iter(Object,bufstart);
      gtk.text_buffer.Get_End_Iter(Object,bufend);
      Set_Text(P.Undo_Text,Get_Text(Object,Bufstart,Bufend));
   end TextBuff_Event;
   ----------------------------------------------------------------------------
   --
   --  Memo: create a memo control as specified.
   --
   function Memo (
         Parent : Container_Type'Class;               
         Origin : Point_Type;                         
         Width  : Integer;                            
         Height : Integer;                            
         Font   : Font_Type            := Parent_Font ) 
     return Memo_Type is 
      W : Memo_Type;  
      C : Container_Ptr := Container_Ptr(Get_Internals(Parent,"Memo"));
      P : Window_Ptr := new Window_Internals;
      Scrollwin:Gtk_Scrolled_Window;
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Memo;
      --Create Frame container
      Gtk_New (Gtk_Frame(P.Widget));
      Set_Border_Width (Gtk_Container(P.Widget), Border_Width => 0);
      Set_Shadow_Type(GTk_Frame(P.Widget),Shadow_In);
      --Create the text view
      Gtk_New (Gtk_Text_View(P.Label_Widget));
      Set_Text(Get_Buffer(Gtk_Text_View(P.Label_Widget)),"");
      --Create undo buffer
      Gtk_New(P.Undo_Text);
      Set_Text(P.Undo_Text,"");      
      --Make composite object
      Set_Wrap_Mode(Gtk_Text_View(P.Label_Widget), Wrap_None);
      Set_Editable(Gtk_Text_View(P.Label_Widget), True);
      Gtk.Text_View.Set_Cursor_Visible(Gtk_Text_View(P.Label_Widget),True);
      Set_Justification(Gtk_Text_View(P.Label_Widget), Justify_Left);
      Set_Border_Window_Size(Gtk_Text_View(P.Label_Widget), Text_Window_Left,Gint(0));
      Set_Border_Window_Size(Gtk_Text_View(P.Label_Widget), Text_Window_Right,Gint(0));
      Set_Border_Window_Size(Gtk_Text_View(P.Label_Widget), Text_Window_Top,Gint(0));
      Set_Border_Window_Size(Gtk_Text_View(P.Label_Widget), Text_Window_Bottom,Gint(0));
      show(P.Label_Widget);

      Gtk_New (ScrollWin);
      Set_Border_Width (Gtk_Container(ScrollWin), Border_Width => 2);
      Set_Shadow_Type(ScrollWin,Shadow_In);
      Set_Policy (ScrollWin, Policy_Always, Policy_Always);
      Add (Scrollwin, Gtk_Text_View(P.Label_Widget));
      --Include in parent fix container
      Add (Gtk_Frame(P.Widget), ScrollWin);
      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));

      if Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)-Can_Focus);
      end if;
      if not Can_Focus_Is_Set(P.Label_Widget) then
         Set_Flags(P.Label_Widget,Flags(P.Label_Widget)+Can_Focus);
      end if;
      if Can_Focus_Is_Set(Gtk_Widget(ScrollWin)) then
         Set_Flags(Gtk_Widget(Scrollwin),Flags(Gtk_Widget(Scrollwin))-Can_Focus);
      end if;
      Show(Scrollwin);
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Memo");

      Set_Origin_Internals(P,C,Origin,True);
      Set_Size_Internals(P,C,Width,Height,True);  
      
      --Handler to make UNDO
      TextBuf_Memo_Callback.Connect(Get_Buffer(Gtk_Text_View(P.Label_Widget)),"begin_user_action",TextBuf_Memo_Callback.To_Marshaller (Textbuff_Event'access),To_SystemAddress(P));

      Lastobject:=Window_Ptr(W.Internals.Pointer);
      
      show_all(P.Widget);

      return W;
   end Memo;

   ----------------------------------------------------------------------------
   --
   --  Get_Column: find the column number where the caret is positioned.
   --
   function Get_Column (
         Memo : Memo_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Memo, "Get_Column");  
      L : Natural;  
      Buff : Gtk_Text_Buffer;
      Iter : Gtk_Text_Iter;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      Get_Iter_At_Mark(Buff,Iter,Get_Insert(Buff));
      L:=Natural(Get_Line_Offset(Iter))+1;
      return L;
   end Get_Column;

   ----------------------------------------------------------------------------
   --
   --  Modified: test if the user has modified the memo since the last
   --            time this function was called.
   --
   function Modified (
         Memo : Memo_Type ) 
     return Boolean is 
      P : Window_Ptr := Get_Internals (Memo, "Modified");  
      Buff : Gtk_Text_Buffer;
      B : Boolean;  
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      B:=Get_Modified(Buff);
      Set_Modified(Buff,False);
      return B;
   end Modified;

   ----------------------------------------------------------------------------
   --
   --  Cut_Selection: cut the current selection to the clipboard.
   --
   procedure Cut_Selection (
         Memo : in     Memo_Type ) is 
      P : Window_Ptr := Get_Internals (Memo, "Cut_Selection");  
      Buff : Gtk_Text_Buffer;
  begin
     Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
     Begin_User_Action(Buff);
     Gtk.Text_Buffer.Cut_Clipboard(Buff,Get(Gdk_None),Get_Editable(Gtk_Text_View(P.Label_Widget)));
     End_user_action(Buff);
   end Cut_Selection;

   ----------------------------------------------------------------------------
   --
   --  Copy_Selection: copy the current selection to the clipboard.
   --
   procedure Copy_Selection (
         Memo : in     Memo_Type ) is 
      P : Window_Ptr := Get_Internals (Memo, "Copy_Selection");  
      Buff : Gtk_Text_Buffer;
  begin
     Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
     gtk.text_buffer.Copy_Clipboard(Buff,Get(Gdk_None));
   end Copy_Selection;

   ----------------------------------------------------------------------------
   --
   --  Paste_Selection: paste the clipboard over the current selection.
   --
   procedure Paste_Selection (
         Memo : in     Memo_Type ) is 
      P : Window_Ptr := Get_Internals (Memo, "Paste_Selection");  
      Buff : Gtk_Text_Buffer;
      IterStart : Gtk_Text_Iter;
      Iterend : Gtk_Text_Iter;
      Result : Boolean;
  begin
     Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
     Begin_User_Action(Buff);
     Gtk.Text_Buffer.Get_Selection_Bounds(Buff,Iterstart,Iterend,Result);
     if Result then
        --Delete previous selection
        gtk.text_buffer.Delete(Buff,iterstart,iterend);
     end if;
     --Insert to start
     Gtk.Text_Buffer.Paste_Clipboard(Buff,Gtk.Clipboard.Get(Gdk_None),null,Get_Editable(Gtk_Text_View(P.Label_Widget)));
     End_user_action(Buff);
   end Paste_Selection;

   ----------------------------------------------------------------------------
   --
   --  Undo_Change: undo the user's last change to the text of the memo.
   --
   procedure Undo_Change (
         Memo : in     Memo_Type ) is 
      P : Window_Ptr := Get_Internals (Memo, "Undo_Change");  
      bufstart, bufend : Gtk_Text_Iter;
   begin
      gtk.text_buffer.Get_Start_Iter(P.Undo_text,bufstart);
      gtk.text_buffer.Get_End_Iter(P.Undo_text,bufend);
      Set_Text(Get_Buffer(Gtk_Text_View(P.Label_Widget)),Get_Text(P.Undo_Text,bufstart,bufend));
   end Undo_Change;

   ----------------------------------------------------------------------------
   --
   --  Show_Selection: scroll the memo so that the caret is in view.
   --
   procedure Show_Selection (
         Memo : in     Memo_Type ) is 
      P : Window_Ptr := Get_Internals (Memo, "Show_Selection");  
      Buff : Gtk_Text_Buffer;
  begin
     Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
     Draw(P.Label_Widget);
     Scroll_Mark_Onscreen(Gtk_Text_View(P.Label_Widget),Get_Selection_Bound(Buff));
   end Show_Selection;

   ----------------------------------------------------------------------------
   --
   --  Get_Count: get the number of lines in the memo.
   --
   function Get_Count (
         Control : Memo_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Count");  
      Buff : Gtk_Text_Buffer;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      return Natural(Get_Line_Count(Buff) - Boolean'Pos(Last_Line(Control) = Length(Control)));
   end Get_Count;

   ----------------------------------------------------------------------------
   --
   --  Get_Line: get the number of the line where the caret is positioned.
   --            Return zero if it's on a blank last line.
   --
   function Get_Line (
         Control : Memo_Type ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Line");  
      Buff : Gtk_Text_Buffer;
      iter : Gtk_Text_Iter;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      if Last_Line(Control) = Length(Control) then
         return 0;
      else
         gtk.text_buffer.Get_Iter_At_Mark(buff,iter,Get_Insert(Buff));
         return Natural(Get_Line(iter)) + 1;
      end if;
   end Get_Line;

   ----------------------------------------------------------------------------
   --
   --  Get_Length: get the length of the specified line.
   --
   function Get_Length (
         Control : Memo_Type;     
         Line    : Natural   := 0 ) 
     return Natural is 
      P : Window_Ptr := Get_Internals (Control, "Get_Length");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Length");  
      Buff : Gtk_Text_Buffer;
      iterstart : Gtk_Text_Iter;
      Iterend : Gtk_Text_Iter;
      Len:Natural;
   begin
      if L = 0 then
         return 0;
      else
         Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
         Gtk.Text_Buffer.Get_Iter_At_Line(Buff,Iterstart,Gint(L-1));
         if L < Get_Count(Control) then
            Gtk.Text_Buffer.Get_Iter_At_Line(Buff,Iterend,Gint(L));
            Gtk.Text_Iter.Set_Offset(Iterend,Get_Offset(Iterend)-1);
         else
            Len:=Length(Control);
            Copy(Iterstart,Iterend);
            Gtk.Text_Iter.Set_Line_Offset(Iterend,Gint(Len)-Get_Offset(Iterstart));
         end if;
         return Natural(Get_Offset(Iterend)-Get_Offset(Iterstart));
      end if;
   end Get_Length;

   ----------------------------------------------------------------------------
   --
   --  Get_Text: get the text of the specified line. Note that the EM_GETLINE
   --            message takes the line length in the first two bytes of the
   --            destination string, and no terminating null is copied (so
   --            the rest of the destination string must be initialised to
   --            nulls).
   --
   function Get_Text (
         Control : Memo_Type;     
         Line    : Natural   := 0 ) 
     return String is 
      P : Window_Ptr := Get_Internals (Control, "Get_Text");  
      L : Natural    := Get_Actual_Line (Control, Line, "Get_Text");  
      W : Natural;  
      Buff : Gtk_Text_Buffer;
      iter : Gtk_Text_Iter;
      iterend : Gtk_Text_Iter;
   begin
      W := Get_Length (Control, L);
      if W = 0 then
         return "";
      else
         Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
         Get_Iter_At_Line(Buff,Iter,Gint(L-1));
         Copy(Iter,Iterend);
         Set_Visible_Line_Offset(iterend,Gint(W));
         declare
            S:String:=Get_Visible_Text(iter,iterend);
         begin
              return Locale_To_UTF8(S);
         end;
      end if;
   end Get_Text;

   ----------------------------------------------------------------------------
   --
   --  Set_Text: set the text of the specified line (select the line and
   --            replace the selection).
   --
   procedure Set_Text (
         Control : in     Memo_Type;     
         Text    : in     String;        
         Line    : in     Natural   := 0 ) is 
      P : Window_Ptr   := Get_Internals (Control, "Set_Text");  
      L : Natural      := Get_Actual_Line (Control, Line, "Set_Text");  
      Buff : Gtk_Text_Buffer;
      iter : Gtk_Text_Iter;
      iterend : Gtk_Text_Iter;
   begin
      if L = 0 then
         Append_Line (Control, Text);
      else
         Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
         Begin_user_action(Buff);
         Get_Iter_At_Line(Buff,Iter,Gint(L-1));
         Copy(Iter,Iterend);
         Set_Line_Offset(iterend,Gint(Get_Line_Count(Buff)));
         Set_Visible_Line_Offset(iterend,Gint(Get_Line_Count(Buff)));
         Gtk.Text_Buffer.Delete(Buff,Iter,Iterend);
         Gtk.Text_Buffer.Insert(Buff,Iter,Locale_To_UTF8(Text));
         End_user_action(Buff);
      end if;
   end Set_Text;

   ----------------------------------------------------------------------------
   --
   --  Select_Line: set the line number for the caret position.
   --
   procedure Select_Line (
         Control : in     Memo_Type;     
         Line    : in     Natural   := 0 ) is 
      P : Window_Ptr := Get_Internals (Control, "Select_Line");  
      L : Natural    := Get_Actual_Line (Control, Line, "Select_Line");    
      Buff : Gtk_Text_Buffer;
      iter : Gtk_Text_Iter;
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      if L = 0 then
         L := Length(Control);
         if L/=0 then
            L:=L-1;
            Gtk.Text_Iter.Set_Offset(Iter,Gint(L));
         else
            gtk.text_buffer.Get_Iter_At_Mark(buff,iter,Get_Insert(Buff));
         end if;
      else
         Get_Iter_At_Line(Buff,Iter,Gint(L-1));
      end if;
      Place_Cursor(Buff,iter);
   end Select_Line;

   ----------------------------------------------------------------------------
   --
   --  Append_Line: add a line containing the specified line to the end
   --               of the memo. If the last line is not blank, add a
   --               preceding EOL to start a new line
   --
   procedure Append_Line (
         Control : in     Memo_Type; 
         Text    : in     String     ) is 
      P : Window_Ptr := Get_Internals (Control, "Append_Line");    
      Buff : Gtk_Text_Buffer;
      Iter : Gtk_Text_Iter;
      L : Natural;
   begin
      L := Length(Control);
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      Begin_User_Action(Buff);
      Gtk.text_buffer.Get_Iter_At_Offset(Buff,Iter,Gint(L));
      if Last_Line(Control) = Length(Control) then
         Gtk.Text_Buffer.Insert(Buff,Iter,Locale_To_UTF8(Text));
      else
         Gtk.Text_Buffer.Insert(Buff,Iter,Locale_To_UTF8(Eol & Text));
      end if;
      End_user_action(Buff);
   end Append_Line;

   ----------------------------------------------------------------------------
   --
   --  Insert_Line: insert a new line above the specified line. If the line
   --               number is zero, append the line as above.
   --
   procedure Insert_Line (
         Control : in     Memo_Type;     
         Text    : in     String;        
         Line    : in     Natural   := 0 ) is 
      P : Window_Ptr   := Get_Internals (Control, "Insert_Line");  
      L : Natural      := Get_Actual_Line (Control, Line, "Select_Line");   
      Buff : Gtk_Text_Buffer;
      iter : Gtk_Text_Iter;
   begin
      if L = 0 then
         Append_Line (Control, Text);
      else
         Select_Line (Control, Line);
         Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
         Begin_User_Action(Buff);
         Get_Iter_At_Line(Buff,Iter,Gint(L-1));
         Gtk.Text_Buffer.Insert(Buff,Iter,Locale_To_Utf8(Text & Eol));
         End_user_action(Buff);
      end if;
   end Insert_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_Line: delete the specified line.
   --
   procedure Delete_Line (
         Control : in     Memo_Type;     
         Line    : in     Natural   := 0 ) is 
      P : Window_Ptr   := Get_Internals (Control, "Delete_Line");  
      L : Natural;  
      Buff : Gtk_Text_Buffer;
      iterstart : Gtk_Text_Iter;
      Iterend : Gtk_Text_Iter;
      Len:Natural; 
   begin
      L := Get_Actual_Line (Control, Line, "Delete_Line");
      if L > 0 then
         Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
         Begin_User_Action(Buff);
         Get_Iter_At_Line(Buff,Iterstart,Gint(L-1));
         if L < Get_Count(Control) then
            Gtk.Text_Buffer.Get_Iter_At_Line(Buff,Iterend,Gint(L));
            Gtk.Text_Iter.Set_Offset(Iterend,Get_Offset(Iterend));
         else
            Len:=Length(Control);
            Copy(Iterstart,Iterend);
            if Get_Count(Control) > 1 then
               Gtk.Text_Iter.Set_Offset(Iterstart,Get_Offset(Iterend)-1);
            end if;
            Gtk.Text_Iter.Set_Line_Offset(Iterend,Gint(Len)-Get_Offset(Iterstart)-2);
         end if;
         Delete(Buff,Iterstart,Iterend);
         End_user_action(Buff);
      end if;
   end Delete_Line;

   ----------------------------------------------------------------------------
   --
   --  Delete_All: delete all lines in the memo.
   --
   procedure Delete_All (
         Control : in     Memo_Type ) is 
      P : Window_Ptr   := Get_Internals (Control, "Delete_All");    
      Buff : Gtk_Text_Buffer; 
   begin
      Buff:=Get_Buffer(Gtk_Text_View(P.Label_Widget));
      Begin_user_action(Buff);
      Set_Text(Buff,"");
      End_user_action(Buff);
   end Delete_All;

   ----------------------------------------------------------------------------
   --
   --                 C A N V A S   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
--     type Monitor_Canvas_Ptr is access all GtkJewl.canvas_implementation.Canvas_Monitor;
        
     function To_System is new Ada.Unchecked_Conversion
        (Canvas_Ptr,System.Address);
     function To_Canvas_Ptr is new Ada.Unchecked_Conversion
        (System.Address,Canvas_Ptr);

     --Function that handles the keyboard event
     function On_Canvas_Key_Press_Event (
        Buffer        : access Gtk_Drawing_Area_Record'Class;
        Event         : Gdk.Event.Gdk_Event;
         CanvasPtr     : System.Address)
            return Boolean is
        C : Canvas_Ptr  := To_Canvas_Ptr(Canvasptr);
     begin
         if C.Keypress >= 0 then
            declare
               S : String :=Get_String(Event);
            begin   
               if S'Length = 1 then
                  C.Monitor.Set_Key (S(1));
                  Window_Info.Set_Command (C.Keypress);
               end if;
            end;
         end if;
         return True;   
      end On_Canvas_Key_Press_Event;
      
      --Function that handles the mouse press button (1) event
      function On_Canvas_Mouse_Press_Event (
        Buffer        : access Gtk_Drawing_Area_Record'Class;
         Event         : Gdk.Event.Gdk_Event;
         CanvasPtr     : System.Address)
            return Boolean is
         C : Canvas_Ptr  := To_Canvas_Ptr(CanvasPtr);
      begin
        if Get_Button(Event) = 1 then
         if C.Action >= 0 then
            C.Monitor.Set_Start (Integer(Gint(Get_X(Event))), Integer(Gint(Get_Y(Event))));
            C.Monitor.Set_Button (True);
            Window_Info.Set_Command (C.Action);
         end if;
        end if;
         return True;
      end On_Canvas_Mouse_Press_Event;
      --Function that handles the mouse release button (1) event
      function On_Canvas_Mouse_Release_Event (
        Buffer        : access Gtk_Drawing_Area_Record'Class;
         Event         : Gdk.Event.Gdk_Event;
         CanvasPtr     : System.Address)
            return Boolean is
         C : Canvas_Ptr  := To_Canvas_Ptr(CanvasPtr);
      begin
        if Get_Button(Event) = 1 then
         if C.Action >= 0 then
            C.Monitor.Set_End (Integer(Gint(Get_X(Event))), Integer(Gint(Get_Y(Event))));
            C.Monitor.Set_Button (False);
         end if;
        end if;
         return True;
      end On_Canvas_Mouse_Release_Event;
      --Function that handles the mouse motion with button (1) press event
      function On_Canvas_Mouse_Motion_Event (
        Buffer        : access Gtk_Drawing_Area_Record'Class;
         Event         : Gdk.Event.Gdk_Event;
         CanvasPtr     : System.Address)
            return Boolean is
         C : Canvas_Ptr  := To_Canvas_Ptr(Canvasptr);
         X,Y : Gint;
         State : Gdk_Modifier_Type;
         Result : gdk.window.Gdk_Window;
      begin
         if Get_Is_Hint(Event) then
            Get_Pointer(Get_Window(Buffer), X, Y, State, Result);
         else
            X:=Gint(Get_X(Event));
            Y:=Gint(Get_Y(Event));
            State:=Get_State(Event);
         end if;   
         if ((State and Button1_Mask) /= 0 ) then
            if C.Action >= 0  and C.Monitor.Mouse_Down then
               C.Monitor.Set_End (Integer(X), Integer(Y));
            end if;
         end if;
         return True;
      end On_Canvas_Mouse_Motion_Event;
      --Function that handles the configure event
      function On_Canvas_Configure_Event (
        Buffer        : access Gtk_Drawing_Area_Record'Class;
         Event         : Gdk.Event.Gdk_Event;
         CanvasPtr     : System.Address)
            return Boolean is
         C : Canvas_Ptr := To_Canvas_Ptr(CanvasPtr);
      begin
         C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
         return True;
      end On_Canvas_Configure_Event;
      
   ----------------------------------------------------------------------------
   --
   --  Canvas: create a canvas window which does not generate a command.
   --          No_List parameter avoid to create list of paint (true)
   --
   function Canvas (
         Parent : Container_Type'Class;               
         Origin : Point_Type;                         
         Width  : Integer;                            
         Height : Integer;                            
         Font   : Font_Type   := Parent_Font;
         No_List   : Boolean   := False  ) 
     return Canvas_Type is 
      W : Canvas_Type;  
      P : Canvas_Ptr  := new Canvas_Internals;
      C : Container_Ptr  := Container_Ptr(Get_Internals(Parent,"Canvas"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Canvas;
      Gtk_New(P.Buffer);
      P.Widget :=Gtk_Widget(P.Buffer);
      P.No_List:=No_List;

      Canvas_Return_CallBack.Connect(P.Buffer,"configure_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_configure_Event'Access),To_System(P));

      Set_Usize(P.Buffer,Gint(Width),Gint(Height));
      Put(C.Fix,Gtk_Widget(P.Buffer),Gint(Origin.X),Gint(Origin.Y));
      if (not Can_Focus_Is_Set(P.Buffer)) then
         Set_Flags(P.Buffer,Flags(P.Buffer) + Can_Focus);
      end if;
      Grab_Focus(P.Buffer);

      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Canvas");

      Set_Origin_Internals(Window_Ptr(P), C, Origin, True);
      Set_Size_Internals(Window_Ptr(P), C, Width, Height, True);      


      Set_Pen(W);
      Set_Fill(W);
      Set_Font(W,Font);

      Show(P.Buffer);      
           
      Set_Colour(W);
  
      P.Keypress:=-1;
      P.Label_Widget:=Null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Canvas;

   ----------------------------------------------------------------------------
   --
   --  Canvas: create a canvas window which generates a command when the
   --          mouse button is pressed within it.
   --          No_List parameter avoid to create list of paint (true)
   --
   function Canvas (
         Parent  : Container_Type'Class;               
         Origin  : Point_Type;                         
         Width   : Integer;                            
         Height  : Integer;                            
         Command : Command_Type;                       
         Font    : Font_Type     := Parent_Font;
         No_List   : Boolean      := False  ) 
     return Canvas_Type is 
      W : Canvas_Type;  
      P : Canvas_Ptr  :=  new Canvas_Internals;
      C : Container_Ptr  := Container_Ptr(Get_Internals(Parent,"Canvas"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Canvas;
      Gtk_New(P.Buffer);
      P.Widget :=Gtk_Widget(P.Buffer);
      P.No_List:=No_List;

      Canvas_Return_CallBack.Connect(P.Buffer,"configure_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_configure_Event'Access),To_System(P));

      Set_Usize(P.Buffer,Gint(Width),Gint(Height));
      Put(C.Fix,P.Buffer,Gint(Origin.X),Gint(Origin.Y));
      if (not Can_Focus_Is_Set(P.Buffer)) then
         Set_Flags(P.Buffer,Flags(P.Buffer) + Can_Focus);
      end if;
      Grab_Focus(P.Buffer);

      Fill_Child(W,Parent,Origin,Width,Height,Font,Command_Type'Pos(Command),"Canvas");
  
      Set_Origin_Internals(Window_Ptr(P), C, Origin, True);
      Set_Size_Internals(Window_Ptr(P), C, Width, Height, True);      

      Canvas_Return_CallBack.Connect(P.Buffer,"button_press_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Press_Event'Access),To_System(P));

      Canvas_Return_CallBack.Connect(P.Buffer,"button_release_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Release_Event'Access),To_System(P));

      Canvas_Return_CallBack.Connect(P.Buffer,"motion_notify_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Motion_Event'Access),To_System(P));

      Set_Pen(W,Black,1);
      Set_Fill(W);
      Set_Font(W,Font);
      
      Show(P.Buffer);  
           
      Set_Colour(W);

      P.Keypress:=-1;
      P.Label_Widget:=Null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Canvas;

   ----------------------------------------------------------------------------
   --
   --  Canvas: create a canvas window which generates a command when the
   --          mouse button or a key is pressed within it.
   --          No_List parameter avoid to create list of paint (true)
   --
   function Canvas (
         Parent   : Container_Type'Class;               
         Origin   : Point_Type;                         
         Width    : Integer;                            
         Height   : Integer;                            
         Command  : Command_Type;                       
         Keypress : Command_Type;                       
         Font     : Font_Type       := Parent_Font;
         No_List   : Boolean         := False ) 
     return Canvas_Type is 
      W : Canvas_Type;  
      P : Canvas_Ptr  := new Canvas_Internals;
      C : Container_Ptr  := Container_Ptr(Get_Internals(Parent,"Canvas"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Canvas;
      Gtk_New(P.Buffer);
      P.Widget :=Gtk_Widget(P.Buffer);
      P.No_List:=No_List;

      Canvas_Return_CallBack.Connect(P.Buffer,"configure_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_configure_Event'Access),To_System(P));

      Set_Usize(P.Buffer,Gint(Width),Gint(Height));
      Put(C.Fix,P.Buffer,Gint(Origin.X),Gint(Origin.Y));

      if (not Can_Focus_Is_Set(P.Buffer)) then
         Set_Flags(P.Buffer,Flags(P.Buffer) + Can_Focus);
      end if;
      Grab_Focus(P.Buffer);
      Fill_Child(W,Parent,Origin,Width,Height,Font,Command_Type'Pos(Command),"Canvas");
      
      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      
 
      Canvas_Return_CallBack.Connect(P.Buffer,"key_press_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Key_Press_Event'Access),To_System(P));

      Canvas_Return_CallBack.Connect(P.Buffer,"button_press_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Press_Event'Access),To_System(P));

      Canvas_Return_CallBack.Connect(P.Buffer,"button_release_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Release_Event'Access),To_System(P));

      Canvas_Return_CallBack.Connect(P.Buffer,"motion_notify_event",
                        Canvas_Return_CallBack.To_Marshaller (On_Canvas_Mouse_Motion_Event'Access),To_System(P));

      Set_Pen(W,Black,1);
      Set_Fill(W);
      Set_Font(W,Font);
      
      Show(P.Buffer); 
             
      Set_Colour(W);

      P.Keypress := Command_Type'Pos(Keypress);
      P.Label_Widget:=Null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      Focus (W);
      return W;
   end Canvas;

   ----------------------------------------------------------------------------
   --
   --  Set_Colour: ask the monitor to set the background colour.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Set_Colour (
         Canvas : in     Canvas_Type;         
         Colour : in     Colour_Type := White; 
         Paint  : in Boolean := True ) is   
      C : Canvas_Ptr     := Canvas_Ptr (Get_Internals (Canvas, "Set_Colour"));  
      Color : gdk.color.Gdk_Color:=To_GdkColor(Colour);
      style : Gtk_Style;
   begin
      ---Put background color in canvas
      Style:=Gtk.Style.Copy(Gtk.Widget.Get_Style(Gtk_Widget(C.Buffer)));
      gtk.style.Set_Foreground(style,State_Normal,color);
      gtk.style.Set_Background(style,State_Normal,color);
      Gtk.Widget.Set_Style(Gtk_Widget(C.Buffer),Style);
      C.Monitor.Set_Bg_Color (Colour);
      if Paint then
         C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
      end if;
   end Set_Colour;

   ----------------------------------------------------------------------------
   --
   --  Erase: ask the monitor to delete the drawing list and then redraw
   --         the window.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Erase (
         Canvas : in     Canvas_Type; 
         Paint    : in Boolean := True) is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Erase"));  
   begin
      C.Monitor.Clear;
      if Paint then
         C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
      end if;
   end Erase;

   ----------------------------------------------------------------------------
   --
   --  Save: ask the monitor to save the current position in the drawing list.
   --
   procedure Save (
         Canvas : in     Canvas_Type) is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Save"));  
   begin
      C.Monitor.Save;
   end Save;

   ----------------------------------------------------------------------------
   --
   --  Restore: revert to a previously saved position in the drawing list
   --           (ignored if there is no saved position). This is safe because
   --           the list always grows unless it is erased, so the saved
   --           position will be valid until Erase is called, at which point
   --           the monitor will reset it to null.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Restore (
         Canvas : in     Canvas_Type; 
         Paint    : in Boolean := True) is
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Restore"));  
   begin
      C.Monitor.Restore;
      if Paint then
         C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
      end if;
   end Restore;

   ----------------------------------------------------------------------------
   --
   --  ForceDraw: Fuerza el repintado total del canvas
   --
   procedure Force_Draw (
         Canvas   : in Canvas_Type) is
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "ForceDraw"));  
   begin
      C.Monitor.Draw(C.Buffer,C.Paintctx, C.No_List);
   end Force_Draw;

   ----------------------------------------------------------------------------
   --
   --  Set_Font: add a font handle to the drawing list.
   --
   procedure Set_Font (
         Canvas : in     Canvas_Type; 
         Font   : in     Font_Type    ) is 
      P : Canvas_Object_Ptr := new Handle_Type;
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Set_Font"));  
      CP: Container_Ptr;
      F : gdk.font.Gdk_Font;
   begin
      if Font.Name'Length = 0 then
         Cp:=C.Parent;
         while Cp /= null and then Cp.Font = null loop
            Cp:=Cp.Parent;
         end loop;
         if CP = null then
            F:=Gdk.Font.From_Description(From_String(Font2string(Default_Font)));
            C.Font:=Default_Font;
         else
            F:=Gdk.Font.From_Description(From_String(Font2string(CP.Font)));
            C.Font:=CP.Font;
         end if;            
      else
         F:=Gdk.Font.From_Description(From_String(Font2string(Font)));
         C.Font:=Font;
      end if;
      C.PaintCTX.Font:=F;
      Handle_Type(P.All).Font := F;
      Add (Canvas, "Set_Font", P,False);
   end Set_Font;

   ----------------------------------------------------------------------------
   --
   --  Set_Pen: add a pen handle to the drawing list.
   --
   procedure Set_Pen (
         Canvas : in     Canvas_Type;          
         Colour : in     Colour_Type := Black; 
         Width  : in     Natural     := 1      ) is 
      P : Canvas_Object_Ptr := new Handle_Type;
      C : Canvas_Ptr     := Canvas_Ptr (Get_Internals (Canvas, "Set_Pen"));  
      Color : Gdk.Color.Gdk_Color := To_Gdkcolor(Colour);
      GC : Gdk.GC.Gdk_GC;
   begin
      Gdk.GC.Gdk_New(GC,Get_Window(C.Buffer));
      if Width > 0 then
         C.PaintCTX.Pen:=True;
         Handle_Type(P.All).Pen :=True;
      else
         C.PaintCTX.Pen:=False;  
         Handle_Type(P.All).Pen :=False;
      end if;
      Set_Fill(GC,Solid);
      Set_Rgb_Fg_Color(Gc,Color);
      Set_Line_Attributes(GC,Gint(Width),Line_Solid,Cap_Not_Last,Join_Miter);
      Handle_Type(P.All).Pengc :=Gc;
      C.PaintCTX.PenGC:=GC;
         
      Add (Canvas, "Set_Pen", P,False);
   end Set_Pen;

   ----------------------------------------------------------------------------
   --
   --  Set_Fill: add a solid brush handle to the drawing list.
   --
   procedure Set_Fill (
         Canvas : in     Canvas_Type; 
         Colour : in     Colour_Type  ) is 
      P : Canvas_Object_Ptr := new Handle_Type;
      C : Canvas_Ptr     := Canvas_Ptr (Get_Internals (Canvas, "Set_Fill"));  
      Color : Gdk.Color.Gdk_Color := To_Gdkcolor(Colour);
      GC : Gdk.GC.Gdk_GC;
   begin
      Gdk.Gc.Gdk_New(Gc,Get_Window(C.Buffer));
      C.PaintCTX.Fill:=True;
      Handle_Type(P.All).Fill :=True;
      Set_Fill(GC,Solid);
      Set_Rgb_Fg_Color(Gc,Color);
      Set_Line_Attributes(GC,0,Line_Solid,Cap_Not_Last,Join_Miter);
      Handle_Type(P.All).FillGC :=GC;
      C.PaintCTX.FillGC:=GC;
      
      Add (Canvas, "Set_Fill", P,False);
   end Set_Fill;

   ----------------------------------------------------------------------------
   --
   --  Set_Fill: add a transparent brush handle to the drawing list.
   --
   procedure Set_Fill (
         Canvas : in     Canvas_Type ) is 
      C : Canvas_Ptr     := Canvas_Ptr (Get_Internals (Canvas, "Set_Fill"));  
      P : Canvas_Object_Ptr := new Handle_Type;
      Gc : Gdk.Gc.Gdk_Gc;
   begin
      Gdk.Gc.Gdk_New(Gc,Get_Window(C.Buffer));
      C.PaintCTX.Fill:=False;
      Handle_Type(P.All).Fill :=False;
      Handle_Type(P.All).Fillgc :=Gc;
      Set_Fill(Gc,Solid);
      Set_Line_Attributes(GC,0,Line_Solid,Cap_Not_Last,Join_Miter);
      C.PaintCTX.FillGC:=GC;

      Add (Canvas, "Set_Fill", P,False);
   end Set_Fill;


   ----------------------------------------------------------------------------
   --
   --  Calcula_Cuadro: Make P1 the Left-Top corner and P2 the Right-Bottom corner
   --             
   procedure Calcula_Cuadro(P1,P2:in out Point_Type) is
      Paux : Point_Type;
   begin
      if P1.X > P2.X then
          Paux.X:=P1.X;         
          P1.X:=P2.X;
          P2.X:=Paux.X;
      end if;
      if P1.Y > P2.Y then
         Paux.Y:=P1.Y;
         P1.Y:=P2.Y;
         P2.Y:=Paux.Y;
      end if;
   end Calcula_Cuadro;

   ----------------------------------------------------------------------------
   --
   --  Draw_Text: add a text string to the drawing with the top left
   --             corner at the specified point.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Text (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         Text   : in     String;       
         Paint  : in     Boolean := True) is 
      P : Canvas_Object_Ptr := new Text_Type (Locale_To_UTF8(Text)'Length);
   begin
      Text_Type(P.All).Text  := Locale_To_UTF8(Text);
      Text_Type(P.All).From  := From;
      Text_Type(P.All).To    := From;
      Text_Type(P.All).Align := -1;
      Add (Canvas, "Draw_Text", P, Paint);
   end Draw_Text;

   ----------------------------------------------------------------------------
   --
   --  Draw_Text: add a text string to the drawing within a rectangle
   --             specified by diagonally opposite corners.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Text (
         Canvas : in     Canvas_Type;           
         From   : in     Point_Type;            
         To     : in     Point_Type;            
         Text   : in     String;                
         Align  : in     Alignment_Type := Left; 
         Paint  : in     Boolean := True) is 
      P : Canvas_Object_Ptr := new Text_Type (Locale_To_UTF8(Text)'Length);
   begin
      Text_Type(P.All).Text  := Locale_To_UTF8(Text);
      Text_Type(P.All).From  := From;
      Text_Type(P.All).To    := To;
      Text_Type(P.All).Align := Alignment_Type'Pos(Align);
      Calcula_Cuadro(Text_Type(P.All).From, Text_Type(P.All).To);

      Add (Canvas, "Draw_Text", P, Paint);
   end Draw_Text;

   ----------------------------------------------------------------------------
   --
   --  Draw_Text: calculate the text rectangle from a height and width.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Text (
         Canvas : in     Canvas_Type;           
         From   : in     Point_Type;            
         Width  : in     Integer;               
         Height : in     Integer;               
         Text   : in     String;                
         Align  : in     Alignment_Type := Left;
         Paint  : in     Boolean := True) is 
   begin
      Draw_Text (Canvas, From, (From.X+Width,From.Y+Height), Text, Align, Paint);
   end Draw_Text;

   ----------------------------------------------------------------------------
   --
   --  Draw_Line: add a line to the drawing between two points.
    --           Paint forces or not to canvas to repaint it
  --
   procedure Draw_Line (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         To     : in     Point_Type;
         Paint  : in     Boolean := True) is 
      P : Canvas_Object_Ptr := new Line_Type;
   begin
      Line_Type(P.All).From := From;
      Line_Type(P.All).To   := To;

      Add (Canvas, "Draw_Line", P, Paint);
   end Draw_Line;

   ----------------------------------------------------------------------------
   --
   --  Draw_Line: calculate the line endpoint from a length and angle.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Line (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         Length : in     Positive;    
         Angle  : in     Angle_Type;    
         Paint  : in     Boolean := True) is 
      To : Point_Type;  
   begin
      To := Endpoint(From,Length,Angle);
      Draw_Line (Canvas, From, To, Paint);
   end Draw_Line;

   ----------------------------------------------------------------------------
   --
   --  Draw_Line_List: add a polyline to the drawing. Ignore polylines with
   --                  less than two points, and draw an ordinary line for a
   --                  polyline with only two points.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Line_List (
         Canvas : in     Canvas_Type; 
         Points : in     Point_List; 
         Paint  : in     Boolean := True) is 
      P : Window_Ptr := Get_Internals (Canvas, "Draw_Line_List");  
   begin
      if Points'Length = 2 then
         Draw_Line (Canvas, Points(Points'First), Points(Points'Last), Paint);
      elsif Points'Length > 2 then
         declare
            P : Canvas_Object_Ptr :=  new Polyline_Type(Points'Length);
         begin
            P.Next := null;
            for I in 1..Points'Length loop
               Polyline_Type(P.All).Points(I) :=(Gint(Points(Points'First+I-1).X), Gint(Points(Points'First+I-1).Y));            
            end loop;
            Add (Canvas, "Draw_Line_List", P, Paint);
         end;
      end if;
   end Draw_Line_List;

   ----------------------------------------------------------------------------
   --
   --  Draw_Rectangle: add either a normal rectangle or a rounded rectangle
   --                  to the drawing, depending on whether the rounding is
   --                  zero or not.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Rectangle (
         Canvas   : in     Canvas_Type;          
         From     : in     Point_Type;           
         To       : in     Point_Type;           
         Rounding : in     Point_Type  := (0, 0); 
         Paint  : in     Boolean := True) is 
      P : Canvas_Object_Ptr;  
   begin
      if Rounding = (0,0) then
         P := new Rectangle_Type;
         Rectangle_Type(P.All).From := From;
         Rectangle_Type(P.All).To   := To;
         Calcula_Cuadro(Rectangle_Type(P.All).From, Rectangle_Type(P.All).To);
      else
         P := new Rounded_Rectangle_Type;
         Rounded_Rectangle_Type(P.All).From   := From;
         Rounded_Rectangle_Type(P.All).To     := To;
         Rounded_Rectangle_Type(P.All).Corner := Rounding;
         Calcula_Cuadro(Rounded_Rectangle_Type(P.All).From, Rounded_Rectangle_Type(P.All).To);
      end if;

      Add (Canvas, "Draw_Rectangle", P, Paint);
   end Draw_Rectangle;

   ----------------------------------------------------------------------------
   --
   --  Draw_Rectangle: calculate the rectangle size from a height and width.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Rectangle (
         Canvas   : in     Canvas_Type;          
         From     : in     Point_Type;           
         Width    : in     Positive;             
         Height   : in     Positive;             
         Rounding : in     Point_Type  := (0, 0);
         Paint  : in     Boolean := True) is     
   begin
      Draw_Rectangle (Canvas, From, (From.X+Width, From.Y+Height), Rounding, Paint);
   end Draw_Rectangle;

   ----------------------------------------------------------------------------
   --
   --  Draw_Ellipse: draw an ellipse whose size is specified by a bounding
   --                rectangle.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Ellipse (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         To     : in     Point_Type; 
         Paint  : in     Boolean := True) is 
      P : Canvas_Object_Ptr :=  new Ellipse_Type;
   begin
      Ellipse_Type(P.All).From := From;
      Ellipse_Type(P.All).To   := To;
      Calcula_Cuadro(Ellipse_Type(P.All).From, Ellipse_Type(P.All).To);
      
      Add (Canvas, "Draw_Ellipse", P, Paint);
   end Draw_Ellipse;

   ----------------------------------------------------------------------------
   --
   --  Draw_Ellipse: calculate the bounding rectangle from a height and width.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Ellipse (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         Width  : in     Positive;    
         Height : in     Positive;
         Paint  : in     Boolean := True) is        
   begin
      Draw_Ellipse (Canvas, From, (From.X + Width, From.Y + Height), Paint);
   end Draw_Ellipse;

   ----------------------------------------------------------------------------
   --
   --  Draw_Circle: draw an ellipse in a bounding square calculated from
   --               the centre point and the radius.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Circle (
         Canvas : in     Canvas_Type; 
         Centre : in     Point_Type;  
         Radius : in     Positive;
         Paint  : in     Boolean := True) is 
      P : Window_Ptr := Get_Internals (Canvas, "Draw_Circle");  
   begin
      Draw_Ellipse (Canvas, (Centre.X - Radius,Centre.Y - Radius),
         (Centre.X + Radius,Centre.Y + Radius), Paint);
   end Draw_Circle;

   ----------------------------------------------------------------------------
   --
   --  Draw_Polygon: create and fill a Windows-style array with the coordinates
   --                of the vertices, then add the polygon to the drawing list.
   --                Draw a line if there are only two vertices, and do nothing
   --                if there are less than two vertices.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Polygon (
         Canvas : in     Canvas_Type; 
         Points : in     Point_List; 
         Paint  : in     Boolean := True) is 
      P : Window_Ptr := Get_Internals (Canvas, "Draw_Polygon");  
   begin
      if Points'Length = 2 then
         Draw_Line (Canvas, Points(Points'First), Points(Points'Last), Paint);
      elsif Points'Length > 2 then
         declare
            P : Canvas_Object_Ptr :=  new Polygon_Type(Points'Length);
         begin 
            P.Next := null;
            for I in 1..Points'Length loop
               Polygon_Type(P.All).Points(I) :=(Gint(Points(Points'First+I-1).X), Gint(Points(Points'First+I-1).Y));
            end loop;
            Add (Canvas, "Draw_Polygon", P, Paint);
         end;
      end if;
   end Draw_Polygon;

   ----------------------------------------------------------------------------
   --
   --  Draw_Image: draw the specified image on the canvas starting at the
   --              specified top-left corner point.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Image (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         Image  : in     Image_Type; 
         Paint  : in     Boolean := True) is 
      I : Image_Ptr;  
   begin
      if Valid(Image) then
         I := Image_Ptr(Image.Internals.Pointer);
         Draw_Image (Canvas, From, I.Width, I.Height, Image, Paint);
      end if;
   end Draw_Image;

   ----------------------------------------------------------------------------
   --
   --  Draw_Image: draw the specified image on the canvas starting at the
   --              specified top-left corner point, stretching it to the
   --              specified bottom-right corner point.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Image (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         To     : in     Point_Type;  
         Image  : in     Image_Type;    
         Paint  : in     Boolean := True) is 
      L : Integer := Integer'Min (From.X, To.X);  
      T : Integer := Integer'Min (From.Y, To.Y);  
      R : Integer := Integer'Max (From.X, To.X);  
      B : Integer := Integer'Max (From.Y, To.Y);  
   begin
      Draw_Image (Canvas, (L,T), R-L, B-T, Image, Paint);
   end Draw_Image;

   ----------------------------------------------------------------------------
   --
   --  Draw_Image: draw the specified image on the canvas starting at the
   --              specified top-left corner point, stretching it to the
   --              specified width and height.
   --           Paint forces or not to canvas to repaint it
   --
   procedure Draw_Image (
         Canvas : in     Canvas_Type; 
         From   : in     Point_Type;  
         Width  : in     Natural;     
         Height : in     Natural;     
         Image  : in     Image_Type;  
         Paint  : in     Boolean := True) is 
      P : Window_Ptr        := Get_Internals (Canvas, "Draw_Image");  
      B : Canvas_Object_Ptr :=  new Bitmap_Type;
      I : Image_Ptr;  
   begin
      if Valid(Image) then
         I := Image_Ptr(Image.Internals.Pointer);
         Bitmap_Type(B.All).From   := From;
         Bitmap_Type(B.All).Width  := Width;
         Bitmap_Type(B.All).Height := Height;
         Bitmap_Type(B.All).Bitmap := Image.Internals;
         Add (Canvas, "Draw_Image", B, Paint);
      end if;
   end Draw_Image;

   ----------------------------------------------------------------------------
   --
   --  Mouse_Down: test whether the mouse was pressed within a specific
   --              canvas.
   --
   function Mouse_Down (
         Canvas : Canvas_Type ) 
     return Boolean is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Mouse_Down"));  
   begin
      Do_events;
      return C.Monitor.Mouse_Down;
   end Mouse_Down;

   ----------------------------------------------------------------------------
   --
   --  Mouse_Moved: test if the mouse has moved within a canvas, which
   --               will only be true while the mouse is down after being
   --               pressed inside this canvas.
   --
   function Mouse_Moved (
         Canvas : Canvas_Type ) 
     return Boolean is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Mouse_Moved"));  
   begin
      Do_Events;
      return C.Monitor.Mouse_Moved;
   end Mouse_Moved;

   ----------------------------------------------------------------------------
   --
   --  Start_Point: get the position where the mouse was pressed within the
   --               specified canvas.
   --
   function Start_Point (
         Canvas : Canvas_Type ) 
     return Point_Type is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Start_Point"));  
      P : Point_Type;   
   begin
      Do_events;
      C.Monitor.Get_Start (P.X, P.Y);
      return P;
   end Start_Point;

   ----------------------------------------------------------------------------
   --
   --  End_Point: get the latest mouse position within the specified canvas.
   --
   function End_Point (
         Canvas : Canvas_Type ) 
     return Point_Type is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "End_Point"));  
      P : Point_Type;  
   begin
      Do_events;
      C.Monitor.Get_End (P.X, P.Y);
      return P;
   end End_Point;

   ----------------------------------------------------------------------------
   --
   --  Key_Code: get the latest key position within the specified canvas.
   --
   function Key_Code (
         Canvas : Canvas_Type ) 
     return Character is 
      C : Canvas_Ptr := Canvas_Ptr (Get_Internals (Canvas, "Key_Code"));  
      K : Character;   
   begin
      Do_events;
      C.Monitor.Get_Key (K);
      return K;
   end Key_Code;

   ----------------------------------------------------------------------------
   --
   --           C O M M O N   D I A L O G   O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --Procedure of callback for when we pushed a button of the Common_Dialog (OK, Cancel)
   procedure Dialog_Button_Event (Object : access Gtk_Button_Record'Class;OK_PRESS : Boolean) is
   begin
      Window_Info.Set_Dialog_Finish_State(OK_PRESS);
   end Dialog_Button_Event;

   --Procedure of callback for when we closed the Common_Dialog [X]
   function Dialog_Delete_Event (Object : access Gtk_Widget_Record'Class;Ok_Press: Boolean) 
         return Boolean is
   begin
      Window_Info.Set_Dialog_Finish_State(OK_PRESS);
      return True;  
   end Dialog_Delete_Event;

   
   ----------------------------------------------------------------------------
   --
   --  Execute: execute a common dialog by asking the message loop to call
   --           its Show_Dialog primitive and return the result.
   --
   function Execute (
         Dialog : Common_Dialog_Type ) 
     return Boolean is 
    P      : Common_Dialog_Ptr := Get_Internals (Dialog, "Execute"); 
    Result : Boolean;
    D      : Gtk_Window;
    Error  : Integer;
   begin
      D:=Window_Info.Active_Window;
      if D /= null then
         Gtk.Window.Set_Transient_For(Gtk_Window(P.Widget),D);
      end if;
      -- Make the window visible and bring it to the foreground
      Show(P.Widget);

      -- Wait for a command (which must be from this dialog, as dialog
      -- windows disable all other windows belonging to the application)
      loop
         Show_Dialog(P,Error,Result);
         exit when Error=0;
         case Error is
            when 1 =>
                 Show_Warning("No se encuentra el archivo" &
                    Ascii.Cr & "Asegurese de haber dado el nombre de archivo correcto");
            when 2 =>   
               Show_Warning("Path del archivo demasiado largo");
            when 3 =>   
               if Show_Query("Fichero no existe" & Ascii.Cr & "  � Desea crearlo ?") then
                  Error:=0;
               end if;
            when 4 =>   
               if Show_Query("Fichero ya existe" & Ascii.Cr & "  � Desea reemplazarlo ?") then
                  Error:=0;
               end if;
            when others =>
               Show_Warning("Error indefinido");
         end case;
         exit when Error=0;
      end loop;

      -- Hide the dialog window and return the command code
      Hide(P.Widget);
      
      return Result;
   end Execute;

   ----------------------------------------------------------------------------
   --
   --  Colour_Dialog: construct a colour dialog.
   --
   function Colour_Dialog return Colour_Dialog_Type is 
      D : Colour_Dialog_Type;  
      P : Colour_Dialog_Ptr  := new Colour_Dialog_Internals;
   begin
      D.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Colour_Dialog;
      P.Colour:=Default_Color;
      Gtk.Color_Selection_Dialog.Gtk_New(Gtk_Color_Selection_Dialog(P.Widget),"Color" );
      Gtk.Window.Set_Modal(Gtk_Window(P.Widget),True);
      Gtk.Window.Set_Resizable(Gtk_Window(P.Widget),False);
      gtk.window.Set_Policy(Gtk_Window(P.Widget),false,false,false);    

      Dialog_Bool_Return_Callback.Connect( P.Widget,"delete_event",Dialog_Bool_Return_Callback.To_Marshaller (Dialog_Delete_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Ok_Button(Gtk_Color_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),True);
      Dialog_Sel_Cb.Connect(Get_Cancel_Button(Gtk_Color_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Help_Button(Gtk_Color_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);

      return D;
   end Colour_Dialog;

   ----------------------------------------------------------------------------
   --
   --  Set_Colour: set the colour stored in a colour dialog.
   --
   procedure Set_Colour (
         Dialog : in     Colour_Dialog_Type; 
         Colour : in     Colour_Type         ) is 
      P : Colour_Dialog_Ptr := Colour_Dialog_Ptr (Get_Internals (Dialog, "Set_Colour")); 
   begin
      P.Colour := Colour;
   end Set_Colour;

   ----------------------------------------------------------------------------
   --
   --  Get_Colour: get the colour stored in a colour dialog.
   --
   function Get_Colour (
         Dialog : in     Colour_Dialog_Type ) 
         return Colour_Type is 
      P : Colour_Dialog_Ptr := Colour_Dialog_Ptr (Get_Internals (Dialog, "Get_Colour"));  

   begin   
      return P.Colour;
   end Get_Colour;

   ----------------------------------------------------------------------------
   --
   --  Font_Dialog: construct a font dialog.
   --
   function Font_Dialog return Font_Dialog_Type is 
      D : Font_Dialog_Type;  
      B : Boolean;
      P : Font_Dialog_Ptr  := new Font_Dialog_Internals;
   begin
      D.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Font_Dialog;
      P.Font:=Default_Font;

      Gtk.Font_Selection.Gtk_New(Gtk_Font_Selection_Dialog(P.Widget),"Font" );
      Gtk.Window.Set_Modal(Gtk_Window(P.Widget),True);
      Gtk.Window.Set_Resizable(Gtk_Window(P.Widget),False);
      gtk.window.Set_Policy(Gtk_Window(P.Widget),false,false,false);
      B:=gtk.font_selection.Set_Font_Name(Gtk_Font_Selection_Dialog(P.Widget),Font2String(P.Font));

      Dialog_Bool_Return_Callback.Connect( P.Widget,"delete_event",Dialog_Bool_Return_Callback.To_Marshaller (Dialog_Delete_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Ok_Button(Gtk_Font_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),True);
      Dialog_Sel_Cb.Connect(Get_Cancel_Button(Gtk_Font_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Apply_Button(Gtk_Font_Selection_Dialog(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);

      return D;
   end Font_Dialog;

   ----------------------------------------------------------------------------
   --
   --  Set_Font: set the font stored in a font dialog.
   --
   procedure Set_Font (
         Dialog : in     Font_Dialog_Type; 
         Font   : in     Font_Type         ) is 
      P : Font_Dialog_Ptr := Font_Dialog_Ptr (Get_Internals (Dialog, "Set_Font"));   
   begin
      P.Font:=Font;
   end Set_Font;

   ----------------------------------------------------------------------------
   --
   --  Get_Font: get the font stored in a font dialog.
   --
   function Get_Font (
         Dialog : in     Font_Dialog_Type ) 
     return Font_Type is 
      P : Font_Dialog_Ptr := Font_Dialog_Ptr (Get_Internals (Dialog, "Get_Font"));  
    begin
      return P.Font;
   end Get_Font;


   ----------------------------------------------------------------------------
    
   ----------------------------------------------------------------------------
   --
   --  Set_Name: set the filename stored in a file dialog.
   --
   procedure Set_Name (
         Dialog : in     File_Dialog_Type; 
         Name   : in     String            ) is 
      P : File_Dialog_Ptr := File_Dialog_Ptr (Get_Internals (Dialog, "Set_Name"));  
   begin
      if Name'Length=0 then
         P.Buffer (P.Buffer'First .. P.Buffer'Last) :=(Others => ' ');       
         return;
      end if;
      if P.Buffer'Length > Name'Length then
         P.Buffer(P.Buffer'First..P.Buffer'First+Name'Length-1) := Name;
         P.Buffer (P.Buffer'First+Name'Length .. P.Buffer'Last) :=(Others => ' ');       
      else
         P.Buffer := Name(Name'First..Name'First+P.Buffer'Length-2);
      end if;
   end Set_Name;

   ----------------------------------------------------------------------------
   --
   --  Get_Name: get the filename stored in a file dialog.
   --
   function Get_Name (
         Dialog : in     File_Dialog_Type ) 
     return String is 
      P : File_Dialog_Ptr := File_Dialog_Ptr (Get_Internals (Dialog, "Get_Name"));  
   begin
      return Trim(P.Buffer,ada.Strings.Both);
   end Get_Name;

   ----------------------------------------------------------------------------
   --  FUNCTIONALITY LOSE
   --  Add_Filter: add a filename filter to a file dialog.
   --
   procedure Add_Filter (
         Dialog : in     File_Dialog_Type; 
         Text   : in     String;           
         Filter : in     String            ) is 
      P : File_Dialog_Ptr := File_Dialog_Ptr (Get_Internals (Dialog, "Add_Filter"));  
   begin
      null;
   end Add_Filter;

   ----------------------------------------------------------------------------
   --
   --  Set_Directory: select the initial directory for a file dialog.
   --
   procedure Set_Directory (
         Dialog : in     File_Dialog_Type; 
         Name   : in     String            ) is 
      P : File_Dialog_Ptr := File_Dialog_Ptr (Get_Internals (Dialog, "Set_Directory"));  
      L : Natural      := Natural'Min (Name'Length, P.Directory'Length - 1);  
   begin
      P.Directory (P.Directory'First .. P.Directory'First+L-1) :=Name(Name'First..Name'First+Integer(L)-1);
      P.Directory (P.Directory'First+L .. P.Directory'Last) :=(Others => ' ');
   end Set_Directory;

   ----------------------------------------------------------------------------
   --
   --  Open_Dialog: construct a file open dialog.
   --
   function Open_Dialog (
         Title : String ) 
     return Open_Dialog_Type is 
      D : Open_Dialog_Type;  
      P : File_Dialog_Ptr  := new Open_Dialog_Internals;
   begin 
      D.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Open_Dialog;

      Gtk.File_Selection.Gtk_New(Gtk_File_Selection(P.Widget),Locale_to_UTF8(Title) );
      Gtk.Window.Set_Modal(Gtk_Window(P.Widget),True);
      Gtk.Window.Set_Resizable(Gtk_Window(P.Widget),False);
      Gtk.Window.Set_Policy(Gtk_Window(P.Widget),False,False,False);
      
      P.Buffer:=(Others => ' ');
      P.Directory:=(Others => ' ');
      P.Filter:=(Others => ' ');
      
      Set_Show_File_Op_Buttons (Gtk_File_Selection(P.Widget), False);

      Dialog_Bool_Return_Callback.Connect(P.Widget,"delete_event",Dialog_Bool_Return_Callback.To_Marshaller (Dialog_Delete_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Ok_Button(Gtk_File_Selection(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),True);
      Dialog_Sel_Cb.Connect(Get_Cancel_Button(Gtk_File_Selection(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);

      return D;
   end Open_Dialog;

   ----------------------------------------------------------------------------
   --
   --  Save_Dialog: construct a file save dialog.
   --
   function Save_Dialog (
         Title  : String;         
         Create : Boolean := True ) 
     return Save_Dialog_Type is 
      D : Save_Dialog_Type;  
      P : File_Dialog_Ptr  := new Save_Dialog_Internals;
   begin
      D.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Save_Dialog;

      Gtk.File_Selection.Gtk_New(Gtk_File_Selection(P.Widget),Locale_To_UTf8(Title) );
      Gtk.Window.Set_Modal(Gtk_Window(P.Widget),True);
      Gtk.Window.Set_Resizable(Gtk_Window(P.Widget),False);
      Gtk.Window.Set_Policy(Gtk_Window(P.Widget),False,False,False);

      P.Buffer:=(Others => ' ');
      P.Directory:=(Others => ' ');
      P.Filter:=(Others => ' ');
      P.Create:=Create;

      Set_Show_File_Op_Buttons (Gtk_File_Selection(P.Widget), False);

      Dialog_Bool_Return_Callback.Connect(P.Widget,"delete_event",Dialog_Bool_Return_Callback.To_Marshaller (Dialog_Delete_Event'access),False);
      Dialog_Sel_Cb.Connect(Get_Ok_Button(Gtk_File_Selection(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),True);
      Dialog_Sel_Cb.Connect(Get_Cancel_Button(Gtk_File_Selection(P.Widget)),"clicked",Dialog_Sel_Cb.To_Marshaller (Dialog_Button_Event'access),False);
     
      return D;
   end Save_Dialog;

   ----------------------------------------------------------------------------
   --
   --           R A N G E     O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   ---------------------------------------------------------------
   -- VAlue_Between : auxiliary procedure to make sure we are in range
   --
   ---------------------------------------------------------------      
   procedure Value_Between (New_Val : in out Gdouble; Adj : Gtk_adjustment) is
   begin
      if New_Val < Get_Lower (Adj) then
         New_Val := Get_Lower (Adj);
      end if;
      if New_Val > Get_Upper (Adj) then
         New_Val := Get_Upper (Adj);
      end if;
   end Value_Between;
   
   ---------------------------------------------------------------
   --   Set_Value : asign value to range control an update
   --
   ---------------------------------------------------------------      
   procedure Set_Value ( R : Range_Control_Type; Val : in Float) is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble := Gdouble(Val);
   begin
      Adj:=P.Adj;
      if Adj /= null then
         Value_Between(New_Val,Adj);
         Set_Value (Adj, New_Val);
         Gtk.Adjustment.Changed(Adj);
      end if;
   end Set_Value;

   ---------------------------------------------------------------
   --   Get_Value : get value from range control
   --
   ---------------------------------------------------------------       
   function Get_Value ( R : Range_Control_Type) return Float is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble;
   begin
      Adj:=P.Adj;
      New_Val:=Get_Value (Adj);
      return Float(New_Val);
   end Get_Value;

   ---------------------------------------------------------------
   --   Set_Min : asign min value for range control an update
   --
   --------------------------------------------------------------- 
   procedure Set_Min ( R : Range_Control_Type; Min : in Float) is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble := Gdouble(Min);
   begin
      Adj:=P.Adj;
      if New_Val < Get_Upper(Adj) then
         Gtk.Adjustment.Set_Lower(Adj,New_Val);
      end if;
      Set_Value(R,Float(Gtk.Adjustment.Get_Value(Adj)));
   end Set_Min;
   
   ---------------------------------------------------------------
   --   Get_Min : get min value from range control
   --
   --------------------------------------------------------------- 
   function Get_Min ( R : Range_Control_Type) return Float is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble;
   begin
      Adj:=P.Adj;
      New_Val:=Get_Lower(Adj);
      return Float(New_Val);      
   end Get_Min;

   ---------------------------------------------------------------
   --   Set_Max : asign max value for range control an update
   --
   --------------------------------------------------------------- 
   procedure Set_Max ( R : Range_Control_Type; Max : in Float) is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble := Gdouble(Max);
   begin
      Adj:=P.Adj;
      if New_Val > Get_Lower(Adj) then
         Gtk.Adjustment.Set_Upper(Adj,New_Val);             
      end if;
      Set_Value(R,Float(Gtk.Adjustment.Get_Value(Adj)));
   end Set_Max;

   ---------------------------------------------------------------
   --   Get_Max : get max value from range control
   --
   --------------------------------------------------------------- 
   function Get_Max ( R : Range_Control_Type) return Float is
      P : Range_Ptr  :=  Range_Ptr(Get_Internals(R,"Set_Value"));
      Adj  : gtk_adjustment;
      New_Val : Gdouble;
   begin
      Adj:=P.Adj;
      New_Val:=Get_Upper(Adj);
      return Float(New_Val);        
   end Get_Max;


   ----------------------------------------------------------------------------
   --
   --           P R O G R E S S B A R     O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Progressbar: create a progressbar in some direction
   --
   function Progressbar (
         Parent     : Container_Type'Class;               
         Origin     : Point_Type;                         
         Width      : Integer;                            
         Height     : Integer; 
         Min        : Float; 
         Max        : Float; 
         Orientation: Range_Orientation) 
     return Progressbar_Type is 
      W : Progressbar_Type;  
      P : Range_Ptr  := new Range_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Button"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Progressbar;
      Gtk_New(Gtk_Progress_Bar(P.Widget));
      case Orientation is 
         when Range_L_To_R =>
            Gtk.Progress_Bar.Set_Orientation(Gtk_Progress_Bar(P.Widget),Progress_Left_To_Right);
         when Range_R_To_L =>
            Gtk.Progress_Bar.Set_Orientation(Gtk_Progress_Bar(P.Widget),Progress_Right_To_Left);
         when Range_B_To_T =>
            Gtk.Progress_Bar.Set_Orientation(Gtk_Progress_Bar(P.Widget),Progress_Bottom_To_Top);
         when Range_T_To_B =>
            Gtk.Progress_Bar.Set_Orientation(Gtk_Progress_Bar(P.Widget),Progress_Top_To_Bottom);
         when others =>
            null;
      end case;
      Gtk.Progress.Set_Show_Text(Gtk_Progress(P.Widget), False);
      Gtk.Progress.Configure(Gtk_Progress(P.Widget),Gdouble(Min),Gdouble(Min),Gdouble(Max));
      if not Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)+Can_Focus);
      end if;      
      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Parent_Font,-1,"Progressbar");
      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      

      Show(P.Widget);         
      P.Adj:=Get_Adjustment(Gtk_Progress(P.Widget));
      P.Label_Widget:=null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Progressbar;

   ----------------------------------------------------------------------------
   --
   --           S C A L E      O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Scale: create a scale (V o H).
   --
   function Scale (
         Parent     : Container_Type'Class;               
         Origin     : Point_Type;                         
         Width      : Integer;                            
         Height     : Integer; 
         Min        : Float; 
         Max        : Float; 
         Step       : Float; 
         Ndigits    : Natural:=0; 
         Orientation: Range_Orientation:=Range_L_To_R;
         Font        : Font_Type := Parent_Font) 
     return Scale_Type is 
      W : Scale_Type;  
      P : Range_Ptr  := new Range_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Button"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Scale;
      case Orientation is 
         when Range_L_To_R | Range_R_To_L =>
               Gtk_New_HScale(Gtk_Scale(P.Widget),Gdouble(Min),Gdouble(Max),Gdouble(Step));
         when Range_B_To_T | Range_T_To_B =>
               Gtk_New_VScale(Gtk_Scale(P.Widget),Gdouble(Min),Gdouble(Max),Gdouble(Step));
      end case;
      Gtk.Scale.Set_Draw_Value(Gtk_Scale(P.Widget), True);
      Gtk.Scale.Set_Digits(Gtk_Scale(P.Widget), Gint(Ndigits));
      Gtk.grange.Set_Update_Policy(Gtk_GRange(P.Widget),Update_Continuous);
      if not Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)+Can_Focus);
      else
         if Step=0.0 then
            Set_Flags(P.Widget,Flags(P.Widget)-Can_Focus);
         end if;  
      end if;      
      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Scale");
      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      

      Show(P.Widget);     

      P.Adj := Get_Adjustment(Gtk_GRange(P.Widget));    
      P.Label_Widget:=null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Scale;

   ----------------------------------------------------------------------------
   --
   --           S C R O L L B A R      O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Scale: create a scrollbar (V o H).
   --
   function Scrollbar (
         Parent     : Container_Type'Class;               
         Origin     : Point_Type;                         
         Width      : Integer;                            
         Height     : Integer; 
         Min        : Float; 
         Max        : Float; 
         Step       : Float;  
         Orientation: Range_Orientation:=Range_L_To_R) 
     return Scrollbar_Type is 
      W : Scrollbar_Type;  
      P : Range_Ptr  := new Range_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Button"));
      Adj : Gtk_Adjustment;
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Scrollbar;
      Gtk.Adjustment.Gtk_New(Adj, Gdouble(Min), Gdouble(Min), Gdouble(Max), Gdouble(Step), Gdouble(Step*4.0),0.0);
      case Orientation is 
         when Range_L_To_R | Range_R_To_L =>
               Gtk_New_HScrollbar(Gtk_Scrollbar(P.Widget),Adj);
         when Range_B_To_T | Range_T_To_B =>
               Gtk_New_VScrollbar(Gtk_Scrollbar(P.Widget),Adj);
      end case;
      Gtk.grange.Set_Update_Policy(Gtk_GRange(P.Widget),Update_Continuous);
      if Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)-Can_Focus);
      end if;      

      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Parent_Font,-1,"Scrollbar");
      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      


      Show(P.Widget);     

      P.Adj := Get_Adjustment(Gtk_GRange(P.Widget));    
      P.Label_Widget:=null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Scrollbar;


   ----------------------------------------------------------------------------
   --
   --           S P I N B U T T O N      O P E R A T I O N S
   --
   ----------------------------------------------------------------------------
   --
   --  Spinbutton: create a spinbutton.
   --
   function Spinbutton (
         Parent     : Container_Type'Class;               
         Origin     : Point_Type;                         
         Width      : Integer;                            
         Height     : Integer; 
         Min        : Float; 
         Max        : Float; 
         Step       : Float; 
         Ndigits    : Natural:=0;
         Font        : Font_Type := Parent_Font) 
     return Spinbutton_Type is 
      W : Spinbutton_Type;  
      P : Range_Ptr  := new Range_Internals;
      C : Container_Ptr := Container_Ptr(Get_Internals (Parent, "Button"));
   begin
      W.Internals.Pointer := Reference_Counted_Ptr(P);
      P.Tipo:=Type_Scale;
      Gtk_New(Gtk_Spin_Button(P.Widget),Gdouble(Min),Gdouble(Max),Gdouble(Step));
      Gtk.Spin_Button.Set_Snap_To_Ticks(Gtk_Spin_Button(P.Widget), False);
      Gtk.Spin_Button.Set_Digits(Gtk_Spin_Button(P.Widget), Guint(Ndigits));
      Gtk.spin_button.Set_Wrap(Gtk_Spin_Button(P.Widget), True);
      Gtk.spin_button.Set_Update_Policy(Gtk_Spin_button(P.Widget),Update_If_Valid);
      if not Can_Focus_Is_Set(P.Widget) then
         Set_Flags(P.Widget,Flags(P.Widget)+Can_Focus);
      end if;      

      Put(C.Fix,P.Widget,Gint(Origin.X),Gint(Origin.Y));
      Fill_Child(W,Parent,Origin,Width,Height,Font,-1,"Spinbutton");
      Set_Origin_Internals(Window_Ptr(P),C,Origin,True);
      Set_Size_Internals(Window_Ptr(P),C,Width,Height,True);      

      Show(P.Widget);     

      P.Adj := Get_Adjustment(Gtk_Spin_Button(P.Widget));    
      P.Label_Widget:=null;
      LastObject:=Window_Ptr(W.Internals.Pointer);
      return W;
   end Spinbutton;



  ----------------------------------------------------------------------------
  --
  --                       T O O L T I P S
  --
  ----------------------------------------------------------------------------
   ---------------------------------------------------------------
   --   Set_Tooltip : set text value of text for public or private part
   --                 if Parent and Control the group have one component
   --                 else if parent have group, the control get this group of tooltip
   --
   --------------------------------------------------------------- 
   procedure Set_Tooltip (Parent : Control_Type'Class; Control : in Control_Type; Text : in String; Privatetext: in String:="") is
      Tip : Gtk_Tooltips;
      P   : Window_Ptr:=Window_Ptr(Get_Internals(Parent,"Set_Tooltip"));
      W   : Window_Ptr:=Window_Ptr(Get_Internals(Control,"Set_Tooltip"));
   begin
      --If Parent is equal Control, we have to assign the values tooltip of Control
      if P = W then
         --If Control don't have Tooltip, we create it
         if P.Tooltip = null then
            Gtk_New(P.Tooltip);
            gtk.tooltips.Enable(P.Tooltip);   
         end if;
         Tip := P.Tooltip;
      else
         P :=Window_Ptr(Get_Internals(Parent,"Set_Tooltip"));   
         if P.Tooltip = null then
            Gtk_New(P.Tooltip);
            Gtk.Tooltips.Enable(P.Tooltip);
         end if;
         Tip := P.Tooltip;
         W.Tooltip := P.Tooltip;
      end if;
      Gtk.Tooltips.Set_Tip(
                  Tooltips => Tip,
                  Widget   => W.Widget,
                  Tip_Text => Locale_To_UTF8(Text),
                  Tip_Private => Locale_To_UTF8(PrivateText));                                
  end Set_Tooltip;
            
   ---------------------------------------------------------------
   --   Tooltip_On : Activate tooltip's of the group
   --
   --------------------------------------------------------------- 
  procedure Tooltip_On (Control : in Control_Type) is
      W   : Window_Ptr:=Window_Ptr(Get_Internals(Control,"Tooltip_On"));
  begin             
     if W.Tooltip /= null then
        Gtk.Tooltips.Enable(W.Tooltip);
     end if;
  end Tooltip_On;
  
   ---------------------------------------------------------------
   --   Tooltip_Off : Disable tooltip's of the group
   --
   --------------------------------------------------------------- 
  procedure Tooltip_Off (Control : in Control_Type) is
      W   : Window_Ptr:=Window_Ptr(Get_Internals(Control,"Tooltip_Off"));
  begin             
     if W.Tooltip /= null then
        Gtk.Tooltips.Disable(W.Tooltip);
     end if;
  end Tooltip_Off;

   ---------------------------------------------------------------
   --   Get_Tooltip : get the public text in tooltip
   --
   --------------------------------------------------------------- 
  function Get_Tooltip (Control : in Control_Type) return String is
     W   : Window_Ptr:=Window_Ptr(Get_Internals(Control,"Get_Tooltip"));
     
  begin             
     if W.Tooltip /= null then
        declare 
           Data: Gtk.Tooltips.Tooltips_Data:= Gtk.Tooltips.Get_Data(W.Widget);
        begin
           return Locale_From_UTF8(Data.Text);
        end;
     else
        return "";
     end if;
  end Get_Tooltip;
     
   ---------------------------------------------------------------
   --   Get_Tooltip : get the private text in tooltip
   --
   --------------------------------------------------------------- 
  function Get_Tooltip_Private (Control : in Control_Type) return String is
     W   : Window_Ptr:=Window_Ptr(Get_Internals(Control,"Get_Tooltip_Private"));
  begin             
     if W.Tooltip /= null then
        declare 
           Data: Gtk.Tooltips.Tooltips_Data:=Gtk.Tooltips.Get_Data(W.Widget);
        begin
           return Locale_From_UTF8(Data.Text_Private);
        end;
     else
        return "";
     end if;
  end Get_Tooltip_Private;
  
   ---------------------------------------------------------------
   --   Set_Action : change or asign new command to window
   --
   --------------------------------------------------------------- 
  procedure Set_Action(Window : in Window_Type; Command : in Command_Type) is
     W   : Window_Ptr:=Window_Ptr(Get_Internals(Window,"Set_Action"));
  begin
     W.Action:=Command_Type'Pos(Command);
  end Set_Action;

   ---------------------------------------------------------------
   --   Set_KeyAction : change or asign new key command to canvas
   --
   --------------------------------------------------------------- 
  procedure Set_KeyAction(Canvas : in Canvas_Type; Command : in Command_Type) is
     W   : Canvas_Ptr:=Canvas_Ptr(Get_Internals(Canvas,"Set_KeyAction"));
  begin
     W.Keypress:=Command_Type'Pos(Command);
  end Set_KeyAction;

   ---------------------------------------------------------------
   --   Disable_Action : delete command from window
   --
   --------------------------------------------------------------- 
  procedure Disable_Action(Window : in Window_Type) is
     W   : Window_Ptr:=Window_Ptr(Get_Internals(Window,"Disable_Action"));
  begin
     W.Action:=-1;
  end Disable_Action;

   ---------------------------------------------------------------
   --   Disable_KeyAction : delete key command from canvas
   --
   --------------------------------------------------------------- 
   procedure Disable_KeyAction(Canvas : in Canvas_Type) is
      W   : Canvas_Ptr:=Canvas_Ptr(Get_Internals(Canvas,"Disable_KeyAction"));
   begin
      W.Keypress:=-1;
   end Disable_KeyAction;

   ---------------------------------------------------------------
   --   Have_Action : test if command is disabled
   --
   --------------------------------------------------------------- 
  function Have_Action(Window : in Window_Type) return Boolean is
     W   : Window_Ptr:=Window_Ptr(Get_Internals(Window,"Have_Action"));
  begin
     return (W.Action/=-1);
  end Have_Action;

   ---------------------------------------------------------------
   --   Have_KeyAction : test if key command is disabled
   --
   --------------------------------------------------------------- 
  function Have_KeyAction(Canvas : in Canvas_Type) return Boolean is
     W   : Canvas_Ptr:=Canvas_Ptr(Get_Internals(Canvas,"Have_KeyAction"));
  begin
     return (W.Keypress/=-1);
  end Have_KeyAction;




   ------------------------------------------------------------------------------
   --
   --               P A C K A G E   I N I T I A L I S A T I O N
   --
   --    Register the window classes if there is no previous module instance.
   --
   ------------------------------------------------------------------------------
begin
   --Innitialize the events control
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk.Type_Conversion.Init;
   Lastobject:=new Window_Internals;
   Lastobject.Tipo:=Type_None;
   --Calculate de frame dimansions
   declare
      Win:Gtk_Window;
      H:Natural:=200;
      R:gdk.rectangle.Gdk_Rectangle:=gdk.rectangle.Full_Area;
   begin
      Gtk_New(Win);
      Show(Win);
      Gdk.Window.Move_Resize(Get_Window(Gtk_Widget(Win)),
         Gint(Screen_Width +1),Gint(Screen_Height+1),
         Gint(H),Gint(H));
            
      Gdk.Window.Get_Frame_Extents(Get_Window(Gtk_Widget(Win)),R);
      Frame_Width_Constant:=Natural(R.Width-Grectangle_Length(H));
      Frame_Height_Constant:=Natural(R.height-GRectangle_Length(H));
      Destroy(Win);
   end;
   Frame_Border_Constant :=Frame_Width_Constant/2;
   Frame_Title_Constant  :=Frame_Height_Constant - (Frame_Width_Constant/2);
  
   Frame_Width_Min       := 104;
   Frame_Height_Min      := Frame_Height_Constant + 1;

   Menu_Height_Constant  := Frame_Height_Constant;

end GtkJewl.Windows;
