------------------------------------------------------------------------------
--
--      G T K J E W L . C A N V A S _ I M P L E M E N T A T I O N
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
with ada.Unchecked_Conversion;
with Ada.Text_Io;
with System;

with GtkJewl.Double_Buffer;   use GtkJewl.Double_Buffer;

with Glib;         use Glib;
with gdk;          use gdk;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.drawable; use Gdk.drawable;
with Gdk.Gc;       use Gdk.Gc;
with Gdk.Font;     use Gdk.Font;
with Gdk.Window;   use Gdk.Window;

with Gdk.Color;    use Gdk.Color;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gdk.Rectangle;use Gdk.Rectangle;

with Pango.Enums;  use Pango.Enums;
with Pango.context;use Pango.context;
with Pango.Font;   use Pango.Font;
with Pango.layout; use Pango.layout;

package body GtkJewl.Canvas_Implementation is

--  use GtkJewl.Win32_Interface;

  procedure Free is new Ada.Unchecked_Deallocation
                            (Canvas_Object_Type'Class, Canvas_Object_Ptr);

  function To_PaintContext_Ptr is new Ada.Unchecked_Conversion
        (System.Address,PaintContext_Ptr);
   
  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  This protected type mediates between the canvas operations and the
  --  message loop task.
  --
  ----------------------------------------------------------------------------

  protected body Canvas_Monitor is

    --------------------------------------------------------------------------
    --
    --  Clear: delete all objects on the drawing list by restoring to the
    --         beginning of the list.
    --
    procedure Clear is
    begin
      Save_Pointer := null;
      Restore;
    end Clear;

    --------------------------------------------------------------------------
    --
    --  Save: record the current position in the drawing list.
    --
    procedure Save is
    begin
      Save_Pointer := Last_Object;
    end Save;

    --------------------------------------------------------------------------
    --
    --  Restore: truncate the drawing list back to the saved position (or
    --           the beginning, if the saved position is null) and delete
    --           any objects removed in the process.
    --
    procedure Restore is
      P,Q : Canvas_Object_Ptr;
    begin
      if Save_Pointer = null then
        P := First_Object;
        First_Object := null;
        Last_Object  := null;
      else
        Last_Object := Save_Pointer;
        P := Last_Object.Next;
        Last_Object.Next := null;
      end if;
      while P /= null loop
        Q := P;
        P := P.Next;
        Free (Q);
      end loop;
    end Restore;

    --------------------------------------------------------------------------
    --
    --  Draw: draw all objects on the drawing list.
    --
   procedure Draw (Buffer : in Gtk_Double_Buffer;
                      Pctx   : in Paintcontext;
                      No_List : in Boolean :=False) is
      P : Canvas_Object_Ptr := First_Object;
   begin
      Context.Pixmap   :=Get_Pixmap(Buffer);
      if First_Object = null and then Pctx.PenGC /= null then
      declare
         P : Canvas_Object_Ptr := new Handle_Type;
      begin
         Handle_Type(P.All).Pen :=PCtx.Pen;
         Handle_Type(P.All).PenGC :=PCtx.PenGC;
         Handle_Type(P.All).Fill :=PCtx.Fill;
         Handle_Type(P.All).FillGC :=PCtx.FillGC;
         Handle_Type(P.All).Font :=PCtx.Font;
         Add (P);
      end;
      end if;         

      if No_List then
         --Double_Buffer.Copy(Buffer);   
         null;
      else
         Double_Buffer.Clear(Buffer);
      end if;
      while P /= null loop
          Draw (P.all, Context);
          P := P.Next;
      end loop;
      if No_List then
         Clear;
      end if;
      Double_Buffer.Draw(Buffer);

    end Draw;

    --------------------------------------------------------------------------
    --
    --  Add: add a new object to the end of the drawing list.
    --
    procedure Add (Object : in Canvas_Object_Ptr) is
    begin
      if Last_Object = null then
          First_Object := Object;
      else
        Last_Object.Next := Object;
      end if;
      Last_Object := Object;
      Object.Next := null;
    end Add;

    --------------------------------------------------------------------------
    --
    --  Set_Bg_Color: store the brush used to erase the background.
    --
    procedure Set_Bg_Color (color : in Colour_Type) is
    begin
      Bg_Color:=color;
    end Set_Bg_Color;

    --------------------------------------------------------------------------
    --
    --  Get_Bg_Color: get the background brush. This is called by the message
    --              loop task in response to a WM_ERASEBKGND message.
    --
    function Get_Bg_Color return Colour_Type is
    begin
      return BG_Color;
    end Get_Bg_Color;    

    --------------------------------------------------------------------------
    --
    --  Set_Start: store the position where the mouse button was pressed.
    --             This is called by the message loop task in response to
    --             a WM_LBUTTONDOWN message. The end position is initially
    --             set to match the start position.
    --
    procedure Set_Start (X, Y : in Integer) is
    begin
      Start_X := X;
      Start_Y := Y;
      End_X := X;
      End_Y := Y;
      Moved := False;
    end Set_Start;

    --------------------------------------------------------------------------
    --
    --  Get_Start: get the position where the mouse button was pressed.
    --
    procedure Get_Start (X, Y : out Integer) is
    begin
      X := Start_X;
      Y := Start_Y;
    end Get_Start;

    --------------------------------------------------------------------------
    --
    --  Set_End: store the current mouse position. This is called by
    --           the message loop task in response to a WM_MOUSEMOVE
    --           or WM_LBUTTONUP message. The Moved flag is set true
    --           to indicate that the mouse has moved.
    --
    procedure Set_End (X, Y : in Integer) is
    begin
      End_X := X;
      End_Y := Y;
      Moved := True;
    end Set_End;

    --------------------------------------------------------------------------
    --
    --  Get_End: get the current mouse position. The Moved flag is reset
    --           so that Mouse_Moved will return False until the mouse is
    --           moved again.
    --
    procedure Get_End (X, Y : out Integer) is
    begin
      X := End_X;
      Y := End_Y;
      Moved := False;
    end Get_End;

    --------------------------------------------------------------------------
    --
    --  Set_Button: store the current mouse button state. This is called
    --              from the message loop task in response to WM_LBUTTONUP
    --              or WM_LBUTTONDOWN messages.
    --
    procedure Set_Button (B : in Boolean) is
    begin
      Button := B;
    end Set_Button;

    --------------------------------------------------------------------------
    --
    --  Mouse_Down: get the current mouse button state.
    --
    function Mouse_Down return Boolean is
    begin
      return Button;
    end Mouse_Down;

    --------------------------------------------------------------------------
    --
    --  Mouse_Moved: test if the mouse has moved since the last time that
    --               Get_End was called.
    --
    function Mouse_Moved return Boolean is
    begin
      return Moved;
    end Mouse_Moved;

    --------------------------------------------------------------------------
    --
    --  Set_Key: store the character corresponding to a key that has been
    --           pressed. This is called from the message loop in response
    --           to WM_CHAR messages.
    --
    procedure Set_Key (C : in Character) is
    begin
      Keycode := C;
    end Set_Key;

    --------------------------------------------------------------------------
    --
    --  Get_Key: return the character corresponding to a key that has been
    --           pressed.
    --
    procedure Get_Key (C : out Character) is
    begin
      C := Keycode;
      Keycode := ASCII.NUL;
    end Get_Key;


--    procedure Set_Action (Code : in Integer) is 
--    begin
--       Action:=Code;
--    end Set_Action;
--    
--    procedure Set_Keypress (Code : in Integer) is 
--    begin
--       Keypress:=Code;
--    end Set_Keypress;
--    
--    function Get_Action return Integer is
--    begin
--       return Action;
--    end Get_Action;
--    
--    function Get_Keypress return Integer is
--    begin
--       return Keypress;
--    end Get_Keypress;

  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 D R A W I N G   O P E R A T I O N S
  --
  --  The following procedures are the implementations of Draw for the
  --  different types of canvas objects. They are called by Draw in the
  --  canvas monitor, which dispatches to the appropriate procedure for
  --  each object in the drawing list.
  --
  ----------------------------------------------------------------------------
  --
  --  Draw a text string
  --
  procedure Draw (Object : in out Text_Type;
                  Window : in out PaintContext) is
     Gc : Gdk.Gc.Gdk_Gc;
     R  : Gdk.Rectangle.Gdk_Rectangle;
     DAlign : Gint;
  begin
    -- Calculate the bounding rectangle
     Gdk_New(Gc,Window.Pixmap);
     Gdk.Gc.Copy(Gc,Window.Pengc);
     if Object.Align >= 0 then
        R.X   := Gint(Object.From.X);
        R.Y    := Gint(Object.From.Y);
        R.Width  := Gint(Object.To.X - Object.From.X);
        R.Height := Gint(Object.To.Y - Object.From.Y);
        Gdk.Gc.Set_Clip_Rectangle(Gc,R);
     end if;
        

    -- Select the appropriate alignment flag (-1 is used to indicate
    -- that the text is not clipped by the bounding rectangle, and 0
    -- upwards are values generated by Alignment_Type'Pos).

    if Object.Align < 0 then  --No Align
      DAlign:=0;
    elsif Object.Align = 0 then  --Align Left
      Dalign:=0;
    elsif Object.Align = 1 then  --Align Center
      Dalign:=Gint((Object.To.X - Object.From.X)/2) - (String_Width(Window.Font,Object.Text)/2);
    else                        --Align Right
      Dalign:=Gint((Object.To.X - Object.From.X)) - (String_Width(Window.Font,Object.Text));
   end if;
     
   if Window.Pen then
    -- Now draw the text
        Gdk.Drawable.Draw_Text(
           Drawable => Window.Pixmap,
           Font     => Window.Font,
           Gc       => GC,
           X        => Gint(Object.From.X) + Dalign,
           Y        => Gint(Object.From.Y) + gdk.font.String_Height(window.Font,Object.Text),
           Text     => Object.Text);
           
   end if;
   Unref(GC);  
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a line
  --
  procedure Draw (Object : in out Line_Type;
                  Window : in out PaintContext) is
  begin
     if Window.Pen then
        Gdk.Drawable.Draw_Line(
           Drawable => Window.Pixmap,
           Gc       => Window.PenGC,
           X1       => Gint(Object.From.X),
           Y1       => Gint(Object.From.Y),
           X2       => Gint(Object.To.X),
           Y2       => Gint(Object.To.Y));
      end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle
  --
  procedure Draw (Object : in out Rectangle_Type;
                  Window : in out PaintContext) is
  begin
     if Window.Pen = False and Window.Fill = False then
        return;
     end if;   
     if Object.From.X = Object.To.X or Object.From.Y = Object.To.Y then
        if Window.Fill then
           Gdk.Drawable.Draw_Line(Window.Pixmap,Window.Fillgc,
              Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
        end if;
        if Window.Pen then
           Gdk.Drawable.Draw_Line(Window.Pixmap,Window.Pengc,
              Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
        end if;
     else
        if Window.Fill then
        Gdk.Drawable.Draw_Rectangle(
           Drawable => Window.Pixmap,
           Gc       => Window.FillGC,
           Filled   => True,
           X        => Gint(Object.From.X),
           Y        => Gint(Object.From.Y),
           Width    => Gint(Object.To.X - Object.From.X),
           Height   => Gint(Object.To.Y - Object.From.Y));
        end if;
        if Window.Pen then
        Gdk.Drawable.Draw_Rectangle(
           Drawable => Window.Pixmap,
           Gc       => Window.PenGC,
           Filled   => False,
           X        => Gint(Object.From.X),
           Y        => Gint(Object.From.Y),
           Width    => Gint(Object.To.X - Object.From.X),
           Height   => Gint(Object.To.Y - Object.From.Y));
        end if;
      end if;  
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle with rounded corners
  --
  procedure Draw (Object : in out Rounded_Rectangle_Type;
        Window : in out Paintcontext) is
     P1:Point_Type;
     P2:Point_Type;
     W,H : Integer;
     CW,CH : Integer;
  begin
     if Window.Pen = False and Window.Fill = False then
        return;
     end if;   
     if Object.From.X = Object.To.X or Object.From.Y = Object.To.Y then
        if Window.Fill then
           Gdk.Drawable.Draw_Line(Window.Pixmap,Window.FillGC,
              Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
        end if;
        if Window.Pen then
           Gdk.Drawable.Draw_Line(Window.Pixmap,Window.Pengc,
              Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
        end if;
     else
        P1:=Object.From;
        P2:=Object.To;
--        if Object.From.X > Object.To.X then
--           P1.X:=Object.To.X; P2.X:=Object.From.X;
--        else
--           P1.X:=Object.From.X; P2.X:=Object.To.X;
--        end if;
--        if Object.From.Y > Object.To.Y then
--           P1.Y:=Object.To.Y; P2.Y:=Object.From.Y;
--        else
--           P1.Y:=Object.From.Y; P2.Y:=Object.To.Y;
--        end if;
        W:=Abs(P1.X - P2.X);
        H:=Abs(P1.Y - P2.Y);
        if Abs(Object.Corner.X) < W/2 then
           CW:=Abs(Object.Corner.X);
        else
           CW:=W/2;
        end if;
        if Abs(Object.Corner.Y) < H/2 then
           CH:=Abs(Object.Corner.Y);
        else
           CH:=H/2;
        end if;
        if Window.Fill then
           Gdk.Drawable.Draw_Rectangle(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P1.X + CW),
              Y        => Gint(P1.Y),
              Width    => Gint(W - (2*CW)),
              Height   => Gint(H));
           Gdk.Drawable.Draw_Rectangle(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P1.X),
              Y        => Gint(P1.Y + CH),
              Width    => Gint(W),
              Height   => Gint(H - (2*Ch)));
              ------------Chaflanes
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P1.X),
              Y        => Gint(P1.Y),
              Width    => Gint(CW*2+1),
              Height   => Gint(CH*2+1),
              Angle1   => 64*90,
              Angle2   => 64*91);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P1.X),
              Y        => Gint(P2.Y - 2*CH),
              Width    => Gint(CW*2+1),
              Height   => Gint(CH*2+1),
              Angle1   => 64*180,
              Angle2   => 64*91);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P2.X - 2*CW),
              Y        => Gint(P2.Y - 2*CH),
              Width    => Gint(CW*2+1),
              Height   => Gint(CH*2+1),
              Angle1   => 64*270,
              Angle2   => 64*91);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(P2.X - 2*CW),
              Y        => Gint(P1.Y),
              Width    => Gint(CW*2+1),
              Height   => Gint(CH*2+1),
              Angle1   => 0,
              Angle2   => 64*91);
        end if;
        --Parte del Borde
        if Window.Pen then
           Gdk.Drawable.Draw_Line(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              X1       => Gint(P1.X+CW-1),
              Y1       => Gint(P1.Y),
              X2       => Gint(P2.X-CW+1),
              Y2       => Gint(P1.Y));     -- Arriba
           Gdk.Drawable.Draw_Line(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              X1       => Gint(P2.X),
              Y1       => Gint(P1.Y+CH-1),
              X2       => Gint(P2.X),
              Y2       => Gint(P2.Y-CH+1));  -- Derecha
           Gdk.Drawable.Draw_Line(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              X1       => Gint(P2.X-CW-1),
              Y1       => Gint(P2.Y),
              X2       => Gint(P1.X+CW),
              Y2       => Gint(P2.Y));     -- Abajo
           Gdk.Drawable.Draw_Line(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              X1       => Gint(P1.X),
              Y1       => Gint(P1.Y+CH-1),
              X2       => Gint(P1.X),
              Y2       => Gint(P2.Y-CH+1));  -- Izquierda
              ------------Chaflanes
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              Filled   => False,
              X        => Gint(P1.X),
              Y        => Gint(P1.Y),
              Width    => Gint(CW*2),
              Height   => Gint(CH*2),
              Angle1   => 64*90,
              Angle2   => 64*90);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              Filled   => False,
              X        => Gint(P1.X),
              Y        => Gint(P2.Y - 2*CH),
              Width    => Gint(CW*2),
              Height   => Gint(CH*2),
              Angle1   => 64*180,
              Angle2   => 64*91);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              Filled   => False,
              X        => Gint(P2.X - 2*CW),
              Y        => Gint(P2.Y - 2*CH),
              Width    => Gint(CW*2),
              Height   => Gint(CH*2),
              Angle1   => 64*270,
              Angle2   => 64*90);
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              Filled   => False,
              X        => Gint(P2.X - 2*CW),
              Y        => Gint(P1.Y),
              Width    => Gint(CW*2),
              Height   => Gint(CH*2),
              Angle1   => 0,
              Angle2   => 64*90);
         end if;
      end if;                    
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw an ellipse
  --
  procedure Draw (Object : in out Ellipse_Type;
                  Window : in out PaintContext) is
  begin
     if Window.Fill then
        if Object.From.X = Object.To.X or Object.From.Y = Object.To.Y then
           if Window.Pen then
              Gdk.Drawable.Draw_Line(Window.Pixmap,Window.FillGC,
                 Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
           end if;
        else
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.FillGC,
              Filled   => True,
              X        => Gint(Object.From.X),
              Y        => Gint(Object.From.Y),
              Width    => Gint(Object.To.X - Object.From.X),
              Height   => Gint(Object.To.Y - Object.From.Y),
              Angle1   => -1,
              Angle2   => 361*64);
        end if;
     end if;
     if Window.Pen then
        if Object.From.X = Object.To.X or Object.From.Y = Object.To.Y then
           if Window.Pen then
              Gdk.Drawable.Draw_Line(Window.Pixmap,Window.Pengc,
                 Gint(Object.From.X),Gint(Object.From.Y),Gint(Object.To.X),Gint(Object.To.Y));
           end if;
        else
           Gdk.Drawable.Draw_Arc(
              Drawable => Window.Pixmap,
              Gc       => Window.PenGC,
              Filled   => False,
              X        => Gint(Object.From.X),
              Y        => Gint(Object.From.Y),
              Width    => Gint(Object.To.X - Object.From.X),
              Height   => Gint(Object.To.Y - Object.From.Y),
              Angle1   => -1,
              Angle2   => 361*64);
        end if;
     end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polyline
  --
  procedure Draw (Object : in out Polyline_Type;
                  Window : in out PaintContext) is
  begin
     if Window.Pen then
        Gdk.Drawable.Draw_Lines(
           Drawable => Window.Pixmap,
           Gc       => Window.PenGC,
           Points   => Object.Points);
     end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polygon
  --
  procedure Draw (Object : in out Polygon_Type;
                  Window : in out PaintContext) is
  begin
     if Window.Fill then
        Gdk.Drawable.Draw_Polygon(
           Drawable => Window.Pixmap,
           Gc       => Window.FillGC,
           Filled   => True,
           Points   => Object.Points);
      end if;           
     if Window.Pen then
        Gdk.Drawable.Draw_Polygon(
           Drawable => Window.Pixmap,
           Gc       => Window.PenGC,
           Filled   => False,
           Points   => Object.Points);
     end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a bitmap
  --
  procedure Draw (Object : in out Bitmap_Type;
                  Window : in out PaintContext) is
     I : Image_Ptr := Image_Ptr(Object.Bitmap.Pointer);
     Img : Gdk_Pixbuf;
  begin
     --Evitamos pintar objetos no existentes o no visibles, pero 
     --dejamos que aparezca el objeto en la lista de objetos a pintar
     if I /= null and Object.Width>0 and Object.Height>0  then
        --Escalamos la imagen, sobre una temporal
        if Object.Width > 10 or Object.Height > 10 then
           Img:=Gdk.Pixbuf.Scale_Simple(
              Src         => I.Img,
              Dest_Width  => Gint(Object.Width),
              Dest_Height => Gint(Object.Height));
        else
           Img:=Gdk.Pixbuf.Gdk_New_Subpixbuf(
              Src_Pixbuf => I.Img,
              Src_X      => 0,
              Src_Y      => 0,
              Width      => Gint(Object.Width),
              Height     => Gint(Object.Height));
        end if;  
        --Pintamos la imagen
        Gdk.Pixbuf.Render_To_Drawable(
           Pixbuf   => Img,
           Drawable => Window.Pixmap,
           Gc       => Window.PenGC,
           Src_X    => 0,
           Src_Y    => 0,
           Dest_X   => Gint(Object.From.X),
           Dest_Y   => Gint(Object.From.Y),
           Width    => Gint(Object.Width),
           Height   => Gint(Object.Height));  
        Unref(Img);      
     end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Select a drawing tool
  --
  procedure Draw (Object : in out Handle_Type;
                  Window : in out PaintContext) is
  begin
      --Cambiamos del PaintContext el elemento necesario
      if Object.PenGC /= Gdk.Gc.Null_Gc then
        Window.Pen   :=Object.Pen;
        Window.Pengc :=Object.Pengc;
      end if;
      if Object.FillGC /= Gdk.Gc.Null_Gc then
        Window.Fill   :=Object.Fill;
        Window.FillGC :=Object.FillGC;
      end if;
      if Object.Font /= Gdk.Font.Null_Font then
        Window.Font   :=Object.Font;
      end if;
  end Draw;

  ----------------------------------------------------------------------------
  --
  --         C O N T R O L L E D   T Y P E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Cleanup: destroy a bitmap handle in an Image_Internals object.
  --
  procedure Cleanup (Object : in out Image_Internals) is
  begin
     if Object.Img /=null then
        Unref(Object.Img);
     end if;
  end Cleanup;

end GtkJewl.Canvas_Implementation;