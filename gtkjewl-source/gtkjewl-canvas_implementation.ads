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


with Ada.Finalization;

with GtkJewl.Double_Buffer;   use GtkJewl.Double_Buffer;

with Glib;         use Glib;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;
with Gdk.Font;     use Gdk.Font;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gdk.Types;    use Gdk.Types;

with gtk.widget;   use gtk.widget;

private package GtkJewl.Canvas_Implementation is

  type Point_List is array (Positive range <>) of aliased Gdk_Point;

  type Paintcontext is 
  record
     Pixmap  : aliased Gdk.drawable.Gdk_Drawable:=Null_Drawable;
     Pen     : Boolean := False;
     PenGC   : aliased Gdk.Gc.Gdk_Gc:=Null_GC;
     Fill    : Boolean := False;
     FillGC  : aliased Gdk.Gc.Gdk_Gc:=Null_Gc;
     Font    : aliased Gdk.Font.Gdk_Font:=Null_Font;
  end record;
  
  type Paintcontext_Ptr is access all Paintcontext;
  
  ----------------------------------------------------------------------------
  --  Forward declarations (defined fully later).
  ----------------------------------------------------------------------------

  type Canvas_Object_Type;
  type Canvas_Object_Ptr is access Canvas_Object_Type'Class;

  type Image_Internals;
  type Image_Ptr is access all Image_Internals;

  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  Every canvas contains one of these protected records to synchronise
  --  drawing operations with accesses from the message loop task. The
  --  canvas stores the objects which are drawn on it as a linked list
  --  of drawing objects (defined in GtkJewl.Objects).
  --
  --  The private data of a Canvas_Monitor is as follows:
  --
  --  First_Object : pointer to the first object in the drawing list
  --  Last_Object  : pointer to the last object in the drawing list
  --  Save_Pointer : pointer to the saved position in the drawing list
  --  BG_Brush     : the brush used to paint the background of the canvas
  --  Start_X      : the X position where the mouse button was pressed
  --  Start_Y      : the Y position where the mouse button was pressed
  --  End_X        : the most recent X position while the mouse is down
  --  End_Y        : the most recent Y position while the mouse is down
  --  Button       : true if the mouse is down
  --  Moved        : true is the mouse has moved while the button is down
  --  Keycode      : the key that was pressed (or NUL if none)
  --
  --  The operations on a Canvas_Monitor are as follows:
  --
  --  Clear        : delete all objects in the drawing list
  --  Save         ; save a pointer to the current end of the drawing list
  --  Restore      : truncate the drawing list to a previously saved point
  --  Draw         : draw all objects in the drawing list (called from the
  --                 message loop task)
  --  Add          : add an object to the end of the drawing list
  --  Set_Bg_Color : set the background color
  --  Get_Bg_Color : get the background color
  --  Set_Start    : set the start position when the mouse button is pressed
  --                 (called from the message loop task)
  --  Get_Start    : get the start position when the mouse button is pressed
  --  Set_End      : set the current mouse position and the mouse-move flag
  --                 (called from the message loop task)
  --  Get_End      : get the current mouse position
  --  Set_Button   : set the mouse button state (called from the message loop
  --                 task)
  --  Mouse_Down   : return the mouse button state
  --  Mouse_Moved  : return the mouse-move flag and reset it
  --  Set_Key      : set the keycode for a key press
  --  Get_Key      : get and reset the current keycode
  --
  ----------------------------------------------------------------------------

  protected type Canvas_Monitor is
    procedure Clear;
    procedure Save;
    procedure Restore;
    
    procedure Draw    (Buffer  : in Gtk_Double_Buffer;
                       Pctx    : in Paintcontext;
                       No_List : in Boolean :=False);
    procedure Add        (Object : in Canvas_Object_Ptr);
    
    procedure Set_Bg_Color  (Color : in Colour_Type);
    function  Get_Bg_Color  return Colour_Type; 
    
    procedure Set_Start  (X, Y : in  Integer);
    procedure Get_Start  (X, Y : out Integer);
    procedure Set_End    (X, Y : in  Integer);
    procedure Get_End    (X, Y : out Integer);
    procedure Set_Button (B : in  Boolean);
    function  Mouse_Down  return Boolean;
    function  Mouse_Moved return Boolean;
    
    procedure Set_Key    (C : in  Character);
    procedure Get_Key    (C : out Character);
    --procedure Set_Action (code : in Integer);
    --procedure Set_Keypress (Code : in Integer);
    
    --function Get_Action return Integer;
    --function Get_Keypress return Integer;
  private
    First_Object : Canvas_Object_Ptr;
    Last_Object  : Canvas_Object_Ptr;
    Save_Pointer : Canvas_Object_Ptr;
    
    Bg_Color     : Colour_Type   :=   (255,255,255);
    Context      : Paintcontext;
    
    Start_X      : Integer   := 0;
    Start_Y      : Integer   := 0;
    End_X        : Integer   := 0;
    End_Y        : Integer   := 0;
    Button       : Boolean   := False;
    Moved        : Boolean   := False;
    
    Keycode      : Character := Ascii.Nul;
    --Action       : Integer   := -1;
    --Keypress     : Integer   := -1;
  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 C A N V A S _ O B J E C T _ T Y P E
  --
  --  Canvas_Object_Type is the base type from which all drawing objects
  --  are derived, and Canvas_Object_Ptr is a class-wide pointer. Each
  --  object contains a pointer so that a singly-linked list of objects
  --  can be constructed. Every object must define a Draw procedure which
  --  draws the object on a window (which is identified by a handle to a
  --  Windows display context).
  --
  ----------------------------------------------------------------------------

  type Canvas_Object_Type is abstract tagged
    record
     Next : Canvas_Object_Ptr;
    end record;

  procedure Draw (Object : in out Canvas_Object_Type;
     Window : in out PaintContext) is abstract;

  ----------------------------------------------------------------------------
  --
  --  Text_Type: an object type containing a text string
  --
  type Text_Type (Length : Natural) is new Canvas_Object_Type with
    record
      Text     : String (1..Length);
      From, To : GtkJewl.Point_Type;
      Align    : Integer;
    end record;

  procedure Draw (Object : in out Text_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  --  Line_Type: an object type which defines a line by a pair of endpoints.
  --
  type Line_Type is new Canvas_Object_Type with
    record
      From, To : GtkJewl.Point_Type;
    end record;

  procedure Draw (Object : in out Line_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  --  Rectangle_Type: an object type which defines a rectangle as a pair
  --                  of coordinates for two opposite corners.
  --
  type Rectangle_Type is new Canvas_Object_Type with
    record
      From, To : GtkJewl.Point_Type;
    end record;

  procedure Draw (Object : in out Rectangle_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Rounded_Rectangle_Type: an object type which defines a rectangle with
  --                         rounded corners using a pair of coordinates
  --                         for two opposite corners and a point to define
  --                         the size of the arc used to round the corners.
  --
  type Rounded_Rectangle_Type is new Canvas_Object_Type with
    record
      From, To, Corner : Point_Type;
    end record;

  procedure Draw (Object : in out Rounded_Rectangle_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Ellipse_Type: an object type which defines an ellipse or circle using
  --               a pair of coordinates for two opposite corners of the
  --               bounding rectangle.
  --
  type Ellipse_Type is new Canvas_Object_Type with
    record
      From, To : GtkJewl.Point_Type;
    end record;

  procedure Draw (Object : in out Ellipse_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Polyline_Type: an object type which defines an open figure as a
  --                sequence of lines.
  --
  type Polyline_Type (Count : Positive) is new Canvas_Object_Type with
    record
      Points : Gdk_Points_Array(1 .. Count);
    end record;

  procedure Draw (Object : in out PolyLine_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Polygon_Type: an object type which defines an open figure as a
  --               sequence of lines.
  --
  type Polygon_Type (Count : Positive) is new Canvas_Object_Type with
    record
     Points : Gdk_Points_Array(1 .. Count);
    end record;

  procedure Draw (Object : in out Polygon_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Handle_Type: an object type which encapsulates a handle to a Windows
  --              GDI object (e.g. a pen or a brush).
  --
  type Handle_Type is new Canvas_Object_Type with
    record
     Pen    : Boolean;
     PenGC  : Gdk.Gc.Gdk_Gc:=Null_GC;
     Fill   : Boolean;
     FillGC : Gdk.Gc.Gdk_Gc:=Null_GC;
     Font   : Gdk.font.Gdk_Font:=Null_Font;
    end record;

  procedure Draw (Object : in out Handle_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  -- Bitmap_Type: an object type representing a bitmap image.
  --
  type Bitmap_Type is new Canvas_Object_Type with
    record
      From   : GtkJewl.Point_Type;
      Width  : Natural;
      Height : Natural;
      Bitmap : GtkJewl.Controlled_Type;
    end record;

  procedure Draw (Object : in out Bitmap_Type;
                  Window : in out PaintContext);

  ----------------------------------------------------------------------------
  --
  --                   C O N T R O L L E D   T Y P E S
  --
  --  Each type below is a reference counted type which contains a handle to
  --  a Windows GDI object. The handle will be deleted automatically when the
  --  last object which refers to it is destroyed.
  --
  ----------------------------------------------------------------------------
  --
  --  Image_Internals: used to store bitmaps in drawings
  --
  type Image_Internals is new Reference_Counted_Type with
    record
      Img    : Gdk_Pixbuf;      
      Width  : Natural;
      Height : Natural;
    end record;
    
  procedure Cleanup (Object : in out Image_Internals);

end GtkJewl.Canvas_Implementation;
