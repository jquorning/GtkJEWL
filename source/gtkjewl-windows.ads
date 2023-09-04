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

with GtkJewl.Types; use GtkJewl.Types;
with GtkJewl.Double_Buffer; use GtkJewl.Double_Buffer;

with System;

with gtk;           use gtk;
with Gtk.Box;       use Gtk.Box;
with Gtk.handle_box;use Gtk.Handle_Box;
with Gtk.Frame;     use Gtk.Frame;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Bar;  use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_item;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Fixed;     use Gtk.Fixed;
with Gtk.Window;    use Gtk.Window;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Button;    use Gtk.Button;
with Gtk.Accel_Group;use Gtk.Accel_Group;
with Gtk.Editable;   use Gtk.Editable;
with Gtk.Drawing_Area;use Gtk.Drawing_Area;
with gdk;           use gdk;
with Gdk.Event;     use Gdk.Event;
with Gdk.Gc;         use Gdk.Gc;
with gdk.drawable;  use gdk.drawable;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Gtk.Text_Buffer;use Gtk.Text_Buffer;
with gtk.item;      use gtk.item;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with gtk.progress_bar; use gtk.progress_bar;

generic                          -- This is a generic package which must be
   type Command_Type is (<>);    -- instantiated with a list of possible
                                 -- command values to be generated whenever
package GtkJewl.Windows is       -- a window is closed, a button is clicked
                                 -- or a menu item is selected.
    
   --Procedures for resize and change position of Frame's
   package Window_Address_Return_Callback is new Gtk.Handlers.User_Return_Callback
        (Gtk.window.Gtk_Window_Record,Boolean, System.Address);
   function On_Frame_Configure_Event(
     Widget        : access Gtk_Window_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     WindowPtr     : System.Address)
     return Boolean;
     
   -- Procedures to control the change of state in MenuItem
   package MenuItem_Address_Callback is new Gtk.Handlers.User_Callback
        (Gtk.widget.Gtk_Widget_Record, System.Address);
   procedure On_Change_State_Event(
     Widget        : access Gtk_Widget_Record'Class;
     WindowPtr     : System.Address);

   --Procedures to capture the event in Menuitem
   package Menuitem_Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean,System.Address);      

  function On_Menuitem_Button_Release_Event(
     Widget        : access Gtk_Widget_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     WindowPtr     : System.Address)
     return Boolean;

    --Procedures to capture the focused window
  package Window_GtkWindow_Return_Callback is new Gtk.Handlers.User_Return_Callback
      (Gtk.Window.Gtk_Window_Record, Boolean, Gtk_Window);  
  
  function On_Focus_In_Event(
     Window        : access Gtk_Window_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     FocusInWin    : GTk_Window)
     return Boolean;              

    --Procedure to close Frame,  pero sin destruirlo, but without destroying it    
    package Window_Cmd_Return_Callback is new Gtk.Handlers.User_Return_Callback
        (Gtk.Window.Gtk_Window_Record, Boolean, System.Address);  
   
   function On_Window_Delete_Event (Object : access Gtk_Window_Record'Class; Window: System.Address) return Boolean;
     
   --Procedure to send a command to handler
    package Widget_Cmd_Callback is new Gtk.Handlers.User_Callback(
        Widget_Type => gtk.widget.Gtk_Widget_Record, User_Type=>System.Address);

   procedure Send_Event (Object : access Gtk_Widget_Record'Class; Window: System.Address);

   --Procedure for Memo, that it captures when changes are made
   package TextBuf_Memo_Callback is new Gtk.Handlers.User_Callback(
        Widget_Type => gtk.text_buffer.Gtk_Text_Buffer_Record, User_Type=>System.Address);

   procedure TextBuff_Event (Object : access Gtk_Text_Buffer_Record'Class; Param: System.Address);

   --Procedure to capture events in canvas
   package Canvas_Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Drawing_Area_Record, Boolean,System.Address);      

  function On_Canvas_Configure_Event(
     Buffer        : access Gtk_Drawing_Area_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     CanvasPtr     : System.Address)
     return Boolean;
  function On_Canvas_Key_Press_Event(
     Buffer        : access Gtk_Drawing_Area_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     CanvasPtr     : System.Address)
     return Boolean;
  function On_Canvas_Mouse_Press_Event(
     Buffer        : access Gtk_Drawing_Area_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     CanvasPtr     : System.Address)
   return Boolean;  
  function On_Canvas_Mouse_Release_Event(
     Buffer        : access Gtk_Drawing_Area_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     CanvasPtr     : System.Address)
   return Boolean;  
  function On_Canvas_Mouse_Motion_Event(
     Buffer        : access Gtk_Drawing_Area_Record'Class;
     Event         : Gdk.Event.Gdk_Event;
     CanvasPtr     : System.Address)
   return Boolean;  

  --Package to respond to the event "destroy" I engage in a  selection dialog without it is destroyed
  package Dialog_Bool_Return_Callback is new Gtk.Handlers.User_Return_Callback(
      Widget_type      => Gtk_Widget_Record,
      Return_Type   => Boolean,
      User_Type   => Boolean
      );
  function Dialog_Delete_Event (Object : access Gtk_Widget_Record'Class;OK_PRESS: Boolean) return Boolean;

  --Package to respond to the button events in selection dialog
  package Dialog_Sel_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type=>Gtk_Button_Record,User_Type=>Boolean);
  procedure Dialog_Button_Event (Object : access Gtk_Button_Record'Class;OK_PRESS : Boolean);


  --Package to respond editbox change
  package Editbox_Changed_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type=>Gtk_Editable_Record,User_Type=>System.Address);
  procedure Editbox_Change(Object : access Gtk_Editable_Record'Class;Edit : System.Address);

  ----------------------------------------------------------------------------
  --  Variables
  ----------------------------------------------------------------------------
  Frame_Width_Constant: Natural :=8;
  Frame_Height_Constant: Natural :=27;
  Frame_Border_Constant: Natural :=Frame_Width_Constant/2;
  Frame_Title_Constant: Natural :=Frame_Height_Constant - (Frame_Width_Constant/2);
  
  Frame_Width_Min : Natural := 104;
  Frame_Height_Min: Natural :=  Frame_Height_Constant + 1;

  Menu_Height_Constant: Natural :=Frame_Width_Constant;
  
  --NoFixed_height_Total: Natural := 0;

  Combobox_Height_Constant: constant Natural :=30;



  ----------------------------------------------------------------------------
  --  Miscellaneous operations
  ----------------------------------------------------------------------------

  procedure Show_Error   (Text  : in String;   -- an error message
                          Title : in String := "Error");
  function  Show_Query   (Text  : in String;   -- a yes/no query
                          Title : in String := "Query")
                                return Boolean;
  procedure Show_Message (Text  : in String;   -- an information message
                          Title : in String := "Message");

  procedure Show_Warning (Text  : in String;   -- an warning message
                          Title : in String := "Warning");

  procedure Play_Sound   (Sound : in String);  -- play a sound file  (functionality lose, only for back compatibility)

  function  Screen_Width  return Natural;      -- width of display screen
  function  Screen_Height return Natural;      -- height of display screen

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  Except for Alignment_Type, these types are defined in the top-level
  --  package GtkJewl and are renamed here for convenience:
  --
  --  Alignment_Type  : used to specify text alignment with a window.
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and bold
  --                    and italic style options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Alignment_Type is (Left, Centre, Right);
  subtype Angle_Type     is GtkJewl.Angle_Type;
  subtype Colour_Range   is GtkJewl.Colour_Range;
  subtype Colour_Type    is GtkJewl.Colour_Type;
  subtype Font_Type      is GtkJewl.Font_Type;
  type    Image_Type     is private;
  subtype Point_Type     is GtkJewl.Point_Type;
  subtype Point_List     is GtkJewl.Point_List;
  type Range_Orientation is 
     (Range_L_To_R, Range_R_To_L, Range_B_To_T, Range_T_To_B);
  pragma Convention (C, Range_Orientation);

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype  Color_Range is Colour_Range;
  subtype  Color_Type  is Colour_Type;
  function Center      return Alignment_Type renames Centre;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  These are renamings of the operations defined in the parent package
  --  provided for convenience.
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
  --  Image operations:
  --    Image    : Load an image from a bitmap file.
  --    Valid    : Test if an image is valid.
  --    Width    : Get the width of an image.
  --    Height   : Get the height of an image.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : GtkJewl.Colour_Type) return  GtkJewl.Colour_Type
                                                renames GtkJewl.Light;
  function Dark     (Colour : GtkJewl.Colour_Type) return  GtkJewl.Colour_Type
                                                renames GtkJewl.Dark;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean := False;
                     Italic : Boolean := False) return  GtkJewl.Font_Type
                                                renames GtkJewl.Font;
  function Name     (Font   : Font_Type)        return String
                                                renames GtkJewl.Name;
  function Size     (Font   : Font_Type)        return Natural
                                                renames GtkJewl.Size;
  function Bold     (Font   : Font_Type)        return Boolean
                                                renames GtkJewl.Bold;
  function Italic   (Font   : Font_Type)        return Boolean
                                                renames GtkJewl.Italic;

  function Image    (Name   : String)           return Image_Type;
  function Valid    (Image  : Image_Type)       return Boolean;
  function Width    (Image  : Image_Type)       return Natural;
  function Height   (Image  : Image_Type)       return Natural;

  function Endpoint (From   : GtkJewl.Point_Type;
                     Length : Positive;
                     Angle  : GtkJewl.Angle_Type)  return  GtkJewl.Point_Type
                                                renames GtkJewl.Endpoint;
  function Inside   (Point  : GtkJewl.Point_Type;
                     From   : GtkJewl.Point_Type;
                     To     : GtkJewl.Point_Type)  return  Boolean
                                                renames GtkJewl.Inside;
  function "+"      (P1, P2 : Point_Type)       return Point_Type
                                                renames GtkJewl."+";
  function "-"      (P1, P2 : Point_Type)       return Point_Type
                                                renames GtkJewl."-";

  ----------------------------------------------------------------------------
  --
  --             S U P P O R T   T Y P E   C O N S T A N T S
  --
  --  Angles  : North, South, East and West
  --  Colours : Black, White, Red, Green, Blue, etc.
  --  Fonts   : Default_Font (the default font for top-level windows) and
  --            Parent_Font (the same font as a window's parent uses)
  --
  ----------------------------------------------------------------------------

  North        : constant Angle_Type  :=   0;
  South        : constant Angle_Type  := 180;
  East         : constant Angle_Type  :=  90;
  West         : constant Angle_Type  := 270;

  Black        : constant Colour_Type := (  0,  0,  0);
  White        : constant Colour_Type := (255,255,255);
  Red          : constant Colour_Type := (255,  0,  0);
  Green        : constant Colour_Type := (  0,255,  0);
  Blue         : constant Colour_Type := (  0,  0,255);
  Gray         : constant Colour_Type := (128,128,128);
  Yellow       : constant Colour_Type := (255,255,  0);
  Cyan         : constant Colour_Type := (  0,255,255);
  Magenta      : constant Colour_Type := (255,  0,255);

  Default_Color : constant Colour_Type   := White;
  Default_Font : constant Font_Type   := Font("Sans",9);
  Parent_Font  : constant Font_Type   := Font("",1);

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ T Y P E
  --
  --  An abstract class providing basic behaviour which all windows share.
  --
  ----------------------------------------------------------------------------

  type Window_Type is abstract tagged private;

  Invalid_Window : exception;   -- raised if an attempt is made to use an
                                -- invalid (non-open) window

  ----------------------------------------------------------------------------
  --
  --  Window operations (common to all windows):
  --
  --  Show       (Window,   -- make the window visible or invisible depending
  --              Visible)  -- on the value of Visible (default: True).
  --  Hide       (Window)   -- make the window invisible.
  --  Focus      (Window)   -- give the window the input focus.
  --  Visible    (Window)   -- return True if the window is visible.
  --  Get_Origin (Window)   -- get the origin (top left point) of the
  --                        -- specified window.
  --  Get_Width  (Window)   -- get the width of the specified window.
  --  Get_Height (Window)   -- get the height of the specified window.
  --  Set_Origin (Window,   -- set the origin (top left point) of the
  --              Origin)   -- specified window to this value.
  --  Set_Size   (Window,   -- set the size of the specified window
  --              Width,    -- to this width (optional)
  --              Height)   -- and this height (optional).
  --  Get_Font   (Window)   -- get the font for the specified window.
  --  Set_Font   (Window,   -- set the font for the specified window
  --              Font)     -- to this one.
  --
  --  Set_Action  (Window,Cmd)   -- Asign action to window
  --  Disable_Command  (Window)  -- Delete action for window
  --  Have_Action  (Window)      -- Test if have action enabled
  ----------------------------------------------------------------------------

  procedure Show       (Window  : in Window_Type;
                        Visible : in Boolean := True);
  procedure Hide       (Window  : in Window_Type);
  procedure Focus      (Window  : in Window_Type);
  function  Visible    (Window  : Window_Type) return Boolean;
  function  Get_Origin (Window  : Window_Type) return Point_Type;
  function  Get_Width  (Window  : Window_Type) return Natural;
  function  Get_Height (Window  : Window_Type) return Natural;
  procedure Set_Origin (Window  : in Window_Type;
                        Origin  : in Point_Type);
  procedure Set_Size   (Window  : in Window_Type;
                        Width   : in Natural := 0;
                        Height  : in Natural := 0);
  function  Get_Font   (Window  : Window_Type) return Font_Type;
  procedure Set_Font   (Window  : in Window_Type;
                        Font    : in Font_Type);
     
  procedure Set_Action(Window : in Window_Type; Command : in Command_Type);
  procedure Disable_Action(Window : in Window_Type);
  function Have_Action(Window : in Window_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --                  W I N D O W   S U B C L A S S E S
  --
  --  The primary window subclasses are containers and controls.  They
  --  share the behaviour common to all windows (above) and provide extra
  --  behaviour as well.
  --
  --  Container_Type : the abstract base type for all containers.
  --  Control_Type   : the abstract base type for all controls.
  --
  ----------------------------------------------------------------------------

  type Container_Type is abstract new Window_Type with private;
  type Control_Type   is abstract new Window_Type with private;

  function  Get_Length (Container : Container_Type) return Natural;
  function  Get_Text   (Container : Container_Type) return String;
  procedure Get_Text   (Container : in  Container_Type;
                        Text      : out String;
                        Length    : out Natural);
  procedure Set_Text   (Container : in  Container_Type;
                        Text      : in  String);

  ----------------------------------------------------------------------------
  --
  --                         C O N T A I N E R S
  --
  --  Containers are windows which can contain other windows. All windows
  --  except frames and dialogs (see below) must be contained within some
  --  other container window. There are some restrictions on the types of
  --  container that a window can be attached to (for example, a menu item
  --  must be attached to a menu).
  --
  --  Most windows specify an origin, a width and a height whose coordinates
  --  are taken relative to the enclosing container. Positive widths and
  --  heights are absolute values, but zero and negative widths and heights
  --  are interpreted as being relative to the width and height of the
  --  enclosing container (so a width of 0 means the width of the enclosing
  --  container, a height of -10 means 10 pixels less than the height of the
  --  enclosing container).
  --
  --  The types of container windows available are as follows:
  --
  --  Frame_Type  : a main window with a title bar, system menu, minimise
  --                and maximise buttons, and a close button.
  --  Dialog_Type : a top level window which is used for modal interaction,
  --                disabling other windows while the interaction takes
  --                place.
  --  Panel_Type  : a plain window which is used as a container for other
  --                subwindows.
  --  Menu_Type   : a menu which can contain menu items and submenus.
  --
  --  Container operations (are common to all containers):
  --
  --  Get_Length (Container)  -- get the length of the text associated with the
  --                          -- control.
  --  Get_Text   (Container)  -- get the text associated with the control as a
  --                          -- string of indefinite size.
  --  Get_Text   (Container,  -- get the text associated with the control into
  --              Text,       -- this fixed-size string variable
  --              Length)     -- and set this integer variable to the actual
  --                          -- number of characters copied.
  --  Set_Text   (Container,  -- set the text associated with the control.
  --              Text)       -- to the specified new text value.
  --
  ----------------------------------------------------------------------------
  
  type Frame_Type  is new Container_Type with private;
  type Dialog_Type is new Container_Type with private;
  type Panel_Type  is new Container_Type with private;
  type Menu_Type   is new Container_Type with private;

  ----------------------------------------------------------------------------
  --
  --                             F R A M E S
  --
  --  A frame is a top level window with a title bar, system menu, minimise
  --  and maximise buttons, and a close button. Closing a frame generates a
  --  command. Frames are normally visible, but can be hidden if required.
  --  A frame should be used as the main window for an application.
  --
  --  Frame operations:
  --
  --  Frame   (Origin,      -- create a frame at the specified position
  --           Width,       -- with the specified width
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --  Frame   (Width,       -- create a frame with the specified width
  --           Height,      -- and height in pixels, placed randomly,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Close   (Frame)       -- close the frame.
  --  Valid   (Frame)       -- true until the frame is closed.
  --
  --  Frame_Width           -- return the width of the frame border.
  --  Frame_Height          -- return the height of the frame border.
  --
  --  Do_Events             -- execute events pending, for interface update
  --
  --  Next_Command          -- return the next command generated by any
  --                        -- control in any existing frame.
  --  Command_Ready         -- test if there is a command pending
  --
  ----------------------------------------------------------------------------

  function  Frame   (Origin  : Point_Type;
                     Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;
  function  Frame   (Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;

  procedure Close   (Frame   : in out Frame_Type);
  function  Valid   (Frame   : Frame_Type) return Boolean;

  function  Frame_Width   return Natural;
  function  Frame_Height  return Natural;

  procedure Do_Events;
  function  Next_Command  return Command_Type;
  function  Command_Ready return Boolean;

  ----------------------------------------------------------------------------
  --
  --                            D I A L O G S
  --
  --  A dialog is a top level window like a frame, but it only has a close
  --  button on its title bar. Dialogs are intended for user interaction.
  --  When a dialog is executed it becomes visible and all other windows
  --  are disabled. Execution of a dialog continues until a command is
  --  generated by closing the dialog window or by clicking on a button
  --  attached to the dialog. Dialog windows do not have a menu bar.
  --
  --  Dialog operations:
  --
  --  Dialog  (Width,       -- create a dialog with the given width and
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Execute (Dialog)      -- execute a dialog and return the command code
  --                        -- used to exit from it.
  --
  --  Dialog_Width          -- return the width of the dialog border.
  --  Dialog_Height         -- return the height of the dialog border.
  --
  ----------------------------------------------------------------------------

  function Dialog  (Width   : Positive;
                    Height  : Positive;
                    Title   : String;
                    Command : Command_Type;
                    Font    : Font_Type := Default_Font) return Dialog_Type;

  function Execute (Dialog  : in Dialog_Type) return Command_Type;
  
  function Dialog_Width   return Natural;
  function Dialog_Height  return Natural;

  ----------------------------------------------------------------------------
  --
  --                             P A N E L S
  --
  --
  --  Panel operations:
  --
  --  Panel (Parent,        -- create a panel inside a parent container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Title,         -- with this title on the border (default: none)
  --         Font)          -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Panel (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Title  : String := "";
                  Font   : Font_Type := Parent_Font) return Panel_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E N U S
  --
  --  A menu is a pull-down list of items attached to a frame's menu bar.
  --  The items on a menu are either menuitems (which generate a command
  --  when they are selected, submenus (which display another menu when
  --  selected) or separators (horizontal bars used to separate a menu
  --  into subsections.
  --
  --  If the text label for a menu contains the character "&", the character
  --  which follows is underlined and the menu can be activated by pressing
  --  Alt + that character. The character "&" is not displayed.
  --
  --  Menu operations:
  --
  --  Menu (Parent,       -- create a menu attached to a frame or a menu
  --        Text)         -- with the specified text label.
  --
  --  Menu_Height         -- return the height of a menu bar.
  --
  ----------------------------------------------------------------------------

  function Menu  (Parent : Frame_Type'Class;
                  Text   : String) return Menu_Type;
  function Menu  (Parent : Menu_Type'Class;
                  Text   : String) return Menu_Type;

  function Menu_Height   return Natural;

  ----------------------------------------------------------------------------
  --
  --                           C O N T R O L S
  --
  --  Controls are windows for user interaction. They hold values (e.g. a
  --  text string) which can normally be set by the user, as well as being
  --  accessed and altered by the program itself. Some controls (e.g. menu
  --  items) generate command values which can be used to trigger actions
  --  in the program. The following operations are common to all controls:
  --
  --  Enable  (Control,     -- enable or disable the control depending on
  --           Enabled)     -- the value of Enabled (default: True).
  --  Disable (Control)     -- disable the control.
  --  Enabled (Control)     -- test if the control is enabled.
  --
  --  Set_Tooltip (Parent,Control,text, textPriv)     -- set text to tooltip
  --  Tooltip_On (Control)     -- enable tooltip
  --  Tooltip_Off (Control)     -- disable tooltip
  --  Get_Tooltip  (Control,     -- get public text of tooltip
  --  Get_Tooltip_Private (Control)     -- get private text of tooltip
  --
  ----------------------------------------------------------------------------

  procedure Enable  (Control : in Control_Type;
                     Enabled : in Boolean := True);
  procedure Disable (Control : in Control_Type);
  function  Enabled (Control : Control_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --                       T O O L T I P S
  --
  ----------------------------------------------------------------------------
  Procedure Set_Tooltip (Parent  : Control_Type'Class; Control : in Control_Type; Text : in String; PrivateText: in String:="");
  Procedure Tooltip_On  (Control : in Control_Type);
  procedure Tooltip_Off (Control : in Control_Type);
  function  Get_Tooltip (Control : in Control_Type) return String;
  function  Get_Tooltip_Private (Control : in Control_Type) return String;


  ----------------------------------------------------------------------------
  --
  --  The types of control available are as follows:
  --
  --  Menuitem_Type     : controls which can appear on pull-down menus
  --  Text_Control_Type : controls containing with a single-line text string
  --  Multiline_Type    : controls containing multiple text strings
  --  Canvas_Type       : a plain window for drawing arbitrary shapes on
  --
  ----------------------------------------------------------------------------

  type Text_Control_Type  is abstract new Control_Type with private;
  type Multiline_Type     is abstract new Control_Type with private;
  type Canvas_Type        is new Control_Type with private;
  type Range_Control_Type  is abstract new Control_Type with private;


  ----------------------------------------------------------------------------
  --
  --                      T E X T   C O N T R O L S
  --
  --  Text controls include a single-line text string. The following
  --  operations are common to all text controls:
  --
  --  Get_Length (Control)  -- get the length of the text associated with the
  --                        -- control.
  --  Get_Text   (Control)  -- get the text associated with the control as a
  --                        -- string of indefinite size.
  --  Get_Text   (Control,  -- get the text associated with the control into
  --              Text,     -- this fixed-size string variable
  --              Length)   -- and set this integer variable to the actual
  --                        -- number of characters copied.
  --  Set_Text   (Control,  -- set the text associated with the control.
  --              Text)     -- to the specified new text value.
  --
  ----------------------------------------------------------------------------

  function  Get_Length (Control : Text_Control_Type) return Natural;
  function  Get_Text   (Control : Text_Control_Type) return String;
  procedure Get_Text   (Control : in  Text_Control_Type;
                        Text    : out String;
                        Length  : out Natural);
  procedure Set_Text   (Control : in  Text_Control_Type;
                        Text    : in  String);

  ----------------------------------------------------------------------------
  --
  --  The types of text control available are as follows:
  --
  --  Button_Type      : a pushbutton which generates a command code.
  --  Action_Menuitem_Type      : a menuitem which only generates a command code.
  --  Label_Type       : a static non-interactive label.
  --  Editbox_Type     : a single-line edit control for text input.
  --
  --  There is a further subclass of text control, as follows:
  --
  --  Boolean_Control_Type : a text control with an associated Boolean state
  --
  ----------------------------------------------------------------------------

  type Button_Type      is new Text_Control_Type with private;
  type Action_Menuitem_Type      is new Text_Control_Type with private;
  type Label_Type       is new Text_Control_Type with private;
  type Editbox_Type     is new Text_Control_Type with private;
  

  type Boolean_Control_Type is abstract new Text_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                            B U T T O N S
  --
  --  Buttons are rectangular labelled controls which generate a command
  --  code when pressed. "Default" buttons are displayed with a heavier
  --  border and respond when the Enter key is pressed.
  --
  --  Button operations:
  --
  --  Button (Parent,     -- create a button in a parent container, with
  --          Origin,     -- top left coordinates relative to the parent,
  --          Width,      -- with the specified width
  --          Height,     -- and the specified height,
  --          Text,       -- labelled with the specified text,
  --          Command,    -- generating this command when it is pressed,
  --          Default,    -- set up as a "default" button (default: False),
  --          Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Button (Parent    : Container_Type'Class;
                   Origin    : Point_Type;
                   Width     : Integer;
                   Height    : Integer;
                   Text      : String;
                   Command   : Command_Type;
                   Default   : Boolean := False;
                   Font      : Font_Type := Parent_Font;
                   Imagefile : String :="") return Button_Type;
     
  ----------------------------------------------------------------------------
  --
  --                    A C T I O N M E N U I T E M
  --
  --  Action_Menuitem operations:
  --
  --  Action_Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --                   Text,      -- with this text label,
  --                   Command)   -- generating this command code when selected
  function Action_Menuitem ( Parent  : Menu_Type'Class;
                            Text   : String;
                            Command : Command_Type) return Action_Menuitem_Type;

  ----------------------------------------------------------------------------
  --
  --                             L A B E L S
  --
  --  A label is a static text control that can be used to label other
  --  controls. Labels do not respond to user interaction, but their
  --  values can be read and altered by the program in the same way as
  --  any other text control.
  --
  --  Label operations:
  --
  --  Label (Parent,        -- create a label inside a container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Text,          -- labelled with the specified text
  --         Align,         -- aligned left, right or centre (default: Left),
  --         Font,          -- using this font (default: same as Parent).
  --         imageFile)     -- nombre del fichero de imagen a a�adir con la etiqueta.
  --
  ----------------------------------------------------------------------------

  function Label (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Text   : String;
                  Align  : Alignment_Type := Left;
                  Font   : Font_Type := Parent_Font) return Label_Type;

  ----------------------------------------------------------------------------
  --
  --                          E D I T B O X E S
  --
  --  An editbox is a text control containing a single line of text whose
  --  value can be edited by the user.
  --
  --  Editbox operations:
  --
  --  Editbox  (Parent,     -- create an editbox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width
  --            Height,     -- and the specified height, and
  --            Text,       -- initialised with the specified text,
  --            Password,   -- optionally set up as a password editbox,
  --            Font)       -- using this font (default: same as Parent).
  --
  --  Modified (Editbox)    -- test if the editbox has been modified by the
  --                        -- user.
  --
  ----------------------------------------------------------------------------

  function Editbox  (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Height   : Integer;
                     Text     : String    := "";
                     Password : Boolean   := False;
                     Font     : Font_Type := Parent_Font) return Editbox_Type;

  function Modified (Editbox  : Editbox_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --                   B O O L E A N   C O N T R O L S
  --
  --  Boolean controls are text controls which can be toggled between two
  --  states (checked or unchecked). The following operations are common
  --  to all Boolean controls:
  --
  --  Get_State (Control)   -- get the current state of the control
  --  Set_State (Control,   -- set the current state of the control
  --             State)     -- to the specified value
  --
  ----------------------------------------------------------------------------

  function  Get_State (Control : Boolean_Control_Type) return Boolean;
  procedure Set_State (Control : in Boolean_Control_Type;
                       State   : in Boolean);


  ----------------------------------------------------------------------------
  --
  --  The types of Boolean controls available are as follows:
  --
  --  Checkbox_Type    : a checkbox which can be checked or unchecked.
  --  Radiobutton_Type : a radio button which can be checked or unchecked.
  --
  ----------------------------------------------------------------------------

  type Menuitem_Type    is new Boolean_Control_Type with private;
  type Check_Menuitem_Type    is new Boolean_Control_Type with private;
  type Radio_Menuitem_Type    is new Boolean_Control_Type with private;
  type Checkbox_Type    is new Boolean_Control_Type with private;
  type Radiobutton_Type is new Boolean_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                         M E N U   I T E M S
  --
  --  Menu items can only be attached to menus. When a menu is clicked, a
  --  pull-down menu appears which can consist of menu items (which generate
  --  a command when clicked) or further menus.
  --
  --  Each menu and menu item has a text label in which the character "&"
  --  is treated specially. "&" will not be displayed, but the following
  --  character (e.g. 'X') will be underlined to indicate that it can be
  --  selected by pressing Alt + 'X' when the menu is visible.
  --
  --  Menuitem operations:
  --
  --  Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --             Text,      -- with this text label,
  --             Command)   -- generating this command code when selected
  --
  --  Separator (Parent)    -- create a separator (an inactive horizontal
  --                        -- bar) attached to a parent menu
  --
  ----------------------------------------------------------------------------

  function Menuitem  (Parent  : Menu_Type'Class;
                      Text    : String;
                      Command : Command_Type)     return Menuitem_Type;

  function Separator (Parent  : Menu_Type'Class)  return Menuitem_Type;

  ----------------------------------------------------------------------------
  --
  --               C H E C K   M E N U   I T E M S
  --
  --  Check_Menuitem operations:
  --
  --  Ckeck_Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --                   Text,      -- with this text label,
  --                   Command    -- generating this command code when selected
  --                   Checked)   -- is initialy selected or not
  function Check_Menuitem ( Parent  : Menu_Type'Class;
                           Text    : String;
                           Command : Command_Type;
                           Checked : Boolean := False) return Check_Menuitem_Type;

  ----------------------------------------------------------------------------
  --
  --               R A D I O   M E N U   I T E M S
  --
  --  Check_Menuitem operations:
  --
  --  Ckeck_Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --                   Text,      -- with this text label,
  --                   Command    -- generating this command code when selected
  --                   Checked)   -- is initialy selected or not
  function Radio_Menuitem ( Parent  : Menu_Type'Class;
                           Text    : String;
                           Command : Command_Type;
                           Checked : Boolean := False) return Radio_Menuitem_Type;

  ----------------------------------------------------------------------------
  --
  --                         C H E C K B O X E S
  --
  --  A checkbox is a labelled control with a left-aligned box that can be
  --  checked or unchecked. Clicking on a checkbox (or pressing Space when
  --  it is selected) toggles the checkbox between the checked and unchecked
  --  states.
  --
  --  Checkbox operations:
  --
  --  Checkbox  (Parent,    -- create a checkbox in a container, with
  --             Origin,    -- top left coordinates relative to the parent,
  --             Width,     -- with the specified width
  --             Height,    -- and the specified height, and
  --             Text,      -- labelled with the specified text, and
  --             Checked,   -- set to this initial state (default: False),
  --             Font)      -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Checkbox (Parent  : Container_Type'Class;
                     Origin  : Point_Type;
                     Width   : Integer;
                     Height  : Integer;
                     Text    : String;
                     Checked : Boolean := False;
                     Font    : Font_Type := Parent_Font) return Checkbox_Type;

  ----------------------------------------------------------------------------
  --
  --                       R A D I O B U T T O N S
  --
  --  A radiobutton is a Boolean control with a left-aligned box that can be
  --  checked or unchecked. Radiobuttons attached to the same container form
  --  a group. Clicking on an unchecked radiobutton will set the radiobutton
  --  to the checked state and will also uncheck the other radiobuttons which
  --  belong to the same group (i.e. which are attached to the same container).
  --  Unlike a checkbox, a radiobutton cannot be turned off by clicking on it;
  --  you can only uncheck a radiobutton by checking another one in the same
  --  group.
  --
  --  Radiobutton operations:
  --
  --  Radiobutton (Parent,      -- create a radiobutton in a container, with
  --               Origin,      -- top left coordinates relative to the parent,
  --               Width,       -- with the specified width
  --               Height,      -- and the specified height, and
  --               Text,        -- labelled with the specified text, and
  --               Checked,     -- set to this initial state (default: False),
  --               Font)        -- using this font (default: Default_Font).
  --
  ----------------------------------------------------------------------------

  function Radiobutton (Parent  : Container_Type'Class;
                        Origin  : Point_Type;
                        Width   : Integer;
                        Height  : Integer;
                        Text    : String;
                        Checked : Boolean := False;
                        Font    : Font_Type := Parent_Font)
                                                    return Radiobutton_Type;

  ----------------------------------------------------------------------------
  --
  --                 M U L T I L I N E   C O N T R O L S
  --
  --  Multiline controls contain multiple lines of text numbered from 1
  --  upwards.  Individual lines can be accessed by specifying a line
  --  number.  The user can select a particular line by clicking on it
  --  with the mouse or using the keyboard arrow keys when the control
  --  is selected.  Specifying the line number to access as 0 will access
  --  the currently selected line.  If no line is selected, the current
  --  line number will be reported as 0 but its contents can still be
  --  accessed .  A Constraint_Error will be raised if an attempt is made
  --  to access a line beyond the last one.
  --
  --  The following operations are common to all multiline controls, but
  --  their precise effects vary slightly according to the actual type of
  --  the control (see below):
  --
  --  Get_Count   (Control) -- get the number of lines of text in the control.
  --  Get_Line    (Control) -- get the number of the currently selected line.
  --  Get_Length  (Control, -- get the length of the specified line
  --               Line)    -- (default: current line).
  --  Get_Text    (Control, -- get the text of the specified line as a string
  --               Line)    -- of indefinite length (default: current line).
  --
  --  Get_Text    (Control, -- get the text of the control
  --               Line,    -- from the specified line (default: current line)
  --               Text,    -- into a fixed-size string variable, together
  --               Length)  -- with the number of characters transferred.
  --  Set_Text    (Control, -- set the text of the control
  --               Line,    -- at the specified line
  --               Text)    -- to the specified value.
  --
  --  Select_Line (Control, -- select the line at the specified line number
  --               Line)    -- (default: 0, meaning deselect).
  --  Append_Line (Control, -- append a line to the end of the control, where
  --               Text)    -- this is the text to append.
  --  Insert_Line (Control, -- insert a new line above the specified
  --               Line,    -- line number, where
  --               Text)    -- this is the text to insert.
  --  Delete_Line (Control, -- delete the specified line.
  --               Line)
  --  Delete_All  (Control) -- delete all lines.
  --
  ----------------------------------------------------------------------------

  function  Get_Count   (Control : Multiline_Type) return Natural is abstract;
  function  Get_Line    (Control : Multiline_Type) return Natural is abstract;
  function  Get_Length  (Control : Multiline_Type;
                         Line    : Natural := 0)   return Natural is abstract;
  function  Get_Text    (Control : Multiline_Type;
                         Line    : Natural := 0)   return String  is abstract;

  procedure Get_Text    (Control : in  Multiline_Type;
                         Line    : in  Natural := 0;
                         Text    : out String;
                         Length  : out Natural);
  procedure Set_Text    (Control : in  Multiline_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0)              is abstract;

  procedure Select_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Append_Line (Control : in Multiline_Type;
                         Text    : in String)                     is abstract;
  procedure Insert_Line (Control : in Multiline_Type;
                         Text    : in String;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_All  (Control : in Multiline_Type)             is abstract;

  ----------------------------------------------------------------------------
  --
  --  The types of multiline controls available are as follows:
  --
  --  Listbox_Type  : a list of text items in a scrollable window
  --  Combobox_Type : an editbox in combination with a drop-down list box
  --  Memo_Type     : a multi-line text editor
  --
  ----------------------------------------------------------------------------

  type Listbox_Type  is new Multiline_Type with private;
  type Combobox_Type is new Multiline_Type with private;
  type Memo_Type     is new Multiline_Type with private;

  ----------------------------------------------------------------------------
  --
  --                          L I S T B O X E S
  --
  --  A listbox is a list of lines of text (initially empty). The lines are
  --  sorted into ascending order by default, but can be left unsorted if
  --  required. For a sorted list, the position at which a new line is added
  --  will be ignored, with the new line being inserted at the appropriate
  --  position according to its value. When no line has been selected, the
  --  contents of the current line will be reported as an empty string ("").
  --
  --  Listbox operations:
  --
  --  Listbox (Parent,      -- create a listbox inside a container, with
  --           Origin,      -- top left coordinates relative to the parent,
  --           Width,       -- the specified width
  --           Height,      -- and the specified height, using
  --           Font)        -- this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Listbox (Parent : Container_Type'Class;
                    Origin : Point_Type;
                    Width  : Integer;
                    Height : Integer;
                    Font   : Font_Type := Parent_Font) return Listbox_Type;

  ----------------------------------------------------------------------------
  --
  --                         C O M B O B O X E S
  --
  --  A combobox consists of an edit control together with a drop-down list
  --  (effectively a combination of an editbox and a listbox). The currently
  --  selected line is displayed in the edit control, and you can specify
  --  whether the user is able to manually edit this value. If not, only
  --  the values in the drop-down list can be selected.
  --
  --  If the contents of the edit control match one of the values in the list,
  --  the position of the corresponding line in the list is reported as the
  --  number of the currently selected line. Otherwise, the number of the
  --  current line is reported as 0. Accessing the value of the current line
  --  (line 0) will report the current value of the edit control.
  --
  --  Combobox operations:
  --
  --  Combobox (Parent,     -- create a combobox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width, whose value can
  --            Editable,   -- be manually edited (default: True),
  --            Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Combobox (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Editable : Boolean := True;
                     Font     : Font_Type := Parent_Font) return Combobox_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E M O S
  --
  --  A memo is a simple multi-line text editor similar to Windows Notepad.
  --  There are several memo-specific operations in addition to the standard
  --  operations on multi-line controls. The user can select a block of text
  --  spanning multiple lines using the mouse (or by moving the cursor with
  --  the Shift key held down) and there are operations to fetch, replace
  --  and delete the selected text, find the line and column position of
  --  the start and end of the selected text, and get its total length (which
  --  will include one or more additional end-of-line characters if the text
  --  spans more than one line).
  --
  --  Memo operations:
  --
  --  Memo (Parent,           -- create a memo inside a container, with
  --        Origin,           -- top left coordinates relative to the parent,
  --        Width,            -- the specified width
  --        Height)           -- and the specified height
  --        Font)             -- using this font (default: same as Parent).
  --
  --  Get_Column      (Memo)  -- get the column position of the current
  --                          -- selection.
  --  Modified        (Memo)  -- test if the user has modified the memo.
  --  Cut_Selection   (Memo)  -- cut the current selection to the clipboard.
  --  Copy_Selection  (Memo)  -- copy the current selection to the clipboard.
  --  Paste_Selection (Memo)  -- paste the clipboard contents to the memo,
  --                          -- replacing the current selection.
  --  Undo_Change     (Memo)  -- undo the user's last change to the memo.
  --  Show_Selection  (Memo)  -- scroll the memo so that the current position
  --                          -- is visible on the screen.
  --
  ----------------------------------------------------------------------------

  function  Memo (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Font   : Font_Type := Parent_Font) return Memo_Type;

  function  Get_Column      (Memo : Memo_Type) return Natural;
  function  Modified        (Memo : Memo_Type) return Boolean;
  procedure Cut_Selection   (Memo : in Memo_Type);
  procedure Copy_Selection  (Memo : in Memo_Type);
  procedure Paste_Selection (Memo : in Memo_Type);
  procedure Undo_Change     (Memo : in Memo_Type);
  procedure Show_Selection  (Memo : in Memo_Type);

  ----------------------------------------------------------------------------
  --  Most operations on multiline types are implemented in different ways,
  --  so they are overridden here:
  --
  function  Get_Count   (Control : Listbox_Type)  return Natural;
  function  Get_Count   (Control : Combobox_Type) return Natural;
  function  Get_Count   (Control : Memo_Type)     return Natural;

  function  Get_Line    (Control : Listbox_Type)  return Natural;
  function  Get_Line    (Control : Combobox_Type) return Natural;
  function  Get_Line    (Control : Memo_Type)     return Natural;

  function  Get_Length  (Control : Listbox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Combobox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Memo_Type;
                         Line    : Natural := 0)  return Natural;

  function  Get_Text    (Control : Listbox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Combobox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Memo_Type;
                         Line    : Natural := 0)  return String;

  procedure Set_Text    (Control : in  Listbox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Combobox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Memo_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);

  procedure Select_Line (Control : in  Listbox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Combobox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Memo_Type;
                         Line    : in  Natural := 0);

  procedure Append_Line (Control : in Listbox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Combobox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Memo_Type;
                         Text    : in String);

  procedure Insert_Line (Control : in Listbox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Combobox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Memo_Type;
                         Text    : in String;
                         Line    : in Natural := 0);

  procedure Delete_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Memo_Type;
                         Line    : in Natural:= 0);

  procedure Delete_All  (Control : in Listbox_Type);
  procedure Delete_All  (Control : in Combobox_Type);
  procedure Delete_All  (Control : in Memo_Type);

  ----------------------------------------------------------------------------
  --
  --                           C A N V A S E S
  --
  --  A canvas is a blank rectangle for drawing on which can optionally
  --  be set to generate a command code when the mouse is clicked on it.
  --  A canvas has an associated font (the same as the parent window by
  --  default), a background colour (initially white), a pen for drawing
  --  lines (initially black, one pixel wide) and a fill colour used to
  --  colour closed shapes (initially white).
  --
  --  The freedom of expression available with canvases make these the
  --  most complex component of all, with over 20 available operations.
  --  There are operations to draw lines, rectangles (with or without
  --  rounded corners), ellipses, circles, line sequences, polygons and
  --  text. The font, pen size and colour and fill colour can be changed
  --  at any time any will affect all subsequent drawing operations (but
  --  everything drawn previously will be unaffected). Rectangles can be
  --  given rounded corners by specifying a rounding factor, which gives
  --  the X and Y offsets from the corners to the points where the rounded
  --  corner begins.
  --
  --  Anything drawn on a canvas will normally stay there, but the canvas
  --  can be erased or the current state of a drawing can be saved and then
  --  restored later (which provides a basic "undo" facility). For example,
  --  a clock can be drawn by drawing a circle, saving the drawing, and then
  --  drawing the hands. To change the position of the hands, restore the
  --  saved drawing (thus erasing the hands) and then redraw the hands in
  --  the new position. You can only save one copy of the drawing, so if you
  --  save it a second time you will lose the previous saved copy.
  --
  --  A canvas can be set up to generate a command when the mouse button
  --  is pressed inside its borders. There are operations to get the
  --  position at which the button was pressed, to get the current mouse
  --  position, and to test if the mouse button is still down and whether
  --  the mouse has been moved. As long as the button is down, the mouse
  --  position will be tracked even if the mouse is moved outside the canvas.
  --  You can track the mouse visually by saving the drawing when the mouse
  --  is first pressed, then restoring the drawing and drawing a new line
  --  from the initial mouse position to the current one.
  --
  --  Canvas operations:
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to Parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height, using
  --           Font               -- this font (default: same as Parent.
  --           No_List            -- indicate to canvas if the list of draw is saved.
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Font              -- uses this font (default: same as Parent.
  --           No_List            -- indicate to canvas if the list of draw is saved.
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Keypress,          -- this one when a key is pressed, and
  --           Font               -- uses this font (default: same as Parent.
  --           No_List            -- indicate to canvas if the list of draw is saved.
  --
  --  Set_KeyAction  (Window,Cmd) -- Asign keyaction to canvas, if initialized
  --  Disable_KeyAction (Window)  -- Delete action for canvas
  --  Have_KeyAction  (Window)    -- Test if have action enabled
  --
  --
  --  Set_Colour     (Canvas,     -- set the background colour for a canvas
  --                  Colour)     -- using this colour (default: white)
  --
  --  Erase          (Canvas)     -- erase the drawing in a canvas. This will
  --                              -- also erase any saved drawing.
  --  Save           (Canvas)     -- save the current drawing.
  --  Restore        (Canvas)     -- restore a saved drawing, erasing anything
  --                              -- drawn since. If the drawing has never been
  --                              -- saved, or the window has been erased using
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --                              -- Erase, this will do nothing.
  --  Set_Font       (Canvas,     -- set a new font for all subsequent text
  --                  Font)       -- drawn on the canvas.
  --  Set_Pen        (Canvas,     -- set the pen used to draw lines on the
  --                  Colour,     -- canvas to this colour (default: black)
  --                  Width)      -- and thickness (default: 1 pixel).
  --  Set_Fill       (Canvas,     -- set the colour used to fill subsequent
  --                  Colour)     -- closed shapes drawn on the canvas.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Set_Fill       (Canvas)     -- clear the colour used to fill subsequent
  --                              -- closed shapes drawn on the canvas.
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- from this top-left point, where
  --                  Text)       -- this is the text to be drawn.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle between this point
  --                  To,         -- and this one, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle starting at this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  To)         -- to this one.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  Length,     -- for this length
  --                  Angle)      -- at this angle.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Line_List (Canvas,     -- draw lines on the canvas connecting
  --                  Points)     -- the points in this list.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this corner point
  --                  To,         -- to this diagonally-opposite point
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this top-left point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  To)         -- to this point.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  Width,      -- with this width.
  --                  Height)     -- and this height
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Circle    (Canvas,     -- draw a circle on the canvas
  --                  Centre,     -- with this centre point
  --                  Radius)     -- and this radius.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Draw_Polygon   (Canvas,     -- draw a polygon on the canvas
  --                  Points)     -- with vertices at these points.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Image)      -- using this image object.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  To,         -- to this one (stretching the image to fit)
  --                  Image)      -- using this image object.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Image)      -- using this image object.
  --                  Paint       -- Paint indicate if the canvas have to repaint it
  --
  --  Mouse_Down     (Canvas)     -- test if the left mouse button is down.
  --  Mouse_Moved    (Canvas)     -- test if the mouse has been moved.
  --  Start_Point    (Canvas)     -- get the point where the mouse button
  --                              -- was first pressed.
  --  End_Point      (Canvas)     -- get the point where the mouse is now
  --                              -- (or its final position when the left
  --                              -- button was released).
  --  Key_Code       (Canvas)     -- get the character code for the last
  --                              -- key pressed.
  --
  ----------------------------------------------------------------------------

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Font     : Font_Type := Parent_Font;
                   No_List   : Boolean   := False ) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Font     : Font_Type := Parent_Font;
                   No_List   : Boolean   := False ) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Keypress : Command_Type;
                   Font     : Font_Type := Parent_Font;
                   No_List   : Boolean   := False ) return Canvas_Type;

  procedure Set_KeyAction(Canvas : in Canvas_Type; Command : in Command_Type);

  procedure Disable_KeyAction(Canvas : in Canvas_Type);

  function Have_KeyAction(Canvas : in Canvas_Type) return Boolean;

  procedure Set_Colour (Canvas : in Canvas_Type;
                        Colour : in Colour_Type := White; 
                        Paint    : in Boolean := True);

  procedure Erase      (Canvas   : in Canvas_Type; Paint : in Boolean := True);
  procedure Save       (Canvas   : in Canvas_Type);
  procedure Restore    (Canvas   : in Canvas_Type; Paint : in Boolean := True);
  procedure Force_Draw  (Canvas   : in Canvas_Type);

  procedure Set_Font   (Canvas   : in Canvas_Type;
                        Font     : in Font_Type);
  procedure Set_Pen    (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type := Black;
                        Width    : in Natural     := 1);
  procedure Set_Fill   (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type);
  procedure Set_Fill   (Canvas   : in Canvas_Type);

  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Text     : in String;
                        Paint  : in     Boolean := True);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Text     : in String;
                        Align    : in Alignment_Type := Left;
                        Paint  : in     Boolean := True);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Integer;
                        Height   : in Integer;
                        Text     : in String;
                        Align    : in Alignment_Type := Left;
                        Paint  : in     Boolean := True);

  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Paint  : in     Boolean := True);
  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Length   : in Positive;
                        Angle    : in Angle_Type;
                        Paint  : in     Boolean := True);

  procedure Draw_Line_List
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List;
                        Paint  : in     Boolean := True);

  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Rounding : in Point_Type := (0,0);
                        Paint  : in     Boolean := True);
  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive;
                        Rounding : in Point_Type := (0,0);
                        Paint  : in     Boolean := True);

  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Paint  : in     Boolean := True);
  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive;
                        Paint  : in     Boolean := True);
  procedure Draw_Circle
                       (Canvas   : in Canvas_Type;
                        Centre   : in Point_Type;
                        Radius   : in Positive;
                        Paint  : in     Boolean := True);

  procedure Draw_Polygon
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List;
                        Paint  : in     Boolean := True);

  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Image    : in Image_Type;
                        Paint  : in     Boolean := True);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Image    : in Image_Type;
                        Paint  : in     Boolean := True);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Natural;
                        Height   : in Natural;
                        Image    : in Image_Type;
                        Paint  : in     Boolean := True);

  function Start_Point (Canvas   : Canvas_Type) return Point_Type;
  function End_Point   (Canvas   : Canvas_Type) return Point_Type;
  function Mouse_Down  (Canvas   : Canvas_Type) return Boolean;
  function Mouse_Moved (Canvas   : Canvas_Type) return Boolean;
  function Key_Code    (Canvas   : Canvas_Type) return Character;

  ----------------------------------------------------------------------------
  --
  --                     C O M M O N   D I A L O G S
  --
  --  Common dialogs are pre-packaged dialog widgets which are not treated
  --  as normal windows (although they are made to look similar for ease of
  --  use).
  --
  --  Common dialog operations (common to all dialogs):
  --
  --  Execute (Dialog)      -- execute the dialog and return True if the OK
  --                        -- button was used to close the dialog and False
  --                        -- otherwise.
  --
  ----------------------------------------------------------------------------

  type Common_Dialog_Type is abstract tagged private;
  function Execute (Dialog : Common_Dialog_Type) return Boolean;

    ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   S U B C L A S S E S
  --
  --  The available dialog subclasses are colour, font and file dialogs.
  --
  --  Colour_Dialog_Type : a dialog to allow the user to select a colour.
  --  Font_Dialog_Type   : a dialog to allow the user to select a font.
  --  File_Dialog_Type   : a dialog to allow the user to select a file name.
  --
  ----------------------------------------------------------------------------
  type Colour_Dialog_Type is new Common_Dialog_Type with private;
  type Font_Dialog_Type   is new Common_Dialog_Type with private;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with private;
  
  ----------------------------------------------------------------------------
  --
  --                     C O L O U R   D I A L O G S
  --
  --  Colour dialogs allow the user to select or create a colour.
  --
  --  Colour dialog operations:
  --
  --  Colour_Dialog           -- create a colour dialog.
  --  Set_Colour    (Dialog,  -- set the initial colour displayed in the
  --                 Colour)  -- dialog to this colour.
  --  Get_Colour    (Dialog)  -- get the colour contained in the dialog.
  --
  ----------------------------------------------------------------------------

  function  Colour_Dialog return Colour_Dialog_Type;

  procedure Set_Colour    (Dialog : in Colour_Dialog_Type;
                           Colour : in Colour_Type);
  function  Get_Colour    (Dialog : in Colour_Dialog_Type) return Colour_Type;

  ----------------------------------------------------------------------------
  --
  --                       F O N T   D I A L O G S
  --
  --  Font dialogs allow the user to select a font.
  --
  --  Font dialog operations:
  --
  --  Font_Dialog             -- create a font dialog.
  --  Set_Font    (Dialog,    -- set the initial font displayed in the
  --               Font)      -- dialog to this font.
  --  Get_Font    (Dialog)    -- get the font contained in the dialog.
  --
  ----------------------------------------------------------------------------
  function  Font_Dialog return Font_Dialog_Type;
  
  procedure Set_Font    (Dialog : in Font_Dialog_Type;
                         Font   : in Font_Type);
  function  Get_Font    (Dialog : in Font_Dialog_Type) return Font_Type;

  ----------------------------------------------------------------------------
  --
  --                       F I L E   D I A L O G S
  --
  --  File dialogs allow the user to select or enter a file name. This is an
  --  abstract type which is further subclassed below.
  --
  --  File dialog operations (common to all file dialogs):
  --
  --  Set_Name      (Dialog,    -- set the initial file name displayed in the
  --                 Name)      -- dialog to this string.
  --  Get_Name      (Dialog)    -- get the file name contained in the dialog.
  --  Add_Filter    (Dialog,    -- add a file type filter to the dialog
  --                 Text,      -- with this description
  --                 Filter)    -- to match this wildcard specification.
  --  Set_Directory (Dialog,    -- set the initial directory for the dialog
  --                 Name)      -- to this directory.
  --
  ----------------------------------------------------------------------------

  procedure Set_Name      (Dialog : in File_Dialog_Type;
                           Name   : in String);
  function  Get_Name      (Dialog : in File_Dialog_Type) return String;

  procedure Add_Filter    (Dialog : in File_Dialog_Type;
                           Text   : in String;
                           Filter : in String);

  procedure Set_Directory (Dialog : in File_Dialog_Type;
                           Name   : in String);

  ----------------------------------------------------------------------------
  --
  --                       O P E N   D I A L O G S
  --
  --  Open dialogs allow the user to select or enter a file name for use as
  --  an input file. The file name selected must be the name of an existing
  --  file.
  --
  --  Open dialog operations:
  --
  --  Open_Dialog (Title)     -- create an open file dialog with this title.
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Type is new File_Dialog_Type with private;

  function Open_Dialog (Title  : String) return Open_Dialog_Type;

  ----------------------------------------------------------------------------
  --
  --                       S A V E   D I A L O G S
  --
  --  Save dialogs allow the user to select or enter a file name for use as
  --  an output file. If the Create parameter to the constructor function
  --  below is True (as it is by default) and an existing file is selected,
  --  the user will be asked if the file should be overwritten. If it is
  --  False and the file does not exist, the user will be asked if it should
  --  be created.
  --
  --  Save dialog operations:
  --
  --  Save_Dialog (Title,     -- create a save file dialog with this title
  --               Create)    -- which will prompt the user as described above.
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Type is new File_Dialog_Type with private;

  function Save_Dialog (Title  : String;
                        Create : Boolean := True) return Save_Dialog_Type;


  ----------------------------------------------------------------------------
  --
  --                       R A N G E   T Y P E
  --
  --   Elements that graphically delimmitan and show a rank of values
  --
  --  Range operations (common to all range):
  --
  --   Set_Value(Range, val)   --asign value to range control an update
  --
  --   Get_Value(Range)        --get value to range control
  --
  --   Set_Min(Range, val)     --asign min value to range control an update
  --
  --   Get_Min(Range)          --get min value from range control
  --
  --   Set_Max(Range, val)     --asign max value to range control an update
  --
  --   Get_Max(Range)          --get max value from range control
  --
  ----------------------------------------------------------------------------

  procedure Set_Value ( R : Range_Control_Type; Val : in Float);
  procedure Set_Min ( R : Range_Control_Type; Min : in Float);
  procedure Set_Max ( R : Range_Control_Type; Max : in Float);

  function Get_Value ( R : Range_Control_Type) return Float;
  function Get_Min ( R : Range_Control_Type) return Float;
  function Get_Max ( R : Range_Control_Type) return Float;


  ----------------------------------------------------------------------------
  --
  --                       P R O G R E S S B A R
  --
  type Progressbar_Type is new Range_Control_Type with private;
  
  function Progressbar 
                  (Parent      : Container_Type'Class;
                   Origin      : Point_Type;
                   Width       : Integer;
                   Height      : Integer;
                   Min         : Float; 
                   Max         : Float; 
                   Orientation : Range_Orientation) return Progressbar_Type;
  

  ----------------------------------------------------------------------------
  --
  --                       S C A L E
  --
  type Scale_Type is new Range_Control_Type with private;
  
  function Scale 
                  (Parent      : Container_Type'Class;
                   Origin      : Point_Type;
                   Width       : Integer;
                   Height      : Integer;
                   Min         : Float; 
                   Max         : Float; 
                   Step        : Float; 
                   Ndigits     : Natural:=0; 
                   Orientation : Range_Orientation:=Range_L_To_R;
                   Font        : Font_Type := Parent_Font) return Scale_Type;


  ----------------------------------------------------------------------------
  --
  --                       S C R O L L B A R
  --
  type Scrollbar_Type is new Range_Control_Type with private;

  function Scrollbar 
                  (Parent      : Container_Type'Class;
                   Origin      : Point_Type;
                   Width       : Integer;
                   Height      : Integer;
                   Min         : Float; 
                   Max         : Float; 
                   Step        : Float; 
                   Orientation : Range_Orientation:=Range_L_To_R) return Scrollbar_Type;


  ----------------------------------------------------------------------------
  --
  --                       S P I N B U T T O N
  --
  type Spinbutton_Type is new Range_Control_Type with private;
  
  function Spinbutton
                  (Parent      : Container_Type'Class;
                   Origin      : Point_Type;
                   Width       : Integer;
                   Height      : Integer;
                   Min         : Float; 
                   Max         : Float; 
                   Step        : Float; 
                   Ndigits     : Natural:=0;
                   Font        : Font_Type := Parent_Font) return Spinbutton_Type;


  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor...
  ----------------------------------------------------------------------------

  procedure Set_Color (Canvas : in Canvas_Type;
                       Colour : in Colour_Type := White;
                       Paint  : in Boolean := True) renames Set_Colour;

  subtype   Color_Dialog_Type is Colour_Dialog_Type;

  function  Color_Dialog  return Colour_Dialog_Type renames Colour_Dialog;
  procedure Set_Color    (Dialog : in Colour_Dialog_Type;
                          Colour : in Colour_Type) renames Set_Colour;
  function  Get_Color    (Dialog : in Colour_Dialog_Type) return Colour_Type
                                                   renames Get_Colour;

     
private       -- and deliberately uninformative!   
  type Window_Type is abstract tagged 
     record
        Internals : GtkJewl.Controlled_Type;   -- see package GtkJewl
     end record;

  type Container_Type is abstract new Window_Type with null record;
  type Control_Type         is abstract new Window_Type with null record;

  type Frame_Type           is new Container_Type with null record;
  type Dialog_Type          is new Container_Type with null record;
  type Panel_Type           is new Container_Type with null record;
  type Menu_Type            is new Container_Type with null record;

  type Text_Control_Type    is abstract new Control_Type with null record;
  type Multiline_Type       is abstract new Control_Type with null record;
  type Canvas_Type          is new Control_Type with null record;
  type Range_Control_Type    is abstract new Control_Type with null record;

  type Button_Type          is new Text_Control_Type with null record;
  type Action_Menuitem_Type        is new Text_Control_Type with null record;
  type Label_Type           is new Text_Control_Type with null record;
  type Editbox_Type         is new Text_Control_Type with null record;

  type Boolean_Control_Type is abstract new Text_Control_Type with null record;

  type Menuitem_Type        is new Boolean_Control_Type with null record;
  type Check_Menuitem_Type   is new Boolean_Control_Type with null record;
  type Radio_Menuitem_Type   is new Boolean_Control_Type with null record;
  type Checkbox_Type        is new Boolean_Control_Type with null record;
  type Radiobutton_Type     is new Boolean_Control_Type with null record;

  type Listbox_Type         is new Multiline_Type with null record;
  type Combobox_Type        is new Multiline_Type with null record;
  type Memo_Type            is new Multiline_Type with null record;

  type Progressbar_Type     is new Range_Control_Type with null record;
  type Scale_Type           is new Range_Control_Type with null record;
  type Scrollbar_Type       is new Range_Control_Type with null record;
  type Spinbutton_Type      is new Range_Control_Type with null record;

  type Common_Dialog_Type is abstract tagged
    record
      Internals : GtkJewl.Controlled_Type;   -- see package GtkJewl
    end record;

  type Colour_Dialog_Type is new Common_Dialog_Type with null record;
  type Font_Dialog_Type   is new Common_Dialog_Type with null record;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with null record;

  type Open_Dialog_Type   is new File_Dialog_Type with null record;
  type Save_Dialog_Type   is new File_Dialog_Type with null record;

  type Image_Type is
    record
     Internals : GtkJewl.Controlled_Type;   -- see package GtkJewl
    end record;

end GtkJewl.Windows;
