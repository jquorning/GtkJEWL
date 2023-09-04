------------------------------------------------------------------------------
--
--      G T K J E W L . W I N D O W S _ I M P L E M E N T A  T I O N
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

with GtkJewl.Canvas_Implementation;  use GtkJewl.Canvas_Implementation;
with GtkJewl.Double_Buffer; use GtkJewl.Double_Buffer;


with gtk.window;   use gtk.window;

with Ada.Exceptions;
with System;                   
use GtkJewl;

with GtkJewl.Types;    use GtkJewl.Types;
with gtk.handlers;  use gtk.handlers;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Editable;  use Gtk.Editable;
with Gtk.Box;       use Gtk.Box;
with Gtk.Menu_Bar;  use Gtk.Menu_Bar;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Status_Bar;use Gtk.Status_Bar;
with Gtk.Frame;     use Gtk.Frame;
with Gtk.Accel_Group;use Gtk.Accel_Group;
with gtk.fixed;     use gtk.fixed;
use Gtk.Widget.Widget_List;

with Gdk.Gc;   use Gdk.Gc;
with Gdk.Color;use Gdk.Color;
with Gdk.Font; use Gdk.Font;
with gtk.drawing_area; use gtk.drawing_area;
with Gdk.Event;use Gdk.Event;
with Gdk.Window;use Gdk.Window;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Tooltips;   use Gtk.Tooltips;
with Gtk.text_buffer; use Gtk.text_buffer;


private package GtkJewl.Window_Implementation is

  ----------------------------------------------------------------------------
  --  Tipos de objetos
  ----------------------------------------------------------------------------
  type Window_Internals;
  type Window_Ptr is access all Window_Internals'Class;

  type Container_Internals;
  type Container_Ptr is access all Container_Internals;

  ----------------------------------------------------------------------------
  --
  --                   W I N D O W _ I N T E R N A L S
  --
  --  Window_Internals (or a type derived from Window_Internals) is what
  --  the Controlled_Type object in every Window_Type object actually
  --  points to. It contains the following fields:
  --
  --  Handle  : the native Windows handle for the window
  --  Parent  : the parent window (null for top-level windows)
  --  Next    : link to the next sibling of this window
  --  First   : link to the first child window of this window
  --  Last    : link to the last child window
  --  Action  : the command code associated with window (-1 if none)
  --  Font    : font handle for the window's font
  --  Top     : position of top of window (negative if relative to parent)
  --  Left    : position of top of window (negative if relative to parent)
  --  Height  : position of top of window (non-positive if relative to parent)
  --  Width   : position of top of window (non-positive if relative to parent)
  --  WndProc : old window procedure for subclassed windows
  --
  --  The Cleanup procedure is called automatically when a Window_Internals
  --  object is deleted and should not be called directly.
  --
  ----------------------------------------------------------------------------

  type Window_Internals is new Reference_Counted_Type with
     record
      Tipo        : Object_Type     :=Type_None;  
      Widget      : Gtk_Widget      :=null;  --Basic type of the object
      Label_Widget : Gtk_Widget     :=null;  --Object Label associated,  if exist
      Changed     : Boolean :=False;         --Boolean that indicate change from the last consults
      No_List     : Boolean :=False;         --Boolean that indicate if the canvas save the list     
      Parent      : Container_Ptr   :=null;
      Next        : Controlled_Type;
      First       : Controlled_Type;
      Last        : Window_Ptr      :=null;
      Top         : Integer := 0;
      Left        : Integer := 0;
      Height      : Integer := 0;
      Width       : Integer := 0;
      Font        : Font_Type     :=null;
      Action      : Integer := -1;
      Tooltip     : Gtk_Tooltips := null;
      Undo_Text   : Gtk_Text_Buffer;         --Pointer for make undo function in Memo_Type
    end record;

  procedure Cleanup (Object : in out Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                C O N T A I N E R _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by container
  --  windows. It includes the following additional component:
  --
  --  Group : a flag to determine whether the WS_GROUP style should be
  --          applied to child controls (used to ensure that radiobutton
  --          groups are correctly delimited).
  --
  ----------------------------------------------------------------------------

  type Container_Internals is new Window_Internals with
     record
      Fix:Gtk_Fixed;          --Area de trabajo con los controles (Containers no Menu)
      Menu:Gtk_Menu;          --Menu para colocar otros items (Container Menu)

-----------------------------------------------------------        
--      Group : Boolean := True;
    end record;

  ----------------------------------------------------------------------------
  --
  --              M A I N _ W I N D O W _ I N T E R N A L S
  --
  --  This is a type derived from Container_Internals for use by top-level
  --  windows. It includes the following additional component:
  --
  --
  ----------------------------------------------------------------------------

  type Main_Window_Internals is new Container_Internals with
    record
        Box:Gtk_Frame;          --Frame (con shadow) para que se parezca a GtkJewl la ventana
                                --Contiene el Fix central
        Vbox: Gtk_Vbox;         --Divide el frame en 3 trozos
        MenuBar:Gtk_Menu_Bar;   --Deja previsto la inclusion de una Barra de Menu
    end record;

  type Main_Window_Ptr is access all Main_Window_Internals;

  procedure Cleanup (Object : in out Main_Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                   R A N G E I N T E R N A L S
  --
  ----------------------------------------------------------------------------
  type Range_Internals is new Window_Internals with
     record
      Adj : Gtk_Adjustment;  
    end record;

  type Range_Ptr is access all Range_Internals;

  ----------------------------------------------------------------------------
  --
  --                   C A N V A S _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by canvas
  --  window. It includes the following additional component:
  --
  --  Monitor : a protected record (defined in Canvas_Implementation)
  --            to record the drawing operations and mouse state and
  --            to synchronise accesses from the message loop task.
  --
  ----------------------------------------------------------------------------
    
  type Canvas_Internals is new Window_Internals with
    record
     Monitor  : Canvas_Monitor;
     Buffer   : Gtk_Double_Buffer;
     Paintctx : Paintcontext;
     Keypress : Integer := -1;  --Indica el comando a enviar en caso de keypress en canvas
    end record;

  procedure Cleanup (Object : in out Canvas_Internals);

  type Canvas_Ptr is access all Canvas_Internals;

  ----------------------------------------------------------------------------
  --
  --            C O M M O N _ D I A L O G _ I N T E R N A L S
  --
  --  Common_Dialog_Internals (or a type derived from it) is what the
  --  Controlled_Type object in every Common_Dialog_Type object actually
  --  points to.
  --
  ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
  --
  --  Rgb: transforma el tipo color standar de las librerias GtkJewl a color GDK
  --
  function To_GdkColor (Colour : Colour_Type) return Gdk.Color.Gdk_Color;

  type Common_Dialog_Internals is abstract new Reference_Counted_Type with 
     record
        Tipo   : Object_Type;     
        Widget : GTk_Widget;
     end record;
  
  type Common_Dialog_Ptr is access all Common_Dialog_Internals'Class;
  
--  function Show_Dialog (Dialog : access Common_Dialog_Internals; Error: out Integer)  return Boolean is abstract;
  procedure Show_Dialog (Dialog : access Common_Dialog_Internals; Error: out Integer; Result : out Boolean)  is abstract;
  ----------------------------------------------------------------------------
  --
  --            C O L O U R _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

--  type Custom_Colours is array (1..16) of aliased Win32_COLORREF;

  type Colour_Dialog_Internals is new Common_Dialog_Internals with
    record
      Colour : Colour_Type;
    end record;

  type Colour_Dialog_Ptr is access all Colour_Dialog_Internals'Class;

--  function Show_Dialog (Dialog : access Colour_Dialog_Internals) return Boolean;
  procedure Show_Dialog (Dialog : access Colour_Dialog_Internals; Error: out Integer; Result : out Boolean);

  ----------------------------------------------------------------------------
  --
  --              F O N T _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Font_Dialog_Internals is new Common_Dialog_Internals with
    record
     Font   : Font_Type;
    end record;

  type Font_Dialog_Ptr is access all Font_Dialog_Internals'Class;

--  function Show_Dialog (Dialog : access Font_Dialog_Internals; Error: out Integer) return Boolean;
  procedure Show_Dialog (Dialog : access Font_Dialog_Internals; Error: out Integer; Result : out Boolean);


  ----------------------------------------------------------------------------
  --
  --              F I L E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

--  type File_Dialog_Internals (N : Natural) is
  type File_Dialog_Internals is abstract new Common_Dialog_Internals with
    record
--      Struct    : aliased Win32_OPENFILENAME;
--      Title     : String (1..N);
      Buffer    : String (1..300):= (others => ' ');
      Directory : aliased String (1..300):= (others => ' ');
      Filter    : aliased String (1..300):= (others => ' ');
--      Length    : Natural := 0;
      Create    : Boolean := False;
    end record;

  type File_Dialog_Ptr is access all File_Dialog_Internals'Class;
  
  ----------------------------------------------------------------------------
  --
  --              O P E N _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Internals is new File_Dialog_Internals with null record;

--  function Show_Dialog (Dialog : access Open_Dialog_Internals; Error: out Integer) return Boolean;
  procedure Show_Dialog (Dialog : access Open_Dialog_Internals; Error: out Integer; Result : out Boolean);


  ----------------------------------------------------------------------------
  --
  --              S A V E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Internals is new File_Dialog_Internals with null record;

--  function Show_Dialog (Dialog : access Save_Dialog_Internals) return Boolean;
  procedure Show_Dialog (Dialog : access Save_Dialog_Internals; Error: out Integer; Result : out Boolean);



  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ I N F O
  --
  --  This protected record is used for global communication between the
  --  message loop task and the main task. The private data this contains
  --  is as follows:
  --
  --  Command       : the current command code (0 if no command available)
  --  Dialog        : the current active dialog (null if no dialog active)
  --  Task_Failed   : True if the message loop task has failed
  --  Failure_Info  : the exception that caused the message loop to fail
  --
  --  The operations provide are as follows:
  --
  --  Get_Command   : wait for a command and then return its code
  --  Test_Command  : test if there is a command pending
  --  Set_Command   : set the current command
  --  Get_Dialog    : get the handle of the current dialog (null if none)
  --  Active_Window : set the handle of the active dialog window and get
  --                  the old value of the handle
  --  Record_Error  : record an exception that caused the message loop
  --                  to fail
  --
  ----------------------------------------------------------------------------

  protected Window_Info is
    procedure Get_Command   (Cmd : out Natural);
    function  Test_Command  return Boolean;
    procedure Set_Command   (Cmd : in  Natural);
    
    procedure Get_Dialog    (Dlg : in out Gtk_Window);
    procedure Set_Active_Window    (Win : in Gtk_Window);
    function  Active_Window return Gtk_Window;
    procedure Record_Error  (Err : in Ada.Exceptions.Exception_Occurrence);
    
    procedure Get_Dialog_Finish_State (Ok : out Boolean);
    function  Test_Dialog_Finish return Boolean;
    procedure Set_Dialog_Finish_State (Ok : in  Boolean);
    
  private
    Command      : Natural := 0;
    
    Dialog       : Gtk_Window:=null;
    Activedlg    : Gtk_Window:=null;
    
    DlgFinish    : Boolean :=False;
    DlgFinishState: Boolean :=False;
    
    Task_Failed  : Boolean := False;
    Failure_Info : Ada.Exceptions.Exception_Occurrence;
  end Window_Info;

 
end GtkJewl.Window_Implementation;
