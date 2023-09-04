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

with Gtk.Font_Selection;use Gtk.Font_Selection;
with gtk.color_selection;use gtk.color_selection;
with Gtk.Color_Selection_Dialog;use Gtk.Color_Selection_Dialog;
with gtk.file_selection;use gtk.file_selection;

with glib; use glib;
with glib.convert; use glib.convert;
with glib.object;use glib.object;

with Gdk.Color; use Gdk.Color;
with Gtk.Gentry; use Gtk.Gentry;
with Gtk.Main;   use Gtk.Main;
with ada.Text_IO;

with Ada.Unchecked_Deallocation;
with Gnat.Os_Lib; use Gnat.Os_Lib;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Unchecked_Conversion;

package body GtkJewl.Window_Implementation is

  use GtkJewl.Canvas_Implementation;
  use type System.Address;
  use Ada.Exceptions;

  use type System.Address;
  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Window_Internals.
  --
  --  This destroys the font handle associated with the window.
  --
  ----------------------------------------------------------------------------
  procedure FreeFont is new Ada.Unchecked_Deallocation
                                (Font_Type_Record'Class,
                                Font_Type);
     
  procedure Cleanup (Object : in out Window_Internals) is
  begin
     hide(Object.Widget);
     Object.Widget:=null;
     Object.Label_Widget:=null;
     Object.Undo_text:=null;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Main_Window_Internals.
  --
  --  This asks the message loop to destroy the window if it still exists to
  --  ensure that the count of top-level windows is decremented properly.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Main_Window_Internals) is
  begin
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Canvas_Internals.
  --
  --  This deletes the drawing list associated with the canvas.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Canvas_Internals) is
  begin
    Object.Monitor.Clear;
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Rgb: transforma el tipo color standar de las librerias GtkJewl a color GDK
  --
  function To_GdkColor (Colour : Colour_Type) return Gdk.Color.Gdk_Color is
     Color : Gdk.Color.Gdk_Color;
  begin
     Gdk.Color.Set_Rgb(Color,
        Guint16((Colour.Red*Integer(Guint16'Last))/Colour_Range'Last),
        Guint16((Colour.Green*Integer(Guint16'Last))/Colour_Range'Last),
        Guint16((Colour.Blue*Integer(Guint16'Last))/Colour_Range'Last));
    return (color);
  end To_GdkColor;
    
  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a colour dialog.
  --
--   function Show_Dialog (Dialog : access Colour_Dialog_Internals ) return Boolean is
  procedure Show_Dialog (Dialog : access Colour_Dialog_Internals; Error: out Integer; Result : out Boolean) is
     B    : Boolean;
     Dead : Boolean;
      Col : Gdk_Color;
   begin
      col:=To_GdkColor(Dialog.Colour);
      Gtk.Color_Selection.Set_Current_Color(Get_Colorsel(Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog(Dialog.Widget)),Col);

      -- Wait for a command (which must be from this dialog, as dialog
      -- windows disable all other windows belonging to the application)
     while (not Window_Info.Test_Dialog_Finish) loop
        Dead:=Gtk.Main.Main_Iteration(True);
     end loop;
     Window_Info.Get_Dialog_Finish_State(B);
      
      if B then
         Gtk.Color_Selection.Get_Current_Color(Get_Colorsel(Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog(Dialog.Widget)),Col);
         Dialog.Colour.Red:=Colour_Range((Integer(gdk.color.Red(Col))*Colour_Range'Last)/Integer(Guint16'Last));
         Dialog.Colour.Green:=Colour_Range((Integer(gdk.color.Green(Col))*Colour_Range'Last)/Integer(Guint16'Last));
         Dialog.Colour.Blue:=Colour_Range((Integer(gdk.color.Blue(Col))*Colour_Range'Last)/Integer(Guint16'Last));
      end if;
      Error:=0;
      Result:=B;
   end Show_Dialog;


  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a font dialog.
  --
--   function Show_Dialog (Dialog : access Font_Dialog_Internals ) return Boolean is 
  procedure Show_Dialog (Dialog : access Font_Dialog_Internals; Error: out Integer; Result : out Boolean) is
     B    : Boolean;
     Dead : Boolean;
   begin
      B:=Gtk.Font_Selection.Set_Font_Name(Gtk_Font_Selection_Dialog(Dialog.Widget),Font2string(Dialog.Font));
     
      -- Wait for a command (which must be from this dialog, as dialog
      -- windows disable all other windows belonging to the application)
     while (not Window_Info.Test_Dialog_Finish) loop
        Dead:=Gtk.Main.Main_Iteration(True);
     end loop;
     Window_Info.Get_Dialog_Finish_State(B);
      
      if B then
         Dialog.Font:=String2font(Gtk.Font_Selection.Get_Font_Name(Gtk_Font_Selection_Dialog(Dialog.Widget)));
      end if;
      Error:=0;
      Result:=B;
   end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show an open file dialog.
  --
--  function Show_Dialog (Dialog : access Open_Dialog_Internals) is
  procedure Show_Dialog (Dialog : access Open_Dialog_Internals; Error: out Integer; Result : out Boolean) is
     B    : Boolean:=False;
     Dead : Boolean;
     Len  : Integer;
  begin
     if Trim(Dialog.Buffer,ada.Strings.Both) /= "" then
        Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),Dialog.Buffer);
     else
        Len:=Trim(Dialog.Directory,Ada.Strings.Both)'Length;    
        if Len > 0 and then Len < Dialog.Directory'Last and then Dialog.Directory(Dialog.Directory'First + Len -1) /=Gnat.Os_Lib.Directory_Separator then
           Dialog.Directory(Dialog.Directory'First .. Dialog.Directory'First + Len)
              := Dialog.Directory(Dialog.Directory'First .. Dialog.Directory'First + Len-1) & Gnat.Os_Lib.Directory_Separator;
        end if;
        if Len > 0 then
           Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),Dialog.Directory);
        else
           Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),"");
        end if;
     end if;
     
   Error:=-1;
   loop
      -- Wait for a command (which must be from this dialog, as dialog
      -- windows disable all other windows belonging to the application)
     while (not Window_Info.Test_Dialog_Finish) loop
        Dead:=Gtk.Main.Main_Iteration(True);
     end loop;
     Window_Info.Get_Dialog_Finish_State(B);
      
        if B then      
        declare
           nfile  : String :=Trim(Get_Text(Gtk_Entry(Get_Selection_Entry(Gtk_File_Selection(Dialog.Widget)))),ada.Strings.both);   
           S      : String :=Trim(Get_Filename(Gtk_File_Selection(Dialog.Widget)),ada.Strings.both);
           fexist : Boolean:=False;   
        begin

           if nFile /= "" then
              declare
                 F:ada.Text_IO.File_Type;
              begin
                 Fexist:=False;
                 Ada.Text_Io.Open(
                 File => F,
                 Mode => ada.Text_IO.In_File,
                 Name => S);
              ada.Text_IO.Close(F);
                 Fexist:=True;
              exception  
                 when others => 
                    Fexist:=false;
                    if Ada.Text_Io.Is_Open(F) then
                       Ada.Text_Io.Close(F);
                    end if;             
              end;
              if Fexist then
                 if Dialog.Buffer'Last >= S'Length then
                    Dialog.Buffer(1..S'Length):=S;
                    Dialog.Buffer(S'Length+1 .. Dialog.Buffer'Last):=(others => ' ');
                    for I in reverse 1.. Dialog.Buffer'Last loop
                       if Dialog.Buffer(I) = Gnat.Os_Lib.Directory_Separator then
                          Dialog.Directory(1..I):=Dialog.Buffer(1..I);
                          if I < Dialog.Directory'Last then
                             Dialog.Directory(I+1..Dialog.Directory'Last):=(others => ' ');
                          end if;
                          exit when true;
                       end if;
                    end loop;
                    Error:=0;
                 else
                    Dialog.Buffer:=(others => ' ');
                    Error:=2;
                    --Path del archivo demasiado largo
                 end if;
              else
                 Error:=1;
                 --No se encuentra el archivo
                 --Asegurese de haber dado el nombre de archivo correcto
              end if;
           end if;      
        end;
     else
        Error:=0;      
     end if;
     exit when Error>=0;      
  end loop;
      Result:=B;
  end Show_Dialog;
             

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a save file dialog.
  --
--  function Show_Dialog (Dialog : access Save_Dialog_Internals) return Boolean is
  procedure Show_Dialog (Dialog : access Save_Dialog_Internals; Error: out Integer; Result : out Boolean) is
     B    : Boolean;
     Dead : Boolean;
     Len  : Integer;
  begin
     if Trim(Dialog.Buffer,ada.Strings.Both) /= "" then
        Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),Trim(Dialog.Buffer,ada.Strings.Both));
     else
        Len:=Trim(Dialog.Directory,Ada.Strings.Both)'Length;    
        if Len > 0 and then Dialog.Directory(Dialog.Directory'First + Len -1) /=Gnat.Os_Lib.Directory_Separator then
           Dialog.Directory(Dialog.Directory'First .. Dialog.Directory'First + Len)
              := Dialog.Directory(Dialog.Directory'First .. Dialog.Directory'First + Len-1) & Gnat.Os_Lib.Directory_Separator;
        end if;
        if Len > 0 then
           Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),Trim(Dialog.Directory,Ada.Strings.Both));
        else
           Gtk.File_Selection.Set_Filename(Gtk_File_Selection(Dialog.Widget),"");
        end if;
     end if;
     
     Error:=-1;
  loop     
     while (not Window_Info.Test_Dialog_Finish) loop
        Dead:=Gtk.Main.Main_Iteration(True);
     end loop;
     Window_Info.Get_Dialog_Finish_State(B);
     
        if B then      
        declare
           nfile  : String :=Trim(Get_Text(Gtk_Entry(Get_Selection_Entry(Gtk_File_Selection(Dialog.Widget)))),ada.Strings.both);   
           S      : String :=Trim(Gtk.File_Selection.Get_Filename(Gtk_File_Selection(Dialog.Widget)),Ada.Strings.Both);
           fexist : Boolean:=False;   
        begin
           if nFile /= "" then
              declare
                 F:ada.Text_IO.File_Type;
              begin
                 Fexist:=False;
              Ada.Text_Io.Open(
                 File => F,
                 Mode => ada.Text_IO.In_File,
                    Name => S);
                 ada.Text_IO.Close(F);
                 Fexist:=True;
              exception  
                 when others => 
                    Fexist:=false;
                    if Ada.Text_Io.Is_Open(F) then
                       Ada.Text_Io.Close(F);
                    end if;             
              end;
              if Dialog.Buffer'Last >= S'Length then
                 Dialog.Buffer(1..S'Length):=S;
                 Dialog.Buffer(S'Length+1 .. Dialog.Buffer'Last):=(others => ' ');
                 for I in reverse 1.. Dialog.Buffer'Last loop
                    if Dialog.Buffer(I) = Gnat.Os_Lib.Directory_Separator then
                       Dialog.Directory(1..I):=Dialog.Buffer(1..I);
                       if I < Dialog.Directory'Last then
                          Dialog.Directory(I+1..Dialog.Directory'Last):=(others => ' ');
                       end if;
                       exit when true;
                    end if;
                 end loop;
                 Error:=0;
              else
                 Dialog.Buffer:=(others => ' ');
                 Error:=2;
                 --Path del archivo demasiado largo
              end if;
              
              if Error = 0 then
                 if Fexist then
                    if Dialog.Create = True then 
                       Error := 4;  ---Existe y pregunta si reemplazar
                    else
                       Error := 0;
                    end if;       
                 else
                    if Dialog.Create = False then 
                       Error := 3;  ---NO Existe y pregunta si crearlo
                    else
                       Error := 0;
                    end if;       
                 end if;
              end if;
           end if;      
        end;
     else
        Error:=0;      
     end if;
     exit when Error>=0;      
  end loop;
      Result:=B;
  end Show_Dialog;


   ----------------------------------------------------------------------------
   --
   --                        W I N D O W _ I N F O
   --
   ----------------------------------------------------------------------------

   protected body Window_Info is

      --------------------------------------------------------------------------
      --
      --  Get_Command: block until a command is available (or the message loop
      --               has failed) and then return it. The command is also reset.
      --               Program_Error is raised if the message loop has failed.
      --
      procedure Get_Command (Cmd :    out Natural ) is
      begin
            if Task_Failed then
               Raise_Exception (Program_Error'Identity,
                  "caused by " & Exception_Name(Failure_Info) &
                  ": " & Exception_Message(Failure_Info));
            end if;
            if Command/=0 then
               Cmd := Command-1;
               Command := 0;
            end if;
      end Get_Command;

      --------------------------------------------------------------------------
      --
      --  Test_Command: test if a command is pending. Program_Error is raised
      --                if the message loop task has failed.
      --
      function Test_Command return Boolean is   
      begin
         if Task_Failed then
            Raise_Exception (Program_Error'Identity,
               "caused by " & Exception_Name(Failure_Info) &
               ": " & Exception_Message(Failure_Info));
         end if;
         return Command /= 0;
      end Test_Command;

      --------------------------------------------------------------------------
      --
      --  Set_Command: store the code for an available command. This is a
      --               procedure, not an entry, so the the message loop won't
      --               stall. If commands aren't handled in time, they'll be
      --               overwritten by the next one that comes along.
      --
      procedure Set_Command (
            Cmd : in     Natural ) is 
      begin
         Command := Cmd+1;
      end Set_Command;

      --------------------------------------------------------------------------
      --
      --  Get_Dialog: swap the handle of the current active window with the
      --              parameter (i.e. record the new handle and return the
      --              old one).
      --
      procedure Get_Dialog (
            Dlg : in out Gtk_Window ) is 
         D : Gtk_Window := Dialog;  
      begin
         Dialog := Dlg;
         Dlg := D;
      end Get_Dialog;

      procedure Set_Active_Window (
            Win : in Gtk_Window ) is   
      begin
         ActiveDlg := Win;
      end Set_Active_Window;

      --------------------------------------------------------------------------
      --
      --  Active_Window: get the handle of the current active window.
      --
      function Active_Window return Gtk_Window is 
      begin
         return ActiveDlg;
      end Active_Window;

      --------------------------------------------------------------------------
      --
      --  Record_Error : record the occurrence of an exception which aborted
      --                 the message loop task.
      --
      procedure Record_Error (
            Err : in     Ada.Exceptions.Exception_Occurrence ) is 
      begin
         Task_Failed := True;
         Ada.Exceptions.Save_Occurrence (Failure_Info, Err);
      end Record_Error;
      

      procedure  Get_Dialog_Finish_State (Ok : out Boolean) is 
      begin
         Dlgfinish:=False;
         Ok:=DlgFinishState;
      end Get_Dialog_Finish_State;
               
      function Test_Dialog_Finish return Boolean is
      begin
         return DlgFinish;
      end Test_Dialog_Finish;

      procedure Set_Dialog_Finish_State (Ok : in  Boolean) is
      begin
         DlgFinishState:=Ok;
         DlgFinish:=True;
      end Set_Dialog_Finish_State;

   end Window_Info;


end GtkJewl.Window_Implementation;
