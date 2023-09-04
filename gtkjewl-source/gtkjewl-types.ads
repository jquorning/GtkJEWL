------------------------------------------------------------------------------
--
--                     G T K J E W L . T Y P E S
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

package GtkJewl.Types is
     type Object_Type is
     (Type_None,
      Type_Window,
        Type_Frame, Type_Dialog, Type_Panel, Type_Menu,
        Type_Button, Type_Action_Menuitem, Type_Label, Type_Editbox,
           Type_Menuitem, Type_Separator, Type_Checkbox, Type_Radiobutton,
           Type_Check_Menuitem, Type_Radio_Menuitem,
        Type_Listbox, Type_Combobox, Type_Memo,
        Type_Canvas,
        Type_Range_Control,
           Type_Progressbar, Type_Scale, Type_Scrollbar, Type_Spinbutton,
      Type_Colour_Dialog, Type_Font_Dialog, Type_Open_Dialog, Type_Save_Dialog
     );
     pragma Convention (C, Object_Type);      
end GtkJewl.Types;
