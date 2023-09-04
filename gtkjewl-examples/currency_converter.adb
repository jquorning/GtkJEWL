with GtkJewl.Simple_Windows;
use  GtkJewl.Simple_Windows;
procedure Currency_Converter is

  My_Frame : Frame_Type := Frame (400, 150, "Currency Converter", 'Q');
  Value    : Editbox_Type :=
                 Editbox (My_Frame, (10,10), 110, 20, "0");
  Do_It    : Button_Type :=
                 Button (My_Frame, (40,40), 80, 25, "Convert", 'X');
  Set_Rate : Button_Type :=
                 Button (My_Frame, (140,40), 80, 25, "Set Rate", 'R');
  Result   : Label_Type :=
                 Label (My_Frame, (10,75), 0, 20, "", Centre);

  Convert  : Dialog_Type :=
                 Dialog (200, 100, "Conversion Rate", 'Q');
  C_Label  : Label_Type :=
                 Label (Convert, (10,10), 40, 25, "Rate:", Right);
  C_Edit   : Editbox_Type :=
                 EditBox (Convert, (60,10), 100, 25, "1.00");
  C_OK     : Button_Type :=
                 Button (Convert, (10,45) , 80, 25, "OK", 'Y');
  C_Cancel : Button_Type :=
                 Button (Convert, (100,45) , 80, 25, "Cancel", 'Q');

  Rate : Float := 1.00;
  Val  : Float := 0.00;
  
begin
  loop
    case Next_Command is
      when 'Q' =>
        exit;
      when 'R' =>
        if Execute(Convert) = 'Y' then
          Rate := Float'Value(Get_Text(C_Edit));
        end if;
      when 'X' =>
        Val := Float(Integer'Value(Get_Text(Value)));
        Set_Text (Result, Integer'Image(Integer(Val*Rate)));
      when others =>
        null;
    end case;
  end loop;
end Currency_Converter;