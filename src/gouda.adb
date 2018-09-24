with Ada.Text_IO;
with Config;

package body Gouda is

  function Create_Message (Msg_Body : UB.Unbounded_String;
                           Sender : UB.Unbounded_String) return Message
  is (Message'(Msg_Body, Sender));

  function Extract_Messages (Conn : Connection.Matrix;
                             Data : GJ.JSON_Value) return Messages is
    Room : String := UB.To_String (Conn.Get_Room_ID);
    Key : GJ.JSON_Value := Data.Get ("rooms")
                               .Get ("join");
  begin
    if Key.Has_Field (Room) then
      declare
        Events : GJ.JSON_Array := Key.Get (Room).Get ("timeline").Get ("events");
        Result_Length : Natural := GJ.Length (Events);
        Result : Messages (1 .. Result_Length);
      begin
        for I in 1 .. Result_Length loop
          Result (I) := Create_Message (
            GJ.Get (Events, I).Get ("content").Get ("body").Write,
            GJ.Get (Events, I).Get ("sender").Write
          );
        end loop;
        return Result;
      end;
    else
      return No_Messages;
    end if;
  end Extract_Messages;

  procedure Run is
    Main_Config : GJ.JSON_Value := Config.Read_Config;
    Conn : Connection.Matrix := Connection.Create (Main_Config);
  begin
    Conn.Login;
    Conn.Join;
    Conn.Upload_Filter;
    loop
      declare
        Data : GJ.JSON_Value := Conn.Sync;
        Text_Messages : Messages := Extract_Messages (Conn, Data);
      begin
        for M of Text_Messages loop
          Ada.Text_IO.Put_Line (UB.To_String (M.Msg_Body));
        end loop;
      end;
      delay 3.0;
    end loop;
  end Run;

end Gouda;
