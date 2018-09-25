with Config;

pragma Elaborate_All (Config);

package body Gouda is

  function Create_Message (Msg_Body : UB.Unbounded_String;
                           Sender : UB.Unbounded_String) return Message
  is (Message'(Msg_Body, Sender));

  function Extract_Messages (Conn : Connection.Matrix;
                             Data : GJ.JSON_Value) return Messages is
    Room : constant String := UB.To_String (Conn.Get_Room_ID);
    Key : constant GJ.JSON_Value := Data.Get ("rooms").Get ("join");
  begin
    if Key.Has_Field (Room) then
      declare
        Events : constant GJ.JSON_Array := Key.Get (Room)
                                              .Get ("timeline")
                                              .Get ("events");
        Result_Length : constant Natural := GJ.Length (Events);
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
    Main_Config : constant GJ.JSON_Value := Config.Read_Config;
    Conn : Connection.Matrix := Connection.Create (Main_Config);
  begin
    Conn.Login;
    Conn.Join;
    Conn.Upload_Filter;
    declare
      Initial_Sync : constant GJ.JSON_Value := Conn.Sync;
    begin
      pragma Unreferenced (Initial_Sync);
    end;
    loop
      declare
        -- TODO: only sync from start up
        Data : constant GJ.JSON_Value := Conn.Sync;
        Text_Messages : constant Messages := Extract_Messages (Conn, Data);
      begin
        for M of Text_Messages loop
          if UB.Count (M.Msg_Body, "hello bot") > 0 then
            Conn.Send_Message ("YO WHAT UP");
          end if;
        end loop;
      end;
      delay 3.0;
    end loop;
  end Run;

end Gouda;
