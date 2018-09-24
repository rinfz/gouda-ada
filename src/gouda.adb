with Ada.Text_IO;
with Config;
with Connection;

package body Gouda is

  function Extract_Messages (Data : GJ.JSON_Value) return Messages is
  begin
    return No_Messages;
  end Extract_Messages;

  procedure Run is
    Main_Config : GJ.JSON_Value := Config.Read_Config;
    Conn : Connection.Matrix := Connection.Create (Main_Config);
    Login : GJ.JSON_Value := Conn.Login;
    Join : GJ.JSON_Value := Conn.Join;
  begin
    loop
      declare
        Data : GJ.JSON_Value := Conn.Sync;
      begin
        Ada.Text_IO.Put_Line (Data.Write);
        delay 3.0;
      end;
    end loop;
  end Run;

end Gouda;
