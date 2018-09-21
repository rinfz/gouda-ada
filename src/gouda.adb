with Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Config;
with Connection;

package body Gouda is

  procedure Run is
    Main_Config : JSON_Value := Config.Read_Config;
    Conn : Connection.Matrix := Connection.Create (Main_Config);
    Login : JSON_Value := Conn.Login;
  begin
    Ada.Text_IO.Put_Line (Login.Get ("user_id"));
    Ada.Text_IO.Put_Line (Login.Get ("access_token"));
  end Run;

end Gouda;
