with Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict; use Dict;
with Config;

package body Gouda is

   procedure Run is
      Data : String := To_Json (
         ((+"first", Create (Integer'(0))), (+"second", Create (Integer'(10))))
      );
      Main_Config : JSON_Value := Config.Read_Config;
   begin
      Ada.Text_IO.Put_Line (Data);
      Ada.Text_IO.Put_Line (Main_Config.Get ("username"));
   end Run;

end Gouda;
