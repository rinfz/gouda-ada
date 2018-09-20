with Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict; use Dict;
with Gouda_Config;

package body Gouda is

   procedure Run is
      Data : String := To_Json (
         ((+"first", Create (Integer'(0))), (+"second", Create (Integer'(10))))
      );
      Config : JSON_Value := Gouda_Config.Read_Config;
   begin
      Ada.Text_IO.Put_Line (Data);
      Ada.Text_IO.Put_Line (Config.Get ("username"));
   end Run;

end Gouda;
