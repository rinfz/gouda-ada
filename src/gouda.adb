with Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict; use Dict;

package body Gouda is

   procedure Run is
      Data : String := To_Json (
         ((+"first", Create (Integer'(0))), (+"second", Create (Integer'(10))))
      );
   begin
      Ada.Text_IO.Put_Line (Data);
   end Run;

end Gouda;
