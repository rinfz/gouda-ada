with GNATCOLL.JSON; use GNATCOLL.JSON;

with Ada.Text_IO;

with Gouda.JSON;

package body Gouda is

   procedure Run is
      package GJ renames Gouda.JSON;
      M : GJ.POST_Data.Map;
   begin
      M.Include ("Test", (Create (Integer'(10))));
      Ada.Text_IO.Put_Line (GJ.Build_Data (M));
   end Run;

end Gouda;
