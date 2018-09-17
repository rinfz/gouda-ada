with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package body Gouda.Config is
   function Read_Config return JSON_Value is
      Input : File_Type;
      Stream : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "config.json");
      loop
	 declare
	    Line : String := Get_Line (Input);
	 begin
	    Append (Stream, Line);
	 end;
	 exit when End_Of_File (Input);
      end loop;
      Close (Input);
      return Read (Stream);
   end Read_Config;
end Gouda.Config;
