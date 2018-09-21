with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Config is
  package UB renames Ada.Strings.Unbounded;

  function Read_Config return GNATCOLL.JSON.JSON_Value is
    Input : File_Type;
    Stream : UB.Unbounded_String;
  begin
    Open (File => Input, Mode => In_File, Name => "config.json");
    loop
      declare
        Line : String := Get_Line (Input);
      begin
        UB.Append (Stream, Line);
      end;
      exit when End_Of_File (Input);
    end loop;
    Close (Input);
    return GNATCOLL.JSON.Read (Stream);
  end Read_Config;
end Config;
