with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with Connection;

pragma Elaborate_All (GNATCOLL.JSON);

package Gouda is
  package GJ renames GNATCOLL.JSON;
  package UB renames Ada.Strings.Unbounded;

  type Message is record
    -- TODO: use UTF8 String
    Msg_Body : UB.Unbounded_String;
    Sender : UB.Unbounded_String;
  end record;
  type Messages (<>) is private;

  function Create_Message (Msg_Body : UB.Unbounded_String;
                           Sender : UB.Unbounded_String) return Message;
  function Extract_Messages (Conn : Connection.Matrix;
                             Data : GJ.JSON_Value) return Messages;
  procedure Run;
private
  type Messages is array (Positive range <>) of Message;

  No_Messages : constant Messages (1 .. 0) := (
    (others => Message'(UB.To_Unbounded_String (""), UB.To_Unbounded_String ("")))
  );
end Gouda;
