with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict;

package Connection is
  type Matrix is tagged private;

  package UB renames Ada.Strings.Unbounded;

  function Create (Config : JSON_Value) return Matrix;
  function Get_Access_Token (Self : Matrix) return UB.Unbounded_String;
  function Get_User_ID (Self : Matrix) return UB.Unbounded_String;
  function Get_Room_ID (Self : Matrix) return UB.Unbounded_String;
  function Get_Filter (Self : Matrix) return UB.Unbounded_String;

  procedure Login (Self : in out Matrix)
    with
      Pre  => UB.Length (Self.Get_Access_Token) = 0 and
        UB.Length (Self.Get_User_ID) = 0,
      Post => UB.Length (Self.Get_Access_Token) > 0 and
        UB.Length (Self.Get_User_ID) > 0;
  procedure Join (Self : in out Matrix)
    with
      Pre  => UB.Length (Self.Get_Room_ID) = 0,
      Post => UB.Length (Self.Get_Room_ID) > 0;
  procedure Upload_Filter (Self : in out Matrix);
  function Sync (Self : in out Matrix) return JSON_Value
    with
      Pre => UB.Length (Self.Get_Filter) > 0;
  procedure Send_Message (Self : in out Matrix; Message : String);

private

  type Matrix is tagged record
    Base_Url : UB.Unbounded_String;
    Username : UB.Unbounded_String;
    Password : UB.Unbounded_String;
    Room : UB.Unbounded_String;
    User_ID : UB.Unbounded_String;
    Room_ID : UB.Unbounded_String;
    Access_Token : UB.Unbounded_String;
    Next_Batch : UB.Unbounded_String;
    Filter : UB.Unbounded_String;
    TX_ID : Natural;
  end record;

  function "+" (Source : String) return UB.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;
  function "-" (Source : UB.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;

  package Params is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type => String,
     Element_Type => String,
     Hash => Ada.Strings.Hash,
     Equivalent_Keys => "=");

  function Build_Url (Self : Matrix;
                      Endpoint : String;
                      Parameters : Params.Map;
                      Version : String)
  return String;

  function POST (Self : Matrix;
                 Endpoint : String;
                 Data : Dict.Items;
                 Parameters : Params.Map := Params.Empty_Map;
                 Version : String := "unstable")
  return JSON_Value;

  function GET (Self : Matrix;
                Endpoint : String;
                Parameters : Params.Map := Params.Empty_Map;
                Version : String := "unstable")
  return JSON_Value;

  function PUT (Self : Matrix;
                Endpoint : String;
                Data : Dict.Items;
                Parameters : Params.Map := Params.Empty_Map;
                Version : String := "unstable")
  return JSON_Value;
end Connection;
