with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict;

package Connection is
  type Matrix is tagged private;

  function Create (Config : JSON_Value) return Matrix;
  function Login (Self : in out Matrix) return JSON_Value;

private
  package UB renames Ada.Strings.Unbounded;

  type Matrix is tagged record
    Base_Url : UB.Unbounded_String;
    Username : UB.Unbounded_String;
    Password : UB.Unbounded_String;
    Room : UB.Unbounded_String;
    Access_Token : UB.Unbounded_String;
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
end Connection;
