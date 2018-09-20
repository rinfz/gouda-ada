with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict;

package Connection is
   function Login return JSON_Value;
private
   package Params is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   function POST (Endpoint : String;
		  Data : Dict.Items;
		  Parameters : Params.Map;
		  Version : String := "unstable")
		 return JSON_Value;
   function GET (Endpoint : String;
		 Parameters : Params.Map;
		 Version : String := "unstable")
		return JSON_Value;
end Connection;
