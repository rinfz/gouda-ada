with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package Dict is
   type Item is record
      Key : Unbounded_String;
      Value : Json_Value;
   end record;
   type Items is array (Positive range <>) of Item;

   package Json_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Json_Value,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   function "+" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   function To_Map (Dict : Items) return Json_Map.Map;
   -- TODO: This should take an "Items" instead of a map
   function To_Json (Map : Json_Map.Map) return String;
end Dict;
