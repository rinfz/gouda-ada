with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

package Dict is
  package UB renames Ada.Strings.Unbounded;
  package GJ renames GNATCOLL.JSON;

  type Item is record
    Key : UB.Unbounded_String;
    Value : GJ.JSON_Value;
  end record;
  type Items is array (Positive range <>) of Item;

  function "+" (Source : String) return UB.Unbounded_String
    renames UB.To_Unbounded_String;

  function "-" (Source : UB.Unbounded_String) return String
    renames UB.To_String;

  function To_Json (Dict : Items) return String;
end Dict;
