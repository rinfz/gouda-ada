with GNATCOLL.JSON; use GNATCOLL.JSON;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Gouda.JSON is

   package POST_Data is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => JSON_Value,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   function Build_Data (Data : POST_Data.Map) return String;

end Gouda.JSON;
