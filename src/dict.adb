with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package body Dict is
   function To_Map (Dict : Items) return Json_Map.Map is
      M : Json_Map.Map;
   begin
      for Item of Dict loop
	 M.Include (To_String (Item.Key), Item.Value);
      end loop;
      return M;
   end To_Map;

   function To_Json (Map : Json_Map.Map) return String is
      Result : Json_Value := Create_Object;
   begin
      for K in Map.Iterate loop
	 Set_Field (Result, Json_Map.Key (K), Map (K));
      end loop;
      return Result.Write;
   end To_Json;
end Dict;
