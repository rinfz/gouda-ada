with GNATCOLL.JSON;
with GNATCOLL.Strings;

package body Gouda.JSON is

   use GNATCOLL.JSON;
   use GNATCOLL.Strings;

   function Build_Data (Data : POST_Data.Map) return String
   is
      Obj : JSON_Value := Create_Object;
   begin
      for I in Data.Iterate loop
	 Set_Field (Obj, POST_Data.Key (I), Data (I));
      end loop;
      return Obj.Write;
   end Build_Data;

end Gouda.JSON;
