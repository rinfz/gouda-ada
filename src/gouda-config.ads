with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict; use Dict;

package Gouda.Config is
   function Read_Config return JSON_Value;
end Gouda.Config;
