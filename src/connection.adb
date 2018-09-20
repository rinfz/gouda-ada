with GNATCOLL.JSON; use GNATCOLL.JSON;
with Dict;

package body Connection is
   function POST (Endpoint : String;
		  Data : Dict.Items;
		  Parameters : Params.Map;
		  Version : String := "unstable")
		 return JSON_Value is
   begin
      null;
   end POST;

   function GET (Endpoint : String;
		 Parameters : Params.Map;
		 Version : String := "unstable")
		return JSON_Value is
   begin
      null;
   end GET;

   function Login return JSON_Value is
   begin
      null;
   end Login;
end Connection;
