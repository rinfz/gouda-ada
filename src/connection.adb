with AWS.Client;
with AWS.Response;

package body Connection is
  function Create (Config : JSON_Value) return Matrix is
    Room : UB.Unbounded_String := Config.Get ("room");
    Hash_Idx : Positive := UB.Index (Room, "#", 1);
    Colon_Idx : Positive := UB.Index (Room, ":", 1);
  begin
    UB.Replace_Slice (Room, Hash_Idx, Hash_Idx, "%23");
    -- Adjust indexes to account for mutation with Hash_Idx
    UB.Replace_Slice (Room, Colon_Idx + 2, Colon_Idx + 2, "%3A");
    return Matrix'(
      Base_Url     => + (Config.Get ("address")),
      Username     => + (Config.Get ("username")),
      Password     => + (Config.Get ("password")),
      Room         =>    Room,
      Room_ID      => + (""),
      Access_Token => + ("")
    );
  end Create;

  function Build_Url (Self : Matrix;
                      Endpoint : String;
                      Parameters : Params.Map;
                      Version : String)
  return String is
    Url : UB.Unbounded_String := Self.Base_Url;
    -- need to copy in case it's an empty map
    P : Params.Map := Parameters.Copy;
  begin
    UB.Append (Url, "/_matrix/client/" & Version & "/" & Endpoint);
    if UB.Length (Self.Access_Token) > 0 then
      P.Include ("access_token", - (Self.Access_Token));
    end if;

    if not P.Is_Empty then
      UB.Append (Url, "?");
      -- final trailing & on the url might be ok to leave in
      for K in P.Iterate loop
        UB.Append(Url, Params.Key (K) & "=" & P (K) & "&");
      end loop;
      Url := + (UB.Slice (Url, 1, UB.Length (Url) - 1));
    end if;

    return -Url;
  end Build_Url;

  function POST (Self : Matrix;
                 Endpoint : String;
                 Data : Dict.Items;
                 Parameters : Params.Map := Params.Empty_Map;
                 Version : String := "unstable")
  return JSON_Value is
    Url : String := Self.Build_Url (Endpoint, Parameters, Version);
    Response : AWS.Response.Data := AWS.Client.Post (
      Url, Dict.To_Json (Data), "application/json"
    );
  begin
    return GNATCOLL.JSON.Read (String'(AWS.Response.Message_Body (Response)));
  end POST;

  function GET (Self : Matrix;
                Endpoint : String;
                Parameters : Params.Map := Params.Empty_Map;
                Version : String := "unstable")
  return JSON_Value is
    Url : String := Self.Build_Url (Endpoint, Parameters, Version);
    Response : AWS.Response.Data := AWS.Client.Get (Url);
  begin
    return GNATCOLL.JSON.Read (String'(AWS.Response.Message_Body (Response)));
  end GET;

  function Login (Self : in out Matrix) return JSON_Value is
    Data : Dict.Items := (
      (+"user", Create (- (Self.Username))),
      (+"password", Create (- (Self.Password))),
      (+"type", Create ("m.login.password")));
    Result : JSON_Value := Self.POST("login", Data);
  begin
    if UB.Length (Self.Access_Token) = 0 then
      Self.Access_Token := + (Result.Get ("access_token"));
    end if;
    return Result;
  end Login;

  function Join (Self : in out Matrix) return JSON_Value is
    Result : JSON_Value := Self.POST("join/" & (-Self.Room), Dict.Null_Data);
  begin
    Self.Room_ID := + (Result.Get ("room_id"));
    return Result;
  end Join;
end Connection;
