with AWS.Client;
with AWS.Response;
with Ada.Strings;
with Ada.Strings.Fixed;

pragma Elaborate_All (AWS.Client);
pragma Elaborate_All (AWS.Response);

package body Connection is
  function Create (Config : JSON_Value) return Matrix is
    Room : UB.Unbounded_String := Config.Get ("room");
    Hash_Idx : constant Positive := UB.Index (Room, "#", 1);
    Colon_Idx : constant Positive := UB.Index (Room, ":", 1);
  begin
    UB.Replace_Slice (Room, Hash_Idx, Hash_Idx, "%23");
    -- Adjust indexes to account for mutation with Hash_Idx
    UB.Replace_Slice (Room, Colon_Idx + 2, Colon_Idx + 2, "%3A");
    return Matrix'(
      Base_Url     => + (Config.Get ("address")),
      Username     => + (Config.Get ("username")),
      Password     => + (Config.Get ("password")),
      Room         =>    Room,
      User_ID      => + (""),
      Room_ID      => + (""),
      Access_Token => + (""),
      Next_Batch   => + (""),
      Filter       => + (""),
      TX_ID        => 0
    );
  end Create;

  function Get_Access_Token (Self : Matrix) return UB.Unbounded_String is
  begin
    return Self.Access_Token;
  end Get_Access_Token;

  function Get_User_ID (Self : Matrix) return UB.Unbounded_String is
  begin
    return Self.User_ID;
  end Get_User_ID;

  function Get_Room_ID (Self : Matrix) return UB.Unbounded_String is
  begin
    return Self.Room_ID;
  end Get_Room_ID;

  function Get_Filter (Self : Matrix) return UB.Unbounded_String is
  begin
    return Self.Filter;
  end Get_Filter;

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
    Url : constant String := Self.Build_Url (Endpoint, Parameters, Version);
    Response : constant AWS.Response.Data := AWS.Client.Post (
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
    Url : constant String := Self.Build_Url (Endpoint, Parameters, Version);
    Response : constant AWS.Response.Data := AWS.Client.Get (Url);
  begin
    return GNATCOLL.JSON.Read (String'(AWS.Response.Message_Body (Response)));
  end GET;

  function PUT (Self : Matrix;
                Endpoint : String;
                Data : Dict.Items;
                Parameters : Params.Map := Params.Empty_Map;
                Version : String := "unstable")
  return JSON_Value is
    Url : constant String := Self.Build_Url (Endpoint, Parameters, Version);
    Response : constant AWS.Response.Data := AWS.Client.Put (Url, Dict.To_Json (Data));
  begin
    return GNATCOLL.JSON.Read (String'(AWS.Response.Message_Body (Response)));
  end PUT;

  procedure Login (Self : in out Matrix) is
    Data : constant Dict.Items := (
      (+"user", Create (-Self.Username)),
      (+"password", Create (-Self.Password)),
      (+"type", Create ("m.login.password")));
    Result : constant JSON_Value := Self.POST("login", Data);
  begin
    Self.Access_Token := +Result.Get ("access_token");
    Self.User_ID := +Result.Get ("user_id");
  end Login;

  procedure Join (Self : in out Matrix) is
    Result : constant JSON_Value := Self.POST("join/" & (-Self.Room), Dict.Null_Data);
  begin
    Self.Room_ID := + (Result.Get ("room_id"));
  end Join;

  procedure Upload_Filter (Self : in out Matrix) is
    Type_Filter : constant Dict.Item :=
      (+"account_data", Read ("{""types"":[""m.room.message""]}"));
    Room_Filter : constant Dict.Item :=
      (+"room", Read ("{""rooms"":[""" & (-Self.Room_ID) & """]}"));
    Data : constant Dict.Items := Dict.Items'(Type_Filter, Room_Filter);
    Result : constant JSON_Value := Self.POST("user/" & (-Self.User_ID) & "/filter", Data);
  begin
    Self.Filter := +Result.Get ("filter_id");
  end Upload_Filter;

  function Sync (Self : in out Matrix) return JSON_Value is
    Parameters : Params.Map;
  begin
    Parameters.Include ("filter", -Self.Filter);
    if UB.Length (Self.Next_Batch) > 0 then
      Parameters.Include ("since", -Self.Next_Batch);
    end if;

    declare
      Result : constant JSON_Value := Self.GET("sync", Parameters);
    begin
      Self.Next_Batch := +Result.Get ("next_batch");
      return Result;
    end;
  end Sync;

  procedure Send_Message (Self : in out Matrix; Message : String) is
    Data : constant Dict.Items := (
      (+"body", Create (Message)),
      (+"msgtype", Create ("m.text")));
    Result : JSON_Value :=
      Self.PUT("rooms/"
               & (-Self.Room_ID)
               & "/send/m.room.message/"
               & Ada.Strings.Fixed.Trim(Natural'Image(Self.TX_ID),
                                        Ada.Strings.Left),
               Data);
    pragma Unreferenced (Result);
  begin
    Self.TX_ID := Self.TX_ID + 1;
  end Send_Message;
end Connection;
