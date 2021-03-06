package body Dict is
  function To_Json (Dict : Items) return String is
    Result : GJ.JSON_Value := GJ.Create_Object;
  begin
    if Dict'Length = 0 then
      return "{}";
    end if;

    for Item of Dict loop
      GJ.Set_Field (Result, - (Item.Key), Item.Value);
    end loop;
    return Result.Write;
  end To_Json;
end Dict;
