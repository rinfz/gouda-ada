with GNATCOLL.JSON;

pragma Elaborate_All (GNATCOLL.JSON);

package Config is
  function Read_Config return GNATCOLL.JSON.JSON_Value;
end Config;
