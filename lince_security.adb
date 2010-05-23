with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Directories;
with Lower_Layer_UDP;
with Lince_IO;
with Lince_Protocol;
with gnutelight_contacts;

package body Lince_Security is

  procedure AddBlacklistedEP (EP : in LLU.End_Point_Type) is
  begin
    GNULContacts.Add_One (BlackListedEPs, EP);
  exception
    when Ex : others => LIO.DebugError ("LSecurity", "AddBlacklistedEP", Ex);
  end AddBlacklistedEP;

  function IsBlacklisted (EP : in LLU.End_Point_Type) return boolean is
    Result : boolean;
  begin
    Result := False;
    for i in 1 .. GNULContacts.Total (BlackListedEPs) loop
      Result := LProtocol.IsSameIP (EP, GNULContacts.Get_One (BlackListedEPs, i));
      if Result = True then
        return Result;
      end if;
    end loop;

    return Result;
  exception
    when Ex : others =>
      LIO.DebugError ("LSecurity", "IsBlacklisted", Ex);
      return True;
  end IsBlacklisted;

  -- Checks that files outside the sharing directory are not served
  function IsTrasversalDirectory ( FileName : ASU.Unbounded_String ) return boolean is
    Pos  : Natural;
  begin
    -- Only with this one should be OK
    -- Linux check:
    Pos  := ASU.Index (FileName, "/");
    if Pos /= 0 then
      return True;
    end if;
    -- Windows check:
    Pos  := ASU.Index (FileName, "\");
    if Pos /= 0 then
      return True;
    end if;
    -- At this point, no further check should be needed. But just in case...
    Pos  := ASU.Index (FileName, "..");
    if Pos /= 0 then
      return True;
    end if;
    Pos  := ASU.Index (FileName, "~");
    if Pos /= 0 then
      return True;
    end if;

    return False;
  end IsTrasversalDirectory;

end Lince_Security;
