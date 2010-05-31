------------------------------------------------------------------------
--  LincePeer
--  Copyright 2010 Francisco Canela Gonzalez
--
-- This file is part of LincePeer.
--
-- LincePeer is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- LincePeer is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with LincePeer.  If not, see <http://www.gnu.org/licenses/>.
--
------------------------------------------------------------------------


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
