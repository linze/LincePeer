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


with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use type Ada.Strings.Unbounded.Unbounded_String;

package body Lince_Protocol is

  function ClearLLUImage ( EP   : in LLU.End_Point_Type) return ASU.Unbounded_String is
    Pos     : Natural;
    TmpStr  : ASU.Unbounded_String;
  begin
    -- Given:
    -- LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: <ip>, Port:  <port>
    -- Returns: IP: <ip>, Port:  <port>
    --
    TmpStr := ASU.To_Unbounded_String(LLU.Image (EP));
    Pos    := ASU.Index (TmpStr, " ");
    return ASU.Tail (TmpStr, ASU.Length(TmpStr) - Pos);
  end ClearLLUImage;

  function GetIP (EP : in LLU.End_Point_Type) return ASU.Unbounded_String is
    Pos    : Natural;
    TmpStr : ASU.Unbounded_String;
  begin
    --IP: <ip>, Port:  <port>
    TmpStr := ClearLLUImage (EP);

    Pos    := ASU.Index (TmpStr, " ");
    TmpStr := ASU.Tail (TmpStr, ASU.Length (TmpStr) - Pos);
    -- <ip>, Port:  <port>
    Pos    := ASU.Index (TmpStr, ",");
    TmpStr := ASU.Head (TmpStr, Pos - 1);
    return TmpStr;
  end GetIP;

  function IsSameIP ( EP1 : in LLU.End_Point_Type;
                     EP2 : in LLU.End_Point_Type) return boolean is
  begin
    return GetIP (EP1) = GetIP(EP2);
  end IsSameIP;



end Lince_Protocol;
