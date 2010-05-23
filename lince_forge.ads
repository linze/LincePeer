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


with ADA.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with ADA.Text_IO;
with ADA.Streams;
with Lower_Layer_UDP;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IO;


package Lince_Forge is

  package ASU              renames ADA.Strings.Unbounded;
  package ASU_IO           renames ADA.Strings.Unbounded.Text_IO;
  package A_IO             renames ADA.Text_IO;
  package AS               renames ADA.Streams;
  package LLU              renames Lower_Layer_UDP;
  package LProtocol        renames Lince_Protocol;
  package LFileProtocol    renames Lince_FileProtocol;
  package LIO              renames Lince_IO;


  -- Data forge
  procedure ForgeDataReq;
  procedure ForgeData;
  procedure ForgeDataErr;

  -- Nodes forge
  -- Search forge
end Lince_Forge;
