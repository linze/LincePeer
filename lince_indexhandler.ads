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
-- Foobar is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with LincePeer.  If not, see <http://www.gnu.org/licenses/>.
--
------------------------------------------------------------------------


with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Lince_Config;
with Lince_FileProtocol;
with Lince_IO;

package Lince_IndexHandler is

  package A_IO            renames Ada.Text_IO;
  package ASU             renames Ada.Strings.Unbounded;
  package ADir            renames Ada.Directories;
  package LConfig         renames Lince_Config;
  package LFileProtocol   renames Lince_FileProtocol;
  package LIO             renames Lince_IO;

  procedure WriteNewIndex ( FileName : in ASU.Unbounded_String;
                           FileSize : in Natural );
  function GetNumberOfParts ( FileSize : in  Natural ) return Natural;

  function GetNextBlock ( FileName  : in ASU.Unbounded_String;
                         Number    : in Positive := 1) return Natural;

  function GetRemainingBlocks ( FileName   : in ASU.Unbounded_String ) return Natural;

  procedure RemoveFromIndex ( FileName   : in ASU.Unbounded_String;
                              BlockPos   : in Positive);

  procedure RemoveIndex ( FileName    : in ASU.Unbounded_String );


end Lince_IndexHandler;
