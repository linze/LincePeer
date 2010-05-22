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


with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Exceptions;
with Ada.Calendar;
with Ada.Directories;
with Lince_Config;
with Lince_Protocol;
with Lince_SearchProtocol;
with Lince_NodeHandler;
with Lince_IO;
with Lince_SearchesList;
with gnutelight_contacts;

package Lince_SearchHandler is

  package LLU                 renames Lower_Layer_UDP;
  package ASU                 renames ADA.Strings.Unbounded;
  package ACal                renames Ada.Calendar;
  package ADir                renames Ada.Directories;
  package LConfig             renames Lince_Config;
  package LProtocol           renames Lince_Protocol;
  package LSearchProtocol     renames Lince_SearchProtocol;
  package LNodeHandler        renames Lince_NodeHandler;
  package LIO                 renames Lince_IO;
  package LSearchesList       renames Lince_SearchesList;
  package GNULContacts        renames gnutelight_contacts;

  SearchesList    : LSearchesList.TSearchesList;

  procedure StartSearch  ( FileName      : in ASU.Unbounded_String );
  procedure HandleSearch ( From      : in LLU.End_Point_Type;
                           Search    : in LSearchProtocol.TSearch);
  procedure HandleGotIt ( From      : in LLU.End_Point_Type;
                          GotIt     : in LSearchProtocol.TGotIt);

end Lince_SearchHandler;
