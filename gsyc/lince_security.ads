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
with Lince_IO;
with Lince_Protocol;
with Lower_Layer_UDP;
with gnutelight_contacts;

package Lince_Security is

  package ASU              renames Ada.Strings.Unbounded;
  package A_IO             renames Ada.Text_IO;
  package ADir             renames Ada.Directories;
  package LIO              renames Lince_IO;
  package LProtocol        renames Lince_Protocol;
  package LLU              renames Lower_Layer_UDP;
  package GNULContacts     renames gnutelight_contacts;

  procedure AddBlacklistedEP (EP : in LLU.End_Point_Type);
  function IsBlacklisted (EP : in LLU.End_Point_Type) return boolean;
  function IsTrasversalDirectory ( FileName : ASU.Unbounded_String ) return boolean;

private
  BlackListedEPs  : GNULContacts.Contacts_List_Type;

end Lince_Security;
