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
with ADA.Strings.Unbounded;
with ADA.Streams;
with ADA.Exceptions;
with Ada.IO_Exceptions;

with Lince_Protocol;
with Lince_IO;
with gnutelight_contacts;

package Lince_NodeProtocol is

  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package AS               renames ADA.Streams;
  package LProtocol        renames Lince_Protocol;
  package LIO              renames Lince_IO;
  package GNULContacts     renames gnutelight_contacts;

  -- Buffer sizes
  HELLOBUFFER    : constant Positive := 1024;
  WELCOMEBUFFER  : constant Positive := 1024;

  -- Max hello retries
  MAXHELLORETRIES: constant Positive := 3;


  type THello   is record
    Options      : Natural;
    EPres        : LLU.End_Point_Type;
    EPsvc        : LLU.End_Point_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;


  type TWelcome is record
    Options      : Natural;
    N            : Natural;
    Peers        : GNULContacts.Contacts_List_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;

  -- Send methods
  -- Sends a record thought a End_Point

  procedure SendHello  ( To       : in LLU.End_Point_Type;
                         Hello    : in THello);

  procedure SendWelcome( To       : in LLU.End_Point_Type;
                         Welcome  : in TWelcome);

  -- Get methods
  -- Transform the data flow into a record.
  procedure GetHello   ( Buffer   : access LLU.Buffer_Type;
                         Hello    : out THello);

  procedure GetWelcome ( Buffer   : access LLU.Buffer_Type;
                         Welcome  : out TWelcome);

  -- Record to string conversion record
  -- Places all the record information in a string to ve
  -- displayed at logs or screen
  function HelloToString      ( Hello   : in THello ) return String;
  function WelcomeToString    ( Welcome : in TWelcome ) return String;

end Lince_NodeProtocol;
