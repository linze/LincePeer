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
with Lince_Protocol;
with Lince_IO;
with gnutelight_contacts;

package Lince_SearchProtocol is

  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package AS               renames ADA.Streams;
  package LProtocol        renames Lince_Protocol;
  package LIO              renames Lince_IO;
  package GNULContacts     renames gnutelight_contacts;

  type TSearch  is record
    Options      : Natural;
    TTL          : Natural;
    EPRes        : LLU.End_Point_Type;
    FileName     : ASU.Unbounded_String;
    EPSvc        : LLU.End_Point_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;

  type TGotIt   is record
    Options      : Natural;
    FileName     : ASU.Unbounded_String;
    EPSvc        : LLU.End_Point_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;


  -- Buffer sizes
  SEARCHBUFFER    : constant Positive := 1024;
  GOTITBUFFER  : constant Positive := 1024;

  -- Send methods
  -- Sends a record thought a End_Point

  procedure SendSearch ( To       : in LLU.End_Point_Type;
                         Search   : in TSearch);

  procedure SendGotIt  ( To       : in LLU.End_Point_Type;
                         GotIt    : in TGotIt);

  -- Get methods
  -- Transform the data flow into a record.
  procedure GetSearch  ( Buffer   : access LLU.Buffer_Type;
                         Search   : out TSearch);

  procedure GetGotIt ( Buffer   : access LLU.Buffer_Type;
                       GotIt    : out TGotIt);

  -- Record to string conversion record
  -- Places all the record information in a string to ve
  -- displayed at logs or screen
  function SearchToString      ( Search  : TSearch ) return String;
  function GotItToString       ( GotIt   : TGotIt ) return String;


end Lince_SearchProtocol;
