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
with Lince_Config;
with Lince_Protocol;
with Lince_NodeProtocol;
with Lince_IO;
with gnutelight_contacts;

package Lince_NodeHandler is

  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package ACal             renames Ada.Calendar;
  package LConfig          renames Lince_Config;
  package LProtocol        renames Lince_Protocol;
  package LNodeProtocol    renames Lince_NodeProtocol;
  package LIO              renames Lince_IO;
  package GNULContacts     renames gnutelight_contacts;

  NodesSlots     : GNULContacts.Contacts_List_Type;
  ListReceived   : boolean;
  Tries          : Positive;
  Sent           : ACal.Time;

  procedure Connect ( ToHost  : in ASU.Unbounded_String;
                      ToPort  : in ASU.Unbounded_String);

  procedure SayHello ( To  : in LLU.End_Point_Type);

  procedure HandleHello ( From  : in LLU.End_Point_Type;
                          Hello : in LNodeProtocol.THello);

  procedure GiveWelcome ( To     : in LLU.End_Point_Type;
                          NodeEP : in LLU.End_Point_Type);

  procedure HandleWelcome ( From    : in LLU.End_Point_Type;
                            Welcome : in LNodeProtocol.TWelcome);


end Lince_NodeHandler;
