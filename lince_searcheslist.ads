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
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Lince_Config;
with Lince_Protocol;
with Lince_SearchProtocol;
with Lince_IndexHandler;
with Lince_IO;
with gnutelight_contacts;

package Lince_SearchesList is


  package LLU             renames Lower_Layer_UDP;
  package ACal            renames Ada.Calendar;
  package ASU             renames Ada.Strings.Unbounded;
  package LConfig         renames Lince_Config;
  package LProtocol       renames Lince_Protocol;
  package LSearchProtocol renames Lince_SearchProtocol;
  package LIndexHandler   renames Lince_IndexHandler;
  package LIO             renames Lince_IO;
  package GNULContacts    renames gnutelight_contacts;

  type TSearchStatus is (stINACTIVE, stPROGRESS, stCOMPLETED);

  type TSearchItem is record
    Status     : TSearchStatus;
    FileName   : ASU.Unbounded_String;
    StartTime  : ACal.Time;
    Contacts   : GNULContacts.Contacts_List_Type;
    Tries      : Positive;
    LastTime   : ACal.Time;
  end record;

  type TSearchesList is array (1 .. LConfig.MAX_SAVED_SEARCHES) of TSearchItem;

  procedure CreateSearchesList ( SearchesList : in out TSearchesList);

  procedure AddSearch ( FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure UpdateTimeAndTries (FileName     : in ASU.Unbounded_String;
                                SearchesList : in out TSearchesList);

  function GetSearchTries ( FileName     : in ASU.Unbounded_String;
                            SearchesList : in TSearchesList) return Positive;

  procedure MarkAsCompleted ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in out TSearchesList);

  procedure AddServer (Server       : in LLU.End_Point_Type;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure AddServer (ServerHost   : in ASU.Unbounded_String;
                       ServerPort   : in ASU.Unbounded_String;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure GetServers ( To           : out GNULContacts.Contacts_List_Type;
                         FileName     : in ASU.Unbounded_String;
                         SearchesList : in TSearchesList);

  function IsMessageTimedOut ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean;

  function IsSearchTimedOut ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in TSearchesList) return boolean;

  function IsSearchRequested ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean;

  function GetSearchPosition ( FileName    : in ASU.Unbounded_String;
                               SearchesList: in TSearchesList ) return Positive;

  procedure PrintServers ( FileName    : in ASU.Unbounded_String;
                           SearchesList : in TSearchesList );
private
  function GetFreePosition ( FileName     : in ASU.Unbounded_String;
                             SearchesList : in TSearchesList )  return Natural;
  function GetInactivePosition ( FileName     : in ASU.Unbounded_String;
                                SearchesList : in TSearchesList) return Natural;
  function GetCompletedPosition ( FileName     : in ASU.Unbounded_String;
                                  SearchesList : in TSearchesList) return Natural;
end Lince_SearchesList;
