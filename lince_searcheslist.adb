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
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Lince_Config;
with Lince_Protocol;
with Lince_SearchProtocol;
with Lince_IndexHandler;
with Lince_IO;
with gnutelight_contacts;

use type Ada.Strings.Unbounded.Unbounded_String;
use type Ada.Calendar.Time;

package body Lince_SearchesList is

  procedure CreateSearchesList ( SearchesList : in out TSearchesList) is
  begin
    for i in 1 .. LConfig.MAX_SAVED_SEARCHES loop
      SearchesList (i).Status := stINACTIVE;
      SearchesList (i).FileName := ASU.To_Unbounded_String ("");
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","CreateSearchesList",Ex);
  end CreateSearchesList;

  procedure AddSearch ( FileName     : in ASU.Unbounded_String;
                        SearchesList : in out TSearchesList) is
    SPos : Natural;
  begin
    SPos := GetFreePosition (FileName, SearchesList);
    if SPos = 0 then
      LIO.VerboseDebug ("LSearchesList", "AddSearch",
                        "No free slots available.");
    elsif IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "AddSearch",
                        "Trying to add a already existing search.");
    else
      SearchesList (Positive(SPos)).Status := stPROGRESS;
      SearchesList (Positive(SPos)).FileName := FileName;
      SearchesList (Positive(SPos)).StartTime := ACal.Clock;
      SearchesList (Positive(SPos)).Tries := 1;
      SearchesList (Positive (SPos)).LastTime := ACal.Clock;
      LIO.VerboseDebug ("LSearchesList", "AddSearch",
                        "Added download: " & ASU.To_String(FileName));
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","AddSearch",Ex);
  end AddSearch;

  procedure UpdateTimeAndTries (FileName     : in ASU.Unbounded_String;
                                SearchesList : in out TSearchesList) is
    SPos : Positive;
  begin
    if not IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "UpdateTimeAndTries",
                        "Search not found !!.");
    else
      SPos := GetSearchPosition (FileName, SearchesList);

      SearchesList (SPos).Tries := SearchesList (SPos).Tries + 1;
      LIO.VerboseDebug ("LSearchesList", "UpdateTimeAndTries",
                        "Sent tries updated: #" & Positive'Image(SearchesList(SPos).Tries) &
                        " tries.");
      SearchesList (SPos).LastTime := ACal.Clock;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","UpdateTimeAndTries",Ex);
  end UpdateTimeAndTries;

  function GetSearchTries ( FileName     : in ASU.Unbounded_String;
                            SearchesList : in TSearchesList) return Positive is
    SPos : Natural;
  begin
    if not IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "GetSearchTries",
                        "WARNING: Search not found !!.");
      return LConfig.SEARCH_RETRIES+1;
    else
      SPos := GetSearchPosition (FileName, SearchesList);
      return SearchesList (SPos).Tries;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList", "GetSearchTries", Ex);
                        raise;
  end GetSearchTries;

  procedure MarkAsCompleted ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in out TSearchesList) is
    SPos : Natural;
  begin
    if not IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "MarkAsCompleted",
                        "Search not found !!.");
    else
      SPos := GetSearchPosition (FileName, SearchesList);
      SearchesList(SPos).Status := stCompleted;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","MarkAsCompleted",Ex);
  end MarkAsCompleted;

  procedure AddServer (Server       : in LLU.End_Point_Type;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList) is
    SPos : Positive;
  begin
    if not IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "AddServer",
                        "They are not such search");
    else
      SPos := Positive(GetSearchPosition (FileName, SearchesList));
      GNULContacts.Add_One (SearchesList (Positive (SPos)).Contacts, Server);
      LIO.VerboseDebug ("LSearchesList", "AddServer",
                        "Added server: " & LLU.Image (Server));
    end if;
    LIO.VerboseDebug ("LSearchesList", "AddServer",
                      "Servers count for this file: " &
                      Natural'Image (GNULContacts.Total (SearchesList (Positive (SPos)).Contacts)));
    -- TODO: Remove. Too verbose debugging
    PrintServers (FileName, SearchesList);
    LIO.VerboseDebug ("LSearchesList", "AddServer", "DownloadPos: " & Positive'Image(SPos));
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","AddServer",Ex);
  end AddServer;

  procedure AddServer (ServerHost   : in ASU.Unbounded_String;
                       ServerPort   : in ASU.Unbounded_String;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList) is
    From : LLU.End_Point_Type;
  begin
    LIO.VerboseDebug ("LSearchesList", "AddServer",
                      "Building a End_Point. Host: " &
                      LLU.To_IP (ASU.To_String (ServerHost)) &
                      " Port: " &
                      ASU.To_String (ServerPort));
    From := LLU.Build (LLU.To_IP (ASU.To_String (ServerHost)), Integer'Value (ASU.To_String (ServerPort)));
    AddServer (From, FileName, SearchesList);
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","AddServer",Ex);
  end AddServer;

  procedure GetServers ( To           : out GNULContacts.Contacts_List_Type;
                         FileName     : in ASU.Unbounded_String;
                         SearchesList : in TSearchesList) is
    SPos   : Positive;
  begin
    if not IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchesList", "GetServers",
                        "Search not found !!.");
    else
      SPos := GetSearchPosition (FileName, SearchesList);
      -- TODO: Remove this. Excesive debugging
      LIO.VerboseDebug ("LSearchesList", "GetServers",
                        "SPos: " & Positive'Image (SPos));
      PrintServers (FileName, SearchesList);


      GNULContacts.Copy (SearchesList (Positive(SPos)).Contacts, To);
      LIO.VerboseDebug ("LSearchesList", "GetServers",
                        "From total: " & Positive'Image(GNULContacts.Total (SearchesList (Positive (SPos)).Contacts)) &
                        " . To total: " & Positive'Image(GNULContacts.Total(To)));
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","GetServers",Ex);
                        raise;
  end GetServers;

  function GetFreePosition ( FileName     : in ASU.Unbounded_String;
                             SearchesList : in TSearchesList ) return Natural is
    SPos : Natural;
  begin
    LIO.VerboseDebug ("LSearchesList", "GetFreePosition",
                      "Searching a inactive position in the searches list...");
    SPos := GetInactivePosition (FileName, SearchesList);
    -- If the history is full, all positions are completed. So we will replace
    -- the completed.
    if SPos = 0 then
      LIO.VerboseDebug ("LSearchesList", "GetFreePosition",
                        "Not found inactive. Searching for completed...");
      SPos := GetCompletedPosition (FileName, SearchesList);
    end if;

    LIO.VerboseDebug ("LSearchesList", "GetFreePosition",
                      "Returning position " & Natural'Image(SPos));

    return SPos;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","GetFreePosition",Ex);
                        raise;
  end;

  function GetInactivePosition ( FileName     : in ASU.Unbounded_String;
                                 SearchesList : in TSearchesList) return Natural is
    Found   : boolean := False;
    SPos    : Natural := 1;
  begin
    while not Found and SPos <= LConfig.MAX_SAVED_SEARCHES loop
      Found := (SearchesList (Positive(SPos)).Status = stINACTIVE);
      if not Found then
        SPos := SPos + 1;
      end if;
    end loop;

    if not Found then
      return 0;
    else
      return Natural(SPos);
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","GetInactivePosition",Ex);
                        raise;
  end GetInactivePosition;

  function GetCompletedPosition ( FileName     : in ASU.Unbounded_String;
                                  SearchesList : in TSearchesList) return Natural is
    Found   : boolean := False;
    SPos    : Natural := 1;
  begin
    while not Found and SPos <= LConfig.MAX_SAVED_SEARCHES loop
      Found := (SearchesList (SPos).Status = stCOMPLETED);
      if not Found then
        SPos := SPos + 1;
      end if;
    end loop;

    if not Found then
      return 0;
    else
      return Natural(SPos);
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList","GetCompletedPosition",Ex);
                        raise;
  end GetCompletedPosition;

  function IsMessageTimedOut ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean is
    SPos         : Positive;
    Sent, Now    : ACal.Time;
    Wait         : Duration;
  begin
    SPos := GetSearchPosition (FileName, SearchesList);
    Sent := SearchesList (SPos).LastTime;
    Now  := ACal.Clock;
    Wait    := Now - Sent;

    return (Wait > LConfig.MAX_PACKET_TIMEOUT);
  end IsMessageTimedOut;

  function IsSearchTimedOut ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in TSearchesList) return boolean is
    SPos         : Positive;
    Sent, Now    : ACal.Time;
    Wait         : Duration;
    TimeToWait   : Duration;
  begin
    SPos    := GetSearchPosition (FileName, SearchesList);
    Sent    := SearchesList (SPos).LastTime;
    Now     := ACal.Clock;
    Wait    := Now - Sent;

    TimeToWait := LConfig.SEARCH_RETRIES * LConfig.MAX_PACKET_TIMEOUT +
                  LConfig.SEARCH_START_TTL * LConfig.MAX_PACKET_TIMEOUT;

    return (Wait > TimeToWait);
  end IsSearchTimedOut;

  function IsSearchRequested ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean is
    Exists : boolean;
    Pos    : Positive;
  begin
    Exists := False;
    Pos    := 1;
    while not Exists and Pos <= LConfig.MAX_SAVED_SEARCHES loop
      if SearchesList (Pos).FileName = FileName then
        Exists := True;
      else
        Pos := Pos + 1;
      end if;
    end loop;

    return Exists;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList", "IsSearchRequested", Ex);
                        raise;
  end IsSearchRequested;

  function GetSearchPosition ( FileName    : in ASU.Unbounded_String;
                               SearchesList: in TSearchesList ) return Positive is
    Found : boolean := False;
    SPos  : Positive := 1;
  begin
    while not Found and SPos <= LConfig.MAX_SAVED_SEARCHES loop
      Found := (SearchesList (SPos).FileName = FileName);
      if not Found then
        SPos := SPos + 1;
      end if;
    end loop;

   return SPos;

  exception
    when Ex : others => LIO.DebugError ("LSearchesList", "GetSearchPosition", Ex);
                        raise;
  end GetSearchPosition;

  procedure PrintServers ( FileName    : in ASU.Unbounded_String;
                          SearchesList : in TSearchesList ) is
    SPos       : Positive;
    NodesCount : Natural;
  begin
    SPos       := GetSearchPosition (FileName, SearchesList);
    NodesCount := GNULContacts.Total (SearchesList (SPos).Contacts);
    LIO.VerboseDebug ("LSearchesList", "PrintServers",
                      Natural'Image (NodesCount) &
                      " servers for " & ASU.To_String (FileName) &
                      " Pos " & Positive'Image(SPos) & " :");
    for i in 1 ..  NodesCount loop
      LIO.VerboseDebug ("LSearchesList", "PrintServers",
                        "   |--- " &
                        LLU.Image (GNULContacts.Get_One (SearchesList (SPos).Contacts, i)));
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LSearchesList", "PrintServers", Ex);
                        raise;
  end PrintServers;


end Lince_SearchesList;
