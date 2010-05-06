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
with gnutelight_contacts;

use type Ada.Strings.Unbounded.Unbounded_String;
use type Lower_Layer_UDP.End_Point_Type;

package body Lince_SearchHandler is

  -- TODO: That protocol waste MAX_PACKET_TIMEOUT ms without necessity.
  -- A packet should be send just after adding the search to the list
  -- and before accesing the main while loop.
  procedure StartSearch  ( FileName      : in ASU.Unbounded_String) is
    Search    : LSearchProtocol.TSearch;
    Tries     : Positive;
    ToEP      : LLU.End_Point_Type;
  begin
    LIO.VerboseDebug ("LSearchHandler", "StartSearch", "Creating the search packet...");
    -- Create the package that will be send SEARCH_RETRIES times.
    Search.Options  := 0;
    Search.TTL      := LConfig.SEARCH_START_TTL;
    Search.EPRes    := LProtocol.EP_localserver;
    Search.FileName := FileName;
    Search.EPSvc    := LProtocol.EP_localserver;

    -- Add the search to the search history
    if not LSearchesList.IsSearchRequested (FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchHandler", "StartSearch", "Including search in the search history...");
      LSearchesList.AddSearch (FileName, SearchesList);
    end if;

    LIO.VerboseDebug ("LSearchHandler", "StartSearch", "Sending packets...");
    -- While isn't search timeout
    while not LSearchesList.IsSearchTimedOut (FileName, SearchesList) loop
      Tries := Positive(LSearchesList.GetSearchTries (FileName, SearchesList));
      -- ... and tries are less than the limit
      while (Tries <= LConfig.SEARCH_RETRIES) loop
        Tries := Positive(LSearchesList.GetSearchTries (FileName, SearchesList));
        -- Send a message for each retry established
        if LSearchesList.IsMessageTimedOut (FileName, SearchesList) then
          -- ... to all the contacts in the nodes list.
          for i in 1 .. GNULContacts.Total (LNodeHandler.NodesSlots) loop
            ToEP       := GNULContacts.Get_One (LNodeHandler.NodesSlots, i);
            if ToEP /= LProtocol.EP_localserver then
              LSearchProtocol.SendSearch (ToEP, Search);
            end if;
          end loop;
          -- And update time and sum one to the number of tries
          LSearchesList.UpdateTimeAndTries(FileName,SearchesList);
        end if;
      end loop;
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LSearchHandler","StartSearch",Ex);
  end StartSearch;

  procedure HandleSearch ( From      : in LLU.End_Point_Type;
                           Search    : in LSearchProtocol.TSearch) is
    GotIt      : LSearchProtocol.TGotIt;
    ToEP       : LLU.End_Point_Type;
    NewSearch  : LSearchProtocol.TSearch;
  begin
    if (Search.EPRes = LProtocol.EP_localserver)
      or (Search.EPSvc = LProtocol.EP_localserver) then
      null;
    else
      -- Propagate the message
      NewSearch := Search;
      NewSearch.TTL := Search.TTL - 1;
      -- Send search to all known nodes
      for i in 1 .. GNULContacts.Total (LNodeHandler.NodesSlots) loop
        ToEP       := GNULContacts.Get_One (LNodeHandler.NodesSlots, i);
        -- ... that aren't the starter node
        if (ToEP /= Search.EPSvc) then
          LSearchProtocol.SendSearch (ToEP, NewSearch);
        end if;
      end loop;

      -- If having the file, reply
      -- TODO: If including trasversal directory checking. Change this
      if ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & Search.FileName)) then
        GotIt.Options     := 0;
        GotIt.FileName    := Search.FileName;
        GotIt.EPSvc       := LProtocol.EP_localserver;
        LSearchProtocol.SendGotIt (Search.EPRes,GotIt);
      end if;
    end if;
  end HandleSearch;


  procedure HandleGotIt ( From      : in LLU.End_Point_Type;
                          GotIt     : in LSearchProtocol.TGotIt) is
  begin
    if not LSearchesList.IsSearchRequested (GotIt.FileName, SearchesList) then
      LIO.VerboseDebug ("LSearchHandler", "HandlerGotIt", "Received unrequested GotIt");
    elsif GotIt.EPSvc = LProtocol.EP_localserver then
      LIO.VerboseDebug ("LSearchHandler", "HandlerGotIt", "Received self GotIt!!!");
    else
      LSearchesList.AddServer (GotIt.EPSvc, GotIt.FileName, SearchesList);
      GNULContacts.Add_One (LNodeHandler.NodesSlots, GotIt.EPSvc);
    end if;
  exception
    when Ex : others => LIO.DebugError ("LSearchHandler","HandleGotIt",Ex);
  end HandleGotIt;


end Lince_SearchHandler;
