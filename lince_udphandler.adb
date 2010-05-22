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
with Ada.Exceptions;
with Lince_Console;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_NodeProtocol;
with Lince_SearchProtocol;
with Lince_FileHandler;
with Lince_NodeHandler;
with Lince_SearchHandler;
with Lince_Forge;
with Lince_IO;
with Lince_Config;
with Lince_DownloadsList;
with Lince_SearchesList;


package body Lince_UDPHandler is


  -- Make the UDP server listen and prepared to receive messages.
  procedure RunServer is
  begin
    LIO.VerboseDebug ("LUDPHandler", "RunServer", "Preparing download list...");
    LDownloadsList.CreateDownloadList (LFileHandler.DownloadsSlots);
    LIO.VerboseDebug ("LUDPHandler", "RunServer", "Preparing searches list...");
    LSearchesList.CreateSearchesList (LSearchHandler.SearchesList);

    LIO.VerboseDebug ("LUDPHandler", "RunServer", "Creating local End_Point...");
    -- Create the local server End-Point
    LProtocol.EP_localserver := LLU.Build (LLU.To_IP (LLU.Get_Host_Name), Integer (ListeningPort));
    -- ... and start listening. PeerHandler method will be triggered
    -- when a message arrives
    LIO.VerboseDebug ("LUDPHandler", "RunServer", "Start listening...");
    -- Create the local server End-Point
    LProtocol.EP_localserver := LLU.Build (LLU.To_IP (LLU.Get_Host_Name), Integer (ListeningPort));
    LLU.Bind (LProtocol.EP_localserver, UDPHandler'Access);
    LIO.Notify ("Server activated.", LIO.mtINFORMATION);
    LIO.VerboseDebug ("LUDPHandler", "RunServer"
                      , "Created End_Point. Listening at:"
                      & LLU.Get_Host_Name
                      & "(" & LLU.To_IP (LLU.Get_Host_Name) & ")"
                      & " Port: " & Integer'Image (ListeningPort));
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler","RunServer",Ex);
  end RunServer;

  -- Method to receive a message, identify the type and
  -- start their related actions procedure.
  procedure UDPHandler ( From     : in LLU.End_Point_Type;
                         To       : in LLU.End_Point_Type;
                         Buffer   : access LLU.Buffer_Type ) is
    PetitionCmd  : LProtocol.TMessage_Type;
  begin
    LIO.VerboseDebug ("LUDPHandler", "UDPHandler"
                      , "Package received from " & LLU.Image (From) & ".");
    PetitionCmd := LProtocol.TMessage_Type'Input (Buffer);

    case PetitionCmd is
    when LProtocol.DATAREQ => ProcessDataReq (From, Buffer);
    when LProtocol.DATA    => ProcessData    (From, Buffer);
    when LProtocol.DATAERR => ProcessDataErr (From, Buffer);
    when LProtocol.HELLO   => ProcessHello (From, Buffer);
    when LProtocol.WELCOME => ProcessWelcome (From, Buffer);
    when LProtocol.SEARCH  => ProcessSearch (From, Buffer);
    when LProtocol.GOTIT   => ProcessGotIt (From, Buffer);
    when others  => LIO.VerboseDebug ("LUDPHandler","UDPHandler", "Unknown package type.");
    end case;

   exception
    when Ex : others => LIO.DebugError ("LUDPHandler","UDPHandler",Ex);
  end UDPHandler;


  -- Methods to coordinate the actions related to a message

  -- Data protocol

  -- DataREQ messages
  procedure ProcessDataReq (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    DataRequest    : LFileProtocol.TDataReq;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessDataReq message", "Processing DataReq");
    LFileProtocol.GetDataReq (Buffer, DataRequest);

    LIO.VerboseDebug ("LUDPHandler", "ProcessDataReq", "Serving block ");
    LFileHandler.ServeBlock (DataRequest);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessDataReq", Ex);
  end ProcessDataReq;

  -- Data messages
  procedure ProcessData   (From   : in LLU.End_Point_Type;
                           Buffer : access LLU.Buffer_Type) is
    Data           : LFileProtocol.TData;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessData", "Processing Data message");
    LFileProtocol.GetData (Buffer, Data);
    LFileHandler.ManageDownload (From, Data);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessData", Ex);
  end ProcessData;

  -- DataERR messages
  procedure ProcessDataErr (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    DataErr    : LFileProtocol.TDataErr;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessDataReq", "Processing DataReq");
    LFileProtocol.GetDataErr (Buffer, DataErr);
    LFileHandler.HandleDataErr (From, DataErr);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessDataErr", Ex);
  end ProcessDataErr;

  -- HELLO messages
  procedure ProcessHello   (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    Hello    : LNodeProtocol.THello;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessHello", "Processing Hello");
    LNodeProtocol.GetHello (Buffer, Hello);
    LNodeHandler.HandleHello (From, Hello);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessHello", Ex);
  end ProcessHello;

  -- WELCOME messages
  procedure ProcessWelcome (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    Welcome    : LNodeProtocol.TWelcome;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessWelcome", "Processing Welcome");
    LNodeProtocol.GetWelcome (Buffer, Welcome);
    LNodeHandler.HandleWelcome (From, Welcome);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessWelcome", Ex);
  end ProcessWelcome;

  -- SEARCH messages
  procedure ProcessSearch  (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    Search    : LSearchProtocol.TSearch;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessSearch", "Processing Search");
    LSearchProtocol.GetSearch (Buffer, Search);
    LSearchHandler.HandleSearch (From, Search);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessSearch", Ex);
  end ProcessSearch;

  -- GOTIT messages
  procedure ProcessGotIt   (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type) is
    GotIt    : LSearchProtocol.TGotIt;
  begin
    LIO.VerboseDebug ("LUDPHandler", "ProcessGotIt", "Processing GotIt");
    LSearchProtocol.GetGotIt (Buffer, GotIt);
    LSearchHandler.HandleGotIt (From, GotIt);
  exception
    when Ex : others => LIO.DebugError ("LUDPHandler", "ProcessGotIt", Ex);
  end  ProcessGotIt;

end Lince_UDPHandler;
