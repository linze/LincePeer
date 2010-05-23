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
with Lince_Security;


package Lince_UDPHandler is


  package LLU              renames Lower_Layer_UDP;
  package ASU              renames Ada.Strings.Unbounded;
  package LConsole         renames Lince_Console;
  package LProtocol        renames Lince_Protocol;
  package LFileProtocol    renames Lince_FileProtocol;
  package LNodeProtocol    renames Lince_NodeProtocol;
  package LSearchProtocol  renames Lince_SearchProtocol;
  package LFileHandler     renames Lince_FileHandler;
  package LNodeHandler     renames Lince_NodeHandler;
  package LSearchHandler   renames Lince_SearchHandler;
  package LForge           renames Lince_Forge;
  package LIO              renames Lince_IO;
  package LConfig          renames Lince_Config;
  package LDownloadsList   renames Lince_DownloadsList;
  package LSearchesList    renames Lince_SearchesList;
  package LSecurity        renames Lince_Security;

  procedure RunServer;

private

  procedure UDPHandler ( From     : in LLU.End_Point_Type;
                         To       : in LLU.End_Point_Type;
                        Buffer   : access LLU.Buffer_Type );

  -- Processing messages triggers
  -- Data protocol
  procedure ProcessDataReq (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type);
  procedure ProcessData    (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type);
  procedure ProcessDataErr (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type);

  -- Node protocol
  procedure ProcessHello   (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type);
  procedure ProcessWelcome (From   : in LLU.End_Point_Type;
                            Buffer : access LLU.Buffer_Type);

  -- Search protocol
    procedure ProcessSearch  (From   : in LLU.End_Point_Type;
                              Buffer : access LLU.Buffer_Type);

    procedure ProcessGotIt   (From   : in LLU.End_Point_Type;
                              Buffer : access LLU.Buffer_Type);
end Lince_UDPHandler;
