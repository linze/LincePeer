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
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Lince_Config;
with Lince_UDPHandler;
with Lince_FileHandler;
with Lince_NodeHandler;
with Lince_SearchHandler;
with Lince_SearchesList;
with Lince_Console;
with Lince_IO;


use type Ada.Strings.Unbounded.Unbounded_String;


procedure Lince is

  package LLU            renames Lower_Layer_UDP;
  package ASU            renames Ada.Strings.Unbounded;
  package LConfig        renames Lince_Config;
  package LUDPHandler    renames Lince_UDPHandler;
  package LFileHandler   renames Lince_FileHandler;
  package LNodeHandler   renames Lince_NodeHandler;
  package LSearchHandler renames Lince_SearchHandler;
  package LSearchesList  renames Lince_SearchesList;
  package LConsole       renames Lince_Console;
  package LIO            renames Lince_IO;


  Arguments    : Natural;
begin
  Arguments := ADA.Command_Line.Argument_Count;
  if Arguments >= 5 then
    LIO.StartLog;
    LConfig.LISTENINGPORT := Positive'Value (ADA.Command_Line.Argument (1));
    LConfig.SHARINGDIR := ASU.To_Unbounded_String (ADA.Command_Line.Argument (2) & "/");
    LConfig.MINPROPAGATIONDELAY := Integer'Value (ADA.Command_Line.Argument (3));
    LConfig.MAXPROPAGATIONDELAY := Integer'Value (ADA.Command_Line.Argument (4));
    LConfig.MAX_PACKET_TIMEOUT := Duration (2 * Integer'Value (ADA.Command_Line.Argument (4)) / 1000);
    LLU.Set_Random_Propagation_Delay (LConfig.MINPROPAGATIONDELAY, LConfig.MAXPROPAGATIONDELAY);
    LConfig.FAULTSPERCENT := Integer'Value (ADA.Command_Line.Argument (5));
    LLU.Set_Faults_Percent (LConfig.FAULTSPERCENT);
  end if;

  case Arguments is
    -- Startup from config file
    when 0 =>
      LIO.StartLog;
      LIO.VerboseDebug ("Lince", "Main", "Startup. Loading configuration file.");
      LConfig.LoadConfig;
      LLU.Set_Random_Propagation_Delay (LConfig.MINPROPAGATIONDELAY, LConfig.MAXPROPAGATIONDELAY);
      LLU.Set_Faults_Percent (LConfig.FAULTSPERCENT);
      LUDPHandler.RunServer;
      LConsole.StartConsole;
    -- Startup as server
    when 5 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server");
      LUDPHandler.RunServer;
      LConsole.StartConsole;
    -- Startup as server with windows size.
    when 6 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server with windows size");
      -- Windows size
      LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD := Positive'Value (ADA.Command_Line.Argument (6));
      LUDPHandler.RunServer;
      LConsole.StartConsole;
    -- Start up with widows size and starting node
    when 8 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server with windows size and starting node");
      -- Windows size
      LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD := Positive'Value (ADA.Command_Line.Argument (6));
      LUDPHandler.RunServer;
      -- Get nodes
      LNodeHandler.Connect (ASU.To_Unbounded_String (ADA.Command_Line.Argument (7)),
                            ASU.To_Unbounded_String (ADA.Command_Line.Argument (8)));
      LConsole.StartConsole;
    -- Start up with windows size and direct node download
    when 9 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as peer downloading a file directly");
      -- Windows size
      LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD := Positive'Value (ADA.Command_Line.Argument (6));
      LUDPHandler.RunServer;
      -- Direct file download
      LSearchesList.AddSearch (ASU.To_Unbounded_String (ADA.Command_Line.Argument (9)),
                               LSearchHandler.SearchesList);
      LSearchesList.AddServer (ASU.To_Unbounded_String (ADA.Command_Line.Argument (7)),
                               ASU.To_Unbounded_String (ADA.Command_Line.Argument (8)),
                               ASU.To_Unbounded_String (ADA.Command_Line.Argument (9)),
                               LSearchHandler.SearchesList);
      LFileHandler.StartDownload (ASU.To_Unbounded_String (ADA.Command_Line.Argument (9)));
      LConsole.StartConsole;
    when others =>
      LConsole.ShowUsage;
  end case;
  LLU.Finalize;
end Lince;
