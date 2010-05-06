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
  LIO.StartLog;
  LIO.VerboseDebug ("Lince", "Main", "Parsing arguments...");
  Arguments := ADA.Command_Line.Argument_Count;
  case Arguments is
    -- Startup as server
    when 5 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server");
      -- Listening port
      LUDPHandler.ListeningPort := Positive'Value (ADA.Command_Line.Argument (1));
      LConfig.SHARINGDIR := ASU.To_Unbounded_String (ADA.Command_Line.Argument (2) & "/");
      LLU.Set_Random_Propagation_Delay (Integer'Value (ADA.Command_Line.Argument (3))
                                      , Integer'Value (ADA.Command_Line.Argument (4)));
      LLU.Set_Faults_Percent (Integer'Value (ADA.Command_Line.Argument (5)));
      LConfig.MAX_PACKET_TIMEOUT := Duration (2 * Integer'Value (ADA.Command_Line.Argument (4)) / 1000);
      LUDPHandler.RunServer;
      LConsole.StartConsole;
    -- Startup as server with windows size.
    when 6 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server with windows size");
      -- Listening port
      LUDPHandler.ListeningPort := Positive'Value (ADA.Command_Line.Argument (1));
      LConfig.SHARINGDIR := ASU.To_Unbounded_String (ADA.Command_Line.Argument (2) & "/");
      LLU.Set_Random_Propagation_Delay (Integer'Value (ADA.Command_Line.Argument (3))
                                      , Integer'Value (ADA.Command_Line.Argument (4)));
      LLU.Set_Faults_Percent (Integer'Value (ADA.Command_Line.Argument (5)));
      LConfig.MAX_PACKET_TIMEOUT := Duration (2 * Integer'Value (ADA.Command_Line.Argument (4)) / 1000);
      -- Windows size
      LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD := Positive'Value (ADA.Command_Line.Argument (6));
      LUDPHandler.RunServer;
      LConsole.StartConsole;
    -- Start up with widows size and starting node
    when 8 =>
      LIO.VerboseDebug ("Lince", "Main", "Startup as server with windows size and starting node");
      -- Listening port
      LUDPHandler.ListeningPort := Positive'Value (ADA.Command_Line.Argument (1));
      LConfig.SHARINGDIR := ASU.To_Unbounded_String (ADA.Command_Line.Argument (2) & "/");
      LLU.Set_Random_Propagation_Delay (Integer'Value (ADA.Command_Line.Argument (3))
                                      , Integer'Value (ADA.Command_Line.Argument (4)) );
      LLU.Set_Faults_Percent (Integer'Value (ADA.Command_Line.Argument (5)));
      LConfig.MAX_PACKET_TIMEOUT := Duration (2 * Integer'Value (ADA.Command_Line.Argument (4)) / 1000);
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
      -- Listening port
      LUDPHandler.ListeningPort := Positive'Value (ADA.Command_Line.Argument (1));
      LConfig.SHARINGDIR := ASU.To_Unbounded_String (ADA.Command_Line.Argument (2) & "/");
      LLU.Set_Random_Propagation_Delay (Integer'Value (ADA.Command_Line.Argument (3))
                                      , Integer'Value (ADA.Command_Line.Argument (4)));
      LLU.Set_Faults_Percent (Integer'Value (ADA.Command_Line.Argument (5)));
      LConfig.MAX_PACKET_TIMEOUT := Duration (2 * Integer'Value (ADA.Command_Line.Argument (4)) / 1000);
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
end Lince;
