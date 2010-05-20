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

  -- Local server
  ListeningPort  : Positive := 7777;


  procedure RunServer;

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
