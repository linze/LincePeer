with ADA.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with ADA.Text_IO;
with Lower_Layer_UDP;
with Lince_Config;
with Lince_Protocol;
with Lince_FileHandler;
with Lince_SearchHandler;
with Lince_Forge;
with Lince_DownloadsList;
with Lince_SearchesList;
with Lince_IO;



package Lince_Console is

  package ASU              renames ADA.Strings.Unbounded;
  package ASU_IO           renames ADA.Strings.Unbounded.Text_IO;
  package A_IO             renames ADA.Text_IO;
  package LLU              renames Lower_Layer_UDP;
  package LConfig          renames Lince_Config;
  package LProtocol        renames Lince_Protocol;
  package LFileHandler     renames Lince_FileHandler;
  package LSearchHandler   renames Lince_SearchHandler;
  package LForge           renames Lince_Forge;
  package LDownloadsList   renames Lince_DownloadsList;
  package LSearchesList    renames Lince_SearchesList;
  package LIO              renames Lince_IO;

  procedure ShowUsage;
  procedure StartConsole;


private
  procedure Next_Token ( Source   : in out ASU.Unbounded_String;
                         Token     : out    ASU.Unbounded_String;
                        Delimiter : in     string);

end Lince_Console;
