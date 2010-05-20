with Ada.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Lince_Config;
with Lince_Protocol;
with Lince_FileHandler;
with Lince_SearchHandler;
with Lince_Forge;
with Lince_DownloadsList;
with Lince_SearchesList;
with Lince_IO;

package body Lince_Console is

  procedure ShowUsage is
  begin
    LIO.Notify ("                                " & LConfig.VERSION, LIO.mtNORMAL);
    LIO.Notify ("                                 May, 2010", LIO.mtNORMAL);
    LIO.Notify ("==========================================================================================", LIO.mtNORMAL);
    LIO.Notify ("Usage:", LIO.mtNORMAL);
    LIO.Notify ("", LIO.mtNORMAL);
    LIO.Notify ("1.) Simple server", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails>", LIO.mtNORMAL);
    LIO.Notify ("2.) Server and establishing widows size", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails> <windows>", LIO.mtNORMAL);
    LIO.Notify ("3.) Server connecting to a node", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails> <windows> <nhost> <nport>", LIO.mtNORMAL);
    LIO.Notify ("4.) Direct download", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails> <windows> <rhost> <rport> <file>", LIO.mtNORMAL);
    LIO.Notify ("", LIO.mtNORMAL);
    LIO.Notify ("Where:", LIO.mtNORMAL);
    LIO.Notify ("  <port>        : Local listening port.", LIO.mtNORMAL);
    LIO.Notify ("  <dir>         : Directory containing files to share and where downloaded files will", LIO.mtNORMAL);
    LIO.Notify ("                  be placed.", LIO.mtNORMAL);
    LIO.Notify ("  <min delay>   : Minimal delay.", LIO.mtNORMAL);
    LIO.Notify ("  <max delay>   : Maximal delay.", LIO.mtNORMAL);
    LIO.Notify ("  <fails>       : Fail percent in each transmission.", LIO.mtNORMAL);
    LIO.Notify ("  <windows>     : Windows size. Numer of blocks that can be downloaded simultaneoulsy.", LIO.mtNORMAL);
    LIO.Notify ("  <nhost>       : Host or IP of the node.", LIO.mtNORMAL);
    LIO.Notify ("  <nport>       : Port of the node.", LIO.mtNORMAL);
    LIO.Notify ("  <rhost>       : Remote host or IP of the file server.", LIO.mtNORMAL);
    LIO.Notify ("  <port>       : Remote port of the file server.", LIO.mtNORMAL);
    LIO.Notify ("", LIO.mtNORMAL);
  end ShowUsage;

  -- User console interface to fire actions
  procedure StartConsole is
    StopConsole           : boolean := FALSE;
    Command               : ASU.Unbounded_String;
    ActionCmd         	  : ASU.Unbounded_String;
    Parameters       	  : ASU.Unbounded_String;
    MoreParameters        : ASU.Unbounded_String;
    MoreAndMoreParameters : ASU.Unbounded_String; -- WTF?
  begin
    while not StopConsole loop
      A_IO.Put ("lince # ");
      Command := ASU_IO.Get_Line;
      Next_Token (Command, ActionCmd, " ");

      if ASU.To_String (ActionCmd) = "quit" then
        LLU.Finalize;
        LIO.Notify ("Shutting down the application...", LIO.mtInformation);
        StopConsole := TRUE;
      -- Download a file from a remote peer
      -- download <host> <port> <file>
      elsif ASU.To_String (ActionCmd) = "directdownload" then
        -- Get first parameter: Remote Host
        Next_Token (Command, Parameters, " ");
        -- Get second parameter: Remote Port
        Next_Token (Command, MoreParameters, " ");
        -- Get third parameter: Remote file
        Next_Token (Command, MoreAndMoreParameters, " ");

        LSearchesList.AddSearch (MoreAndMoreParameters, LSearchHandler.SearchesList);
        LSearchesList.AddServer (Parameters, MoreParameters, MoreAndMoreParameters, LSearchHandler.SearchesList);
        LFileHandler.StartDownload (MoreAndMoreParameters);
      elsif ASU.To_String (ActionCmd) = "download" then
        -- Get first parameter: File
        Next_Token (Command, Parameters, " ");
        LSearchHandler.StartSearch (Parameters);
        LFileHandler.StartDownload (Parameters);
      elsif ASU.To_String (ActionCmd) = "search" then
        -- Get first parameter: File
        Next_Token (Command, Parameters, " ");
        LSearchHandler.StartSearch (Parameters);
      -- Checks for expired downloads
      -- checkexpired
      elsif ASU.To_String (ActionCmd) = "checkexpired" then
        LDownloadsList.CheckForTimeOuts(LFileHandler.DownloadsSlots);
      -- Package forging commands
      -- forge <type of message>
      elsif ASU.To_String (ActionCmd) = "forge" then
          Next_Token (Command, Parameters, " ");
          if ASU.To_String (Parameters) = "datareq" then
            LForge.ForgeDataReq;
          elsif ASU.To_String (Parameters) = "data" then
            null;
          elsif ASU.To_String (Parameters) = "dataerr" then
            LForge.ForgeDataErr;
          else
            LIO.Notify ("You're so drunk to use this program...", LIO.mtINFORMATION);
          end if;
      -- Error logging commands
      -- log file <file name>
      -- log verbose <on/off>
      elsif ASU.To_String (ActionCmd) = "log" then
          Next_Token (Command, Parameters, " ");
          -- Establish the log file name
          if ASU.To_String (Parameters) = "file" then
            Next_Token (Command, Parameters, " ");
            LConfig.LogFileName := Parameters;
            LIO.StartLog;
          -- Turn verbose log on/off
          elsif ASU.To_String (Parameters) = "verbose" then
            Next_Token (Command, Parameters, " ");
            if ASU.To_String (Parameters) = "on" then
              LConfig.LogMethodsFlow := True;
            elsif ASU.To_String (Parameters) = "off" then
              LConfig.LogMethodsFlow := False;
            end if;
          else
            LIO.Notify ("You're so drunk to use this program...", LIO.mtINFORMATION);
          end if;
      else
        LIO.Notify ("You're so drunk to use this program...", LIO.mtINFORMATION);
      end if;
    end loop;
  end;


  -- Method to extract slices of a string separated by a delimiter
  procedure Next_Token ( Source    : in out ASU.Unbounded_String;
                        Token     : out ASU.Unbounded_String;
                        Delimiter : in string) is
    Posicion : integer;
  begin
    Posicion := ASU.Index (Source, Delimiter);
    case Posicion is
    when 1 =>
      -- El delimitador esta al comienzo de la cadena
      Token 	:= ASU.To_Unbounded_String (Delimiter);
      Source 	:= ASU.Tail (Source, ASU.Length (Source) - Posicion);
    when 0 =>
      -- El delimitador no se encuentra en la cadena
      Token		:= Source;
      Source	:= ASU.To_Unbounded_String ("");
    when others =>
      -- Otros casos
      Token	:= ASU.Head (Source, Posicion - 1);
      Source 	:= ASU.Tail (Source, ASU.Length (Source) - Posicion);
    end case;
  end;
end Lince_Console;
