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
    LIO.Notify ("1.) Simple server (loads configuration from lincepeer.conf)", LIO.mtNORMAL);
    LIO.Notify ("   ./lince", LIO.mtNORMAL);
    LIO.Notify ("2.) Simple server with options", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails>", LIO.mtNORMAL);
    LIO.Notify ("3.) Server and establishing widows size", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails> <windows>", LIO.mtNORMAL);
    LIO.Notify ("4.) Server connecting to a node", LIO.mtNORMAL);
    LIO.Notify ("   ./lince <port> <dir> <min delay> <max delay> <fails> <windows> <nhost> <nport>", LIO.mtNORMAL);
    LIO.Notify ("5.) Direct download", LIO.mtNORMAL);
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

  procedure ShowCommands is
  begin

    LIO.Notify (" COMMAND LIST", LIO.mtNORMAL);
    LIO.Notify ("==================================================", LIO.mtNORMAL);
    LIO.Notify (" + File searching and downloading commands", LIO.mtNORMAL);
    LIO.Notify ("    - directdownload <host> <port> <file>", LIO.mtNORMAL);
    LIO.Notify ("        Downloads directly a file from the host", LIO.mtNORMAL);
    LIO.Notify ("      indicated.", LIO.mtNORMAL);
    LIO.Notify ("    - search <file>", LIO.mtNORMAL);
    LIO.Notify ("        Search for nodes having the file.", LIO.mtNORMAL);
    LIO.Notify ("    - download <file>", LIO.mtNORMAL);
    LIO.Notify ("        Download a file previously searched.", LIO.mtNORMAL);
    LIO.Notify ("    - quickdownload <file>", LIO.mtNORMAL);
    LIO.Notify ("        Search and download a file.", LIO.mtNORMAL);
    LIO.Notify (" + Debugging commands", LIO.mtNORMAL);
    LIO.Notify ("    - log verbose [on/off]", LIO.mtNORMAL);
    LIO.Notify ("        Logs method flow", LIO.mtNORMAL);
    LIO.Notify ("    - log file <file>", LIO.mtNORMAL);
    LIO.Notify ("        Change the file in which log is stored.", LIO.mtNORMAL);
    LIO.Notify ("    - forge <type>", LIO.mtNORMAL);
    LIO.Notify ("        Where type is:", LIO.mtNORMAL);
    LIO.Notify ("           * datareq", LIO.mtNORMAL);
    LIO.Notify ("           * data", LIO.mtNORMAL);
    LIO.Notify ("           * dataerr", LIO.mtNORMAL);
    LIO.Notify ("        Build a package with the parameters given", LIO.mtNORMAL);
    LIO.Notify ("        by the user.", LIO.mtNORMAL);
    LIO.Notify (" + Other commands", LIO.mtNORMAL);
    LIO.Notify ("    - help", LIO.mtNORMAL);
    LIO.Notify ("        This message.", LIO.mtNORMAL);
    LIO.Notify ("    - quit", LIO.mtNORMAL);
    LIO.Notify ("        Terminate the program execution.", LIO.mtNORMAL);
    LIO.Notify ("", LIO.mtNORMAL);
  end ShowCommands;


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
        LIO.Notify ("Shutting down the application...", LIO.mtInformation);
        StopConsole := TRUE;
      elsif ASU.To_String (ActionCmd) = "help" then
        ShowCommands;
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
      elsif ASU.To_String (ActionCmd) = "search" then
        -- Get first parameter: File
        Next_Token (Command, Parameters, " ");
        LSearchHandler.StartSearch (Parameters);
      elsif ASU.To_String (ActionCmd) = "download" then
        -- Get first parameter: File
        Next_Token (Command, Parameters, " ");
        LFileHandler.StartDownload (Parameters);
      elsif ASU.To_String (ActionCmd) = "quickdownload" then
        Next_Token (Command, Parameters, " ");
        LSearchHandler.StartSearch (Parameters);
        LFileHandler.StartDownload (Parameters);
      -- Package forging commands
      -- forge <type of message>
      elsif ASU.To_String (ActionCmd) = "forge" then
          Next_Token (Command, Parameters, " ");
          if ASU.To_String (Parameters) = "datareq" then
            LForge.ForgeDataReq;
          elsif ASU.To_String (Parameters) = "data" then
            LForge.ForgeData;
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
