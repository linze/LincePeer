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
with ADA.Strings.Unbounded.Text_IO;
with ADA.Streams.Stream_IO;
with ADA.Exceptions;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Lince_Config;

package body Lince_IO is

  -- Asks something and returns the answer
  procedure GetInfo ( Question : in String; Answer : out ASU.Unbounded_String ) is
  begin
    Notify (Question, mtASKINFO);
    Answer := ASU_IO.Get_Line;
  end;

  -- Shows a text in the screen
  procedure Notify (Message : in String; MessageType : in TOutputType) is
  begin
    case MessageType is
    when mtNORMAL       => A_IO.Put_Line (Message);
    when mtRECEIVED     => A_IO.Put_Line ("[>>] " & Message);
    when mtSENT         => A_IO.Put_Line ("[<<] " & Message);
    when mtINFORMATION  => A_IO.Put_Line ("[ii] " & Message);
    when mtASKINFO      => A_IO.Put      (Message & "> ");
    when mtERROR        => A_IO.Put_Line ("[EE] " & Message);
    when mtDEBUG        => A_IO.Put_Line ("[DD] " & Message);
    when mtOTHER        => null;
    end case;
  end Notify;

  procedure VerboseDebug ( PackageName     : String;
                           MethodName      : String;
                           Message         : String) is
  begin
    if LConfig.ShowMethodsFlow then
      Notify ("** [" & GCT_IO.Image (ACal.Clock,"%T.%i") & "]" &
              "ERROR!!: (" & PackageName & "->" & MethodName & "): " & Message, mtDEBUG);
    end if;
    if LConfig.LogMethodsFlow then
      Log ("[" & GCT_IO.Image (ACal.Clock,"%T.%i") & "] (" & PackageName & "->" & MethodName & "): " & Message);
    end if;
  end VerboseDebug;

  procedure DebugError ( PackageName   : String;
                         MethodName    : String;
                         Ex            : ADA.Exceptions.Exception_Occurrence) is
  begin
    if LConfig.ShowErrors then
      Notify ("(" & PackageName & "->" & MethodName & "): Unexpected exception " &
              Ada.Exceptions.Exception_Name (Ex) & " at " &
              Ada.Exceptions.Exception_Message (Ex)
              , mtERROR);
    end if;
    if LConfig.LogErrors then
      Log ("(" & PackageName & "->" & MethodName & "): Unexpected exception " &
           Ada.Exceptions.Exception_Name (Ex) & " at " &
           Ada.Exceptions.Exception_Message (Ex));
    end if;
  end DebugError;

  procedure StartLog is
    File        : A_IO.File_Type;
  begin
    A_IO.Create (File, A_IO.Out_File, ASU.To_String (LConfig.LogFileName));
    A_IO.Put_Line (File, "-- Execution start");
    A_IO.Close (File);
    Notify ("Logging started", mtInformation);
  end;

  -- BUG: It could fail if, in middle of execution, log file is
  -- removed. Method "FileExists" is in LFileHandler and referencing
  -- will cause crossed dependecies.
  -- TODO: Implement ADir.FileExists to avoid the bug.
  procedure Log ( Message : in String) is
    File        : A_IO.File_Type;
    Done        : boolean;
  begin
    Done := False;
    while not Done loop
      begin
        A_IO.Open (File, A_IO.Append_File, ASU.To_String (LConfig.LogFileName));
        A_IO.Put_Line (File, Message);
        A_IO.Close (File);
        Done := True;
      exception
        when A_IO.Use_Error => Done := False;
      end;
    end loop;
  end Log;


end Lince_IO;
