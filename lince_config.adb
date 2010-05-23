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
with Ada.Text_IO;
with Ada.Directories;

use type Ada.Strings.Unbounded.Unbounded_String;

package body Lince_Config is

  function IsCommented ( Line : in ASU.Unbounded_String ) return boolean is
    FirstCharacter : ASU.Unbounded_String;
  begin
    -- Empty lines are commnets
    if ASU.Length (Line) < 1 then
      return True;
    else
      FirstCharacter := ASU.Head (Line, 1);
      -- First character is the delimiter?
      if FirstCharacter = COMMENTDELIMITER then
        return True;
      else
        return False;
      end if;
    end if;
  exception
    when others =>
      return True;
  end IsCommented;

  procedure ParseLine (Line      : in ASU.Unbounded_String;
                       Variable  : out ASU.Unbounded_String;
                       Value     : out ASU.Unbounded_String) is
    Pos  : integer;
  begin
    Pos  := ASU.Index (Line, "=");
    if Pos = 0 then
      Variable := ASU.To_Unbounded_String ("");
      Value    := ASU.To_Unbounded_String("");
    else
      Variable := ASU.Head (Line, Pos - 1);
      Value    := ASU.Tail (Line, ASU.Length(Line) - Pos + 1);
    end if;
  end ParseLine;

  function BinaryToBoolean ( Value : in ASU.Unbounded_String) return boolean is
  begin
    return Integer'Value(ASU.To_String(Value)) = 1;
  end BinaryToBoolean;


  procedure ChangeVariable ( Variable : in ASU.Unbounded_String;
                             Value    : in ASU.Unbounded_String) is
  begin
    if ASU.To_String (Variable) = "EXTRASACTIVE" then
      EXTRASACTIVE       := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "SHARINGDIR" then
      SHARINGDIR         := Value;
    elsif ASU.To_String (Variable) = "LOGGING" then
      LOGGING            := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "LOGFILENAME" then
      LOGFILENAME        := Value;
    elsif ASU.To_String (Variable) = "SHOWERRORS" then
      SHOWERRORS         := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "LOGERRORS" then
      LOGERRORS          := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "SHOWMETHODSFLOW" then
      SHOWMETHODSFLOW    := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "LOGMETHODSFLOW" then
      LOGMETHODSFLOW     := BinaryToBoolean (Value);
    elsif ASU.To_String (Variable) = "LISTENINGPORT" then
      LISTENINGPORT      := Integer'Value (ASU.To_String (Value));
    elsif ASU.To_String (Variable) = "MINPROPAGATIONDELAY" then
      MINPROPAGATIONDELAY:= Integer'Value (ASU.To_String (Value));
    elsif ASU.To_String (Variable) = "MAXPROPAGATIONDELAY" then
      MAXPROPAGATIONDELAY:= Integer'Value (ASU.To_String (Value));
    elsif ASU.To_String (Variable) = "FAULTSPERCENT" then
      FAULTSPERCENT      := Integer'Value (ASU.To_String (Value));
    elsif ASU.To_String (Variable) = "MAX_PACKET_TIMEOUT" then
      MAX_PACKET_TIMEOUT := Duration'Value (ASU.To_String(Value));
    elsif ASU.To_String (Variable) = "INDEX_EXTENSION" then
      INDEX_EXTENSION    := Value;
    elsif ASU.To_String (Variable) = "SEARCH_RETRIES" then
      SEARCH_RETRIES     := Positive'Value (ASU.To_String (Value));
    elsif ASU.To_String (Variable) = "SEARCH_START_TTL" then
      SEARCH_START_TTL   := Positive'Value (ASU.To_String(Value));
    end if;
  end ChangeVariable;


  procedure LoadConfig is
    File      : A_IO.File_Type;
    Line      : ASU.Unbounded_String;
    Variable  : ASU.Unbounded_String;
    Value     : ASU.Unbounded_String;
    IsComment : boolean;
  begin
    if ADir.Exists (ASU.To_String (CONFIGURATIONFILE)) then
      A_IO.Open (File, A_IO.In_File, ASU.To_String (CONFIGURATIONFILE));
      IsComment := False;
      while not A_IO.End_Of_File (File) loop
        Line := ASU.To_Unbounded_String (A_IO.Get_Line (File));
        if not IsCommented (Line) then
          ParseLine (Line, Variable, Value);
          ChangeVariable (Variable, Value);
        end if;
      end loop;
    end if;
  end LoadConfig;

end Lince_Config;
