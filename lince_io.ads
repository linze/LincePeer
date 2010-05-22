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

package Lince_IO is

  package ASU              renames ADA.Strings.Unbounded;
  package ASU_IO           renames ADA.Strings.Unbounded.Text_IO;
  package A_IO             renames ADA.Text_IO;
  package AS_IO            renames ADA.Streams.Stream_IO;
  package ACal             renames Ada.Calendar;
  package GCT_IO           renames Gnat.Calendar.Time_IO;
  package LConfig          renames Lince_Config;

  type TOutputType is (mtNORMAL, mtRECEIVED, mtSENT, mtINFORMATION, mtERROR, mtASKINFO, mtDEBUG, mtOTHER);

  procedure Notify (Message : String; MessageType : in TOutputType);
  procedure GetInfo ( Question : in String; Answer : out ASU.Unbounded_String );

  procedure VerboseDebug ( PackageName     : String;
                           MethodName      : String;
                           Message         : String);

  procedure DebugError ( PackageName     : String;
                         MethodName      : String;
                         Ex              : ADA.Exceptions.Exception_Occurrence);

  procedure StartLog;
  procedure Log (Message : in String);
end Lince_IO;
