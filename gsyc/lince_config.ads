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
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Directories;
with Lower_Layer_UDP;

package Lince_Config is

  package ASU           renames Ada.Strings.Unbounded;
  package ACal          renames Ada.Calendar;
  package A_IO          renames Ada.Text_IO;
  package ADir          renames Ada.Directories;
  package LLU           renames Lower_Layer_UDP;

  -- Maybe it's more elegant to handle the variables like
  -- SMF does: Having them in a array. It's a better solution
  -- to load them from a file or database. Too late to change
  -- it, anyway.

  REVISION            : constant String := "1.5.1";
  VERSION             : constant String := "Lince " & Revision;

  -- Configuration file
  CONFIGURATIONFILE  : ASU.Unbounded_String := ASU.To_Unbounded_String ("lincepeer.conf");
  COMMENTDELIMITER   : ASU.Unbounded_String := ASU.To_Unbounded_String ("#");


  -- Enables Nodes and Search protocol by default
  -- TODO: Check if this is really needed.
  EXTRASACTIVE                      : boolean := TRUE;

  -- Directories
  SHARINGDIR                        : ASU.Unbounded_String := ASU.To_Unbounded_String ("");

  -- Logging and error handling options
  LOGGING                           : boolean := True;
  LOGFILENAME                       : ASU.Unbounded_String := ASU.To_Unbounded_String ("lince.log");
    -- Verbosity
    SHOWERRORS                      : Boolean := True;
    LOGERRORS                       : Boolean := True;
    SHOWMETHODSFLOW                 : Boolean := False;
    LOGMETHODSFLOW                  : Boolean := True;

  -- Connection options
  LISTENINGPORT                     : integer := 7777;
  MINPROPAGATIONDELAY               : integer := 0;
  MAXPROPAGATIONDELAY               : integer := 0;
  FAULTSPERCENT                     : integer := 0;

  -- Transmission options
  MAX_PACKET_TIMEOUT                 : duration;

  MAX_PARALLEL_DOWNLOADS             : Positive := 100;
  MAX_PARALLEL_BLOCKS_PER_DOWNLOAD   : Positive := 100;
  MAX_DOWNLOAD_SERVERS               : Positive := 1000;

  -- Index file options
  INDEX_EXTENSION                    : ASU.Unbounded_String := ASU.To_Unbounded_String(".idx");

  -- Search options
  MAX_SAVED_SEARCHES                 : Positive := 100;
  SEARCH_RETRIES                     : Positive := 5;
  SEARCH_START_TTL                   : Positive := 5;

  -- Security
  ENABLESECURITYCHECKS               : boolean := True;

  -- Loads the variables from the configuration file
  procedure LoadConfig;

private
  function IsCommented ( Line : in ASU.Unbounded_String ) return boolean;
  procedure ParseLine (Line      : in ASU.Unbounded_String;
                       Variable  : out ASU.Unbounded_String;
                       Value     : out ASU.Unbounded_String);
  function BinaryToBoolean ( Value : in ASU.Unbounded_String) return boolean;
  procedure ChangeVariable ( Variable : in ASU.Unbounded_String;
                             Value    : in ASU.Unbounded_String);

end Lince_Config;
