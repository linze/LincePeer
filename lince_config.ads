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

with Ada.Strings.Unbounded;
with Ada.Calendar;
with Lower_Layer_UDP;

package Lince_Config is

  package ASU      renames Ada.Strings.Unbounded;
  package ACal     renames Ada.Calendar;
  package LLU      renames Lower_Layer_UDP;

  REVISION            : constant String := "1.4";
  VERSION             : constant String := "Lince " & Revision;


  -- Enables Nodes and Search protocol by default
  EXTRASACTIVED       : constant boolean := TRUE;

  -- Directories
  SHARINGDIR                        : ASU.Unbounded_String := ASU.To_Unbounded_String ("");

  -- Logging and error handling options
  LOGGING                           : boolean := False;
  LOGFILENAME                       : ASU.Unbounded_String := ASU.To_Unbounded_String ("lince.log");
    -- Verbosity
    SHOWERRORS                      : Boolean := True;
    LOGERRORS                       : Boolean := False;
    SHOWMETHODSFLOW                 : Boolean := False;
    LOGMETHODSFLOW                  : Boolean := False;

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
end Lince_Config;
