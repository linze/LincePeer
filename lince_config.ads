with Ada.Strings.Unbounded;
with Ada.Calendar;
with Lower_Layer_UDP;

package Lince_Config is

  package ASU      renames Ada.Strings.Unbounded;
  package ACal     renames Ada.Calendar;
  package LLU      renames Lower_Layer_UDP;

  REVISION            : constant String := "1.2 beta";
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
    LOGERRORS                       : Boolean := True;
    SHOWMETHODSFLOW                 : Boolean := False;
    LOGMETHODSFLOW                  : Boolean := True;

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
