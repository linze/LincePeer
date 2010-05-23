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


with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Lince_Config;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IndexHandler;
with Lince_IO;

package Lince_DownloadsList is

  package LLU             renames Lower_Layer_UDP;
  package ACal            renames Ada.Calendar;
  package ASU             renames Ada.Strings.Unbounded;
  package LConfig         renames Lince_Config;
  package LProtocol       renames Lince_Protocol;
  package LFileProtocol   renames Lince_FileProtocol;
  package LIndexHandler   renames Lince_IndexHandler;
  package LIO             renames Lince_IO;

  type TBlock is record
    Active       : Boolean;
    Completed    : Boolean;
    BlockPos     : Positive;
    From         : LLU.End_Point_Type;
    SentTime     : ACal.Time;
  end record;
  type TBlocksSlots is array (1 .. LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD) of TBlock;

  type TServer is record
    Active       : Boolean;
    End_Point    : LLU.End_Point_Type;
  end record;
  type TServersSlots is array (1 .. LConfig.MAX_DOWNLOAD_SERVERS) of TServer;


  type TDownload is record
    Active       : Boolean;
    FileName     : ASU.Unbounded_String;
    ReceivedSize : boolean;
    Blocks       : TBlocksSlots;
    StartTime    : ACal.Time;
    Servers      : TServersSlots;
  end record;
  type TDownloadsSlots is array (1 .. LConfig.MAX_PARALLEL_DOWNLOADS) of TDownload;

  procedure CreateDownloadList ( DownloadsList : in out TDownloadsSlots );

  procedure AddDownload ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String);

  procedure AddBlock    ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String;
                          BlockPos      : in Positive;
                          From          : in LLU.End_Point_Type);

  procedure AddServer   ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String;
                          Server        : in LLU.End_Point_Type);


  procedure CheckForTimeOuts ( DownloadsList  : in out TDownloadsSlots);

  function GetDownloadTime ( FileName : in ASU.Unbounded_String;
                            DownloadsList : in TDownloadsSlots) return Duration;

  function IsDownloadRequested( FileName       : in ASU.Unbounded_String;
                                  DownloadsList  : in TDownloadsSlots) return Boolean;
  function IsBlockRequested    ( FileName       : in ASU.Unbounded_String;
                                 BlockPos       : in Positive;
                                DownloadsList  : in TDownloadsSlots) return Boolean;
  function IsBlockCompleted    ( FileName       : in ASU.Unbounded_String;
                                 BlockPos       : in Positive;
                                 DownloadsList  : in TDownloadsSlots) return Boolean;
  function IsWaitingForSize   ( FileName        : in ASU.Unbounded_String;
                                DownloadsList   : in TDownloadsSlots) return Boolean;

  procedure MarkDownloadAsCompleted ( FileName     : in ASU.Unbounded_String;
                                      DownloadsList : in out TDownloadsSlots);
  procedure MarkBlockAsCompleted ( FileName     : in ASU.Unbounded_String;
                                   BlockPos     : in Positive;
                                  DownloadsList : in out TDownloadsSlots);
  function ActiveDownloads     ( DownloadsList  : in TDownloadsSlots) return Natural;
  function ActiveBlocks        ( BlocksList     : in TBlocksSlots) return Natural;
  function ActiveBlocks ( DownloadsList     : in TDownloadsSlots;
                          FileName          : in ASU.Unbounded_String) return Natural;
  function ActiveServers ( ServersList     : in TServersSlots) return Natural;

  function GetRandomServer ( FileName      : in ASU.Unbounded_String;
                             DownloadsList : in TDownloadsSlots) return LLU.End_Point_Type;

  procedure ToggleSizeReceived ( FileName      : in ASU.Unbounded_String;
                                DownloadsList : in out TDownloadsSlots);
  procedure UpdateIndex ( FileName      : in ASU.Unbounded_String;
                          DownloadsList  : in out TDownloadsSlots);
  procedure PrintDownloadInformation  ( FileName      : in ASU.Unbounded_String;
                                       DownloadsList  : in out TDownloadsSlots);
private
  procedure CreateBlockList ( BlocksList : in out TBlocksSlots );
  procedure CreateServerList ( ServersList : in out TServersSlots );

  procedure AskBlock           ( FileName        : in ASU.Unbounded_String;
                                 Block           : in TBlock;
                                 IsWaitingForSize: in boolean);

  function GetDownloadPosition ( FileName       : in ASU.Unbounded_String;
                                 DownloadsList  : in TDownloadsSlots ) return Natural;
  function GetBlockPosition    ( BlockPos       : in Positive;
                                 BlocksList     : in TBlocksSlots ) return Natural;

  function GetInactiveDownloadPos ( DownloadsList : TDownloadsSlots) return Natural;
  function GetInactiveBlockPos ( BlocksList : TBlocksSlots) return Natural;

end Lince_DownloadsList;
