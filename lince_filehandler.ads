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
with ADA.Strings.Unbounded;
with ADA.Streams;
with ADA.Streams.Stream_IO;
with ADA.Exceptions;
with ADA.Directories;
with Lince_Config;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IndexHandler;
with Lince_SearchHandler;
with Lince_DownloadsList;
with Lince_SearchesList;
with Lince_IO;
with gnutelight_contacts;

package Lince_FileHandler is
  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package AS               renames ADA.Streams;
  package AS_IO            renames ADA.Streams.Stream_IO;
  package ADir             renames ADA.Directories;
  package LConfig          renames Lince_Config;
  package LProtocol        renames Lince_Protocol;
  package LFileProtocol    renames Lince_FileProtocol;
  package LIndexHandler    renames Lince_IndexHandler;
  package LSearchHandler   renames Lince_SearchHandler;
  package LSearchesList    renames Lince_SearchesList;
  package LDownloadsList   renames Lince_DownloadsList;
  package LIO              renames Lince_IO;
  package GNULContacts    renames gnutelight_contacts;

  DownloadsSlots    : LDownloadsList.TDownloadsSlots;

  -- This methods are public due the need of use them in LForge.
  function FileExists   (FileName : in ASU.Unbounded_String) return Boolean;
  function BlockExists  (FileName : in ASU.Unbounded_String;
                         Position : in Positive) return Boolean;

  procedure GetBlock    ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in out Positive;
                          Block      : access AS.Stream_Element_Array);

  -- Main methods

  procedure ServeBlock   ( DataReq   : in LFileProtocol.TDataReq);

  procedure StartDownload ( FileName  : in ASU.Unbounded_String);

  procedure FirstPopulate ( FileName : in ASU.Unbounded_String);

  procedure ManageDownload  ( From     : in LLU.End_Point_Type;
                              Data      : in out LFileProtocol.TData);

  procedure AskMoreBlocks ( FileName     : in ASU.Unbounded_String );

  procedure NotifyDataError ( DataReq: in LFileProtocol.TDataReq;
                             Error   : in LProtocol.TOption_Type);

  procedure HandleDataErr ( From     : in LLU.End_Point_Type;
                           DataErr  : in LFileProtocol.TDataErr);

  procedure KeepDownloadAlive ( FileName : in ASU.Unbounded_String);


private
  -- Auxiliary methods

  procedure WriteBlock  ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in Positive;
                          Block      : access AS.Stream_Element_Array);

  procedure CreateEmptyFile (FileName    : in ASU.Unbounded_String);
end Lince_FileHandler;
