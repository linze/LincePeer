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

  -- Main methods

  procedure ServeBlock   ( DataReq   : in LFileProtocol.TDataReq);

  procedure StartDownload ( FileName  : in ASU.Unbounded_String);

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
  function FileExists   (FileName : in ASU.Unbounded_String) return Boolean;
  function BlockExists  (FileName : in ASU.Unbounded_String;
                         Position : in Positive) return Boolean;

  procedure GetBlock    ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in out Positive;
                          Block      : access AS.Stream_Element_Array);

  procedure WriteBlock  ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in Positive;
                          Block      : access AS.Stream_Element_Array);

  procedure CreateEmptyFile (FileName    : in ASU.Unbounded_String);
end Lince_FileHandler;
