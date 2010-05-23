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
with ADA.Unchecked_Deallocation;
with ADA.Streams;
with ADA.Exceptions;
with Ada.IO_Exceptions;
with Lince_Protocol;
with Lince_IO;

use type Ada.Strings.Unbounded.Unbounded_String;

package body Lince_SearchProtocol is

  procedure SendSearch ( To       : in LLU.End_Point_Type;
                        Search   : in TSearch) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset(SEARCHBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.SEARCH);
    Natural'Output (Buffer'Access, Search.Options);
    Natural'Output (Buffer'Access, Search.TTL);
    LLU.End_Point_Type'Output (Buffer'Access, Search.EPRes);
    ASU.Unbounded_String'Output (Buffer'Access, Search.FileName);
    LLU.End_Point_Type'Output (Buffer'Access, LProtocol.EP_localserver);
    if Search.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, Search.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LSearchProtocol", "SendSearch"
                      ,"Sent search petition " & SearchToString(Search));
  exception
    when Ex : others => LIO.DebugError ("LSearchProtocol","SendSearch",Ex);
  end SendSearch;


  procedure SendGotIt  ( To       : in LLU.End_Point_Type;
                        GotIt    : in TGotIt) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset(GOTITBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.GOTIT);
    Natural'Output (Buffer'Access, GotIt.Options);
    ASU.Unbounded_String'Output (Buffer'Access, GotIt.FileName);
    LLU.End_Point_Type'Output (Buffer'Access, GotIt.EPSvc);
    if GotIt.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, GotIt.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LSearchProtocol", "SendGotIt"
                      ,"Sent GotIt response " & GotItToString(GotIt));
  exception
    when Ex : others => LIO.DebugError ("LSearchProtocol","SendGotIt",Ex);
  end SendGotIt;


  procedure GetSearch  ( Buffer   : access LLU.Buffer_Type;
                        Search   : out TSearch) is
  begin
    Search.Options := Natural'Input (Buffer);
    Search.TTL     := Natural'Input (Buffer);
    Search.EPRes   := LLU.End_Point_Type'Input (Buffer);
    Search.FileName := ASU.Unbounded_String'Input (Buffer);
    Search.EPSvc   := LLU.End_Point_Type'Input (Buffer);

    if Search.Options = 1 then
      Search.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LSearchProtocol", "GetSearch"
                      ,"Readed " & SearchToString(Search));
  exception
    when Ex : others => LIO.DebugError ("LSearchProtocol","GetSearch",Ex);
  end GetSearch;


  procedure GetGotIt ( Buffer   : access LLU.Buffer_Type;
                      GotIt    : out TGotIt) is
  begin
    GotIt.Options := Natural'Input (Buffer);
    GotIt.FileName := ASU.Unbounded_String'Input (Buffer);
    GotIt.EPSvc   := LLU.End_Point_Type'Input (Buffer);

    if GotIt.Options = 1 then
      GotIt.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LSearchProtocol", "GetGotIt"
                      ,"Readed " & GotItToString(GotIt));
  exception
    when Ex : others => LIO.DebugError ("LSearchProtocol","GetGotIt",Ex);
  end GetGotIt;

  function SearchToString      ( Search  : TSearch ) return String is
    UString      : ASU.Unbounded_String;
  begin
    UString     := "Options: " & Natural'Image (Search.Options) &
                   " TTL: " & Natural'Image (Search.TTL) &
                   " EPres: " & LLU.Image (Search.EPRes) &
                   " FileName: " & Search.FileName &
                   " EPsvc: " & LLU.Image (Search.EPSvc);

    if Search.Options = 1 then
      UString := UString & " Option: ";
      case Search.OptionType is
        when LProtocol.SIZEREQ         => UString := UString & "SIZEREQ";
        when LProtocol.SIZE            => UString := UString & "SIZE";
        when LProtocol.FILE_NOT_FOUND  => UString := UString & "FILE_NOT_FOUND";
        when LProtocol.BLOCK_NOT_FOUND => UString := UString & "BLOCK_NOT_FOUND";
        when others => UString := UString & "Unknown option";
      end case;
    end if;
    return ASU.To_String (UString);
  end SearchToString;

  function GotItToString       ( GotIt   : TGotIt ) return String is
    UString      : ASU.Unbounded_String;
  begin
    UString     := "Options: " & Natural'Image (GotIt.Options) &
                   " FileName: " & GotIt.FileName &
                   " EPsvc: " & LLU.Image (GotIt.EPsvc);

    if GotIt.Options = 1 then
      UString := UString & " Option: ";
      case GotIt.OptionType is
        when LProtocol.SIZEREQ         => UString := UString & "SIZEREQ";
        when LProtocol.SIZE            => UString := UString & "SIZE";
        when LProtocol.FILE_NOT_FOUND  => UString := UString & "FILE_NOT_FOUND";
        when LProtocol.BLOCK_NOT_FOUND => UString := UString & "BLOCK_NOT_FOUND";
        when others => UString := UString & "Unknown option";
      end case;
    end if;
    return ASU.To_String (UString);
  end GotItToString;


end Lince_SearchProtocol;
