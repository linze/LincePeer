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


with Ada.Text_IO;
with Ada.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with ADA.Streams;
with Lince_Protocol;
with Lince_FileProtocol;
with Ada.Exceptions;
with Lower_Layer_UDP;

package body Lince_Forge is

  -- Forge DataReq
  procedure ForgeDataReq is
    DataReq     : LFileProtocol.TDataReq;
    EPto        : LLU.End_Point_Type;
    Tmp1, Tmp2  : ASU.Unbounded_String;
  begin
    LIO.GetInfo ("Destination IP?", Tmp1);
    LIO.GetInfo ("Destination port?", Tmp2);
    EPto := LLU.Build (LLU.To_IP (ASU.To_String(Tmp1)), Integer'Value(ASU.To_String (Tmp2)));

    -- Option field
    LIO.GetInfo ("Options? [0 or 1]", Tmp1);
    DataReq.Options := Natural'Value (ASU.To_String (Tmp1));

    -- EPres field
    LIO.GetInfo ("Reply IP?", Tmp1);
    LIO.GetInfo ("Reply port?", Tmp2);
    DataReq.EPres := LLU.Build (LLU.To_IP (ASU.To_String (Tmp1)), Integer'Value (ASU.To_String (Tmp2)));

   -- FileName field
    LIO.GetInfo ("Requested file name?", Tmp1);
    DataReq.FileName   := Tmp1;

    -- BlockPos field
    LIO.GetInfo ("Block position? [>1]", Tmp1);
    DataReq.BlockPos   := Positive'Value (ASU.To_String(Tmp1));

    -- BlockSize field
    LIO.GetInfo ("Block size? [>1]", Tmp1);
    DataReq.BlockSize   := Positive'Value (ASU.To_String(Tmp1));

    -- Option_Type and the Option
    if DataReq.Options = 1 then
      LIO.GetInfo ("Option? [0=Nothing,1=SizeReq,2=Size,3=File_Not_Found,4=Block_Not_Found]", Tmp1);
      case Integer'Value ( ASU.To_String (Tmp1)) is
        when 0 => null;
        when 1 => DataReq.OptionType := LProtocol.SIZEREQ;
        when 2 => DataReq.OptionType := LProtocol.SIZE;
        when 3 => DataReq.OptionType := LProtocol.FILE_NOT_FOUND;
        when 4 => DataReq.OptionType := LProtocol.BLOCK_NOT_FOUND;
        when others => null;
      end case;
    end if;

    LIO.Notify ("Sending DATAREQ", LIO.mtINFORMATION);
    LFileProtocol.SendDataReq (EPto, DataReq);
    LIO.Notify ("DATAREQ sent", LIO.mtDEBUG);
  exception
    when others =>
      LIO.Notify ("One or some of the data introduced are not valid.", LIO.mtERROR);
  end ForgeDataReq;

  -- Forge Data
  procedure ForgeData is
    Data        : LFileProtocol.TData;
    EPto        : LLU.End_Point_Type;
    Tmp1, Tmp2  : ASU.Unbounded_String;
  begin
    LIO.GetInfo ("Destination IP?", Tmp1);
    LIO.GetInfo ("Destination port?", Tmp2);
    EPto := LLU.Build (LLU.To_IP (ASU.To_String(Tmp1)), Integer'Value(ASU.To_String (Tmp2)));

    LIO.GetInfo ("Options? [0 or 1]", Tmp1);
    Data.Options := Natural'Value (ASU.To_String (Tmp1));

    LIO.GetInfo ("File to send?", Tmp1);
    Data.FileName   := Tmp1;

    LIO.GetInfo ("Block position? [>1]", Tmp1);
    Data.BlockPos   := Positive'Value (ASU.To_String(Tmp1));

    LIO.GetInfo ("Block size? [>1]", Tmp1);
    Data.BlockSize   := Positive'Value (ASU.To_String (Tmp1));

    Data.BlockData := new AS.Stream_Element_Array (1 .. AS.Stream_Element_Offset (LFileProtocol.DATABLOCKSIZE));

    if Data.Options = 1 then
      LIO.GetInfo ("Option? [0=Nothing,1=SizeReq,2=Size,3=File_Not_Found,4=Block_Not_Found]", Tmp1);
      case Integer'Value ( ASU.To_String (Tmp1)) is
        when 0 => null;
        when 1 => Data.OptionType := LProtocol.SIZEREQ;
        when 2 => Data.OptionType := LProtocol.SIZE;
                  LIO.GetInfo ("File size?", Tmp1);
                  Data.Size := Natural'Value (ASU.To_String(Tmp1));
        when 3 => Data.OptionType := LProtocol.FILE_NOT_FOUND;
        when 4 => Data.OptionType := LProtocol.BLOCK_NOT_FOUND;
        when others => null;
      end case;
    end if;

    LIO.Notify ("Sending DATAREQ", LIO.mtINFORMATION);
    LFileProtocol.SendData (EPto, Data);
    LIO.Notify ("DATAREQ sent", LIO.mtDEBUG);
  exception
    when others =>
      LIO.Notify ("One or some of the data introduced are not valid.", LIO.mtERROR);
  end ForgeData;

  -- Forge DataErr
  procedure ForgeDataErr is
    DataErr     : LFileProtocol.TDataErr;
    EPto        : LLU.End_Point_Type;
    Tmp1, Tmp2  : ASU.Unbounded_String;
  begin
    LIO.GetInfo ("Destination IP?", Tmp1);
    LIO.GetInfo ("Destination port?", Tmp2);
    EPto := LLU.Build (LLU.To_IP (ASU.To_String(Tmp1)), Integer'Value(ASU.To_String (Tmp2)));

    LIO.GetInfo ("Options? [0 or 1]", Tmp1);
    DataErr.Options := Natural'Value (ASU.To_String (Tmp1));

    LIO.GetInfo ("File name?", Tmp1);
    DataErr.FileName   := Tmp1;

    LIO.GetInfo ("Block position? [>1]", Tmp1);
    DataErr.BlockPos   := Positive'Value (ASU.To_String(Tmp1));

    if DataErr.Options = 1 then
      LIO.GetInfo ("Option? [0=Nothing,1=SizeReq,2=Size,3=File_Not_Found,4=Block_Not_Found]", Tmp1);
      case Integer'Value ( ASU.To_String (Tmp1)) is
        when 0 => null;
        when 1 => DataErr.OptionType := LProtocol.SIZEREQ;
        when 2 => DataErr.OptionType := LProtocol.SIZE;
        when 3 => DataErr.OptionType := LProtocol.FILE_NOT_FOUND;
        when 4 => DataErr.OptionType := LProtocol.BLOCK_NOT_FOUND;
        when others => null;
      end case;
    end if;

    LIO.Notify ("Sending DATAERR", LIO.mtINFORMATION);
    LFileProtocol.SendDataErr (EPto, DataErr);
    LIO.Notify ("DATAERR sent", LIO.mtDEBUG);
  exception
    when others =>
      LIO.Notify ("One or some of the data introduced are not valid.", LIO.mtERROR);
  end ForgeDataErr;


end Lince_Forge;
