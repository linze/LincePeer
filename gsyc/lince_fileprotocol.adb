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


--
--  FORGE AND SEND DATA SHARING PROTOCOL MESSAGES
--

with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Streams;
with ADA.Exceptions;
with ADA.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Lince_Protocol;
with Lince_IO;

-- Applied to be able to compare Data.OptionType and a TOption_Type.
use type Lince_Protocol.TOption_Type;
use type Ada.Strings.Unbounded.Unbounded_String;


package body Lince_FileProtocol is

  --
  --  RECORD TO MESSAGE
  --

  -- DataREQ methods
  procedure SendDataReq ( To       : in LLU.End_Point_Type;
                          DataReq  : in TDataReq) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset (DATAREQBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.DATAREQ);
    Natural'Output (Buffer'Access, DataReq.Options);
    LLU.End_Point_Type'Output (Buffer'Access, DataReq.EPRes);
    ASU.Unbounded_String'Output (Buffer'Access, DataReq.FileName);
    Positive'Output (Buffer'Access, DataReq.BlockPos);
    Positive'Output (Buffer'Access, DataReq.BlockSize);
    if DataReq.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, DataReq.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LFileProtocol", "SendDataReq"
                      , "Sent block petition to" & ASU.To_String(LProtocol.ClearLLUImage(To) & ": " & DataReqToString (DataReq)));
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","SendDataReq",Ex);
  end SendDataReq;

  -- Data methods
 procedure SendData    ( To       : in LLU.End_Point_Type;
                         Data     : in out TData) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset (DATABUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.DATA);
    Natural'Output (Buffer'Access, Data.Options);
    ASU.Unbounded_String'Output (Buffer'Access, Data.FileName);
    Positive'Output (Buffer'Access, Data.BlockPos);
    Positive'Output (Buffer'Access, Data.BlockSize);
    AS.Stream_Element_Array'Output (Buffer'Access, Data.BlockData.all);
    -- Free the access
    Delete_DataAccess (Data.BlockData);
    if Data.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, Data.OptionType);
      -- Only SIZE flag could be expected with DATA.
      if Data.OptionType = LProtocol.SIZE then
        Natural'Output (Buffer'Access, Data.Size);
      end if;
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LFileProtocol", "SendData"
                      , "Sent block to" & ASU.To_String(LProtocol.ClearLLUImage (To)) & ": " & DataToString (Data) &
                      ". DataBlock should be null here.");
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","SendData",Ex);
  end SendData;


   -- DataERR methods
  procedure SendDataErr ( To       : in LLU.End_Point_Type;
                          DataErr  : in TDataErr) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset(DATAERRBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.DATAERR);
    Natural'Output (Buffer'Access, DataErr.Options);
    ASU.Unbounded_String'Output (Buffer'Access, DataErr.FileName);
    Positive'Output (Buffer'Access, DataErr.BlockPos);
    if DataErr.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, DataErr.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LFileProtocol", "SendDataErr"
                      , "Sent block error to " & ASU.To_String(LProtocol.ClearLLUImage (To)) & ": " &
                      DataErrToString (DataErr));
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","SendDataErr",Ex);
  end SendDataErr;



  --
  --  MESSAGE TO RECORD
  --

  -- DataREQ methods
  procedure GetDataReq ( Buffer   : access LLU.Buffer_Type;
                         DataReq  : out TDataReq) is
  begin
    DataReq.Options := Natural'Input (Buffer);
    DataReq.EPRes := LLU.End_Point_Type'Input (Buffer);
    DataReq.FileName := ASU.Unbounded_String'Input (Buffer);
    DataReq.BlockPos := Positive'Input (Buffer);
    DataReq.BlockSize := Positive'Input (Buffer);
    if DataReq.Options = 1 then
      DataReq.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LFileProtocol", "GetDataReq"
                      ,"Readed " & DataReqToString(DataReq));
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","GetDataReq",Ex);
  end GetDataReq;


    -- Data methods
  procedure GetData    ( Buffer   : access LLU.Buffer_Type;
                         Data     : out TData) is
  begin
    Data.Options := Natural'Input (Buffer);
    Data.FileName := ASU.Unbounded_String'Input (Buffer);
    Data.BlockPos := Positive'Input (Buffer);
    Data.BlockSize := Positive'Input (Buffer);
    Data.BlockData := new AS.Stream_Element_Array (1..AS.Stream_Element_Offset(DATABLOCKSIZE));
    Data.BlockData.all := AS.Stream_Element_Array'Input (Buffer);
    -- We check if they are options at the tail
    if Data.Options = 1 then
      Data.OptionType := LProtocol.TOption_Type'Input (Buffer);
      -- Only SIZE flag could be expected with DATA.
      -- It needed to add a "use" clause at the beginning. Ask if
      -- a cleaner way is avaiable.
      if Data.OptionType = LProtocol.SIZE then
        Data.Size := Natural'Input (Buffer);
      end if;
    end if;
    LIO.VerboseDebug ("LFileProtocol", "GetData"
                      ,"Readed " & DataToString(Data));
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","GetData",Ex);
  end GetData;


  -- DataERR methods
  procedure GetDataErr ( Buffer   : access LLU.Buffer_Type;
                         DataErr  : out TDataErr) is
  begin
    DataErr.Options := Natural'Input (Buffer);
    DataErr.FileName := ASU.Unbounded_String'Input (Buffer);
    DataErr.BlockPos := Positive'Input (Buffer);
    if DataErr.Options = 1 then
      DataErr.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LFileProtocol", "GetDataErr"
                      ,"Readed " & DataErrToString(DataErr));
  exception
    when Ex : others => LIO.DebugError ("LFileProtocol","GetDataErr",Ex);
  end GetDataErr;

  --
  --  RECORD TO STRING
  --
  -- Used for debug and output messages
  function DataReqToString ( DataReq   : TDataReq ) return String is
    TmpResult     : ASU.Unbounded_String;
  begin
    TmpResult := ASU.To_Unbounded_String ("File: ");
    ASU.Append (TmpResult, DataReq.FileName);
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" EPRes: "));

    ASU.Append (TmpResult, ASU.To_String(LProtocol.ClearLLUImage (DataReq.EPRes)));
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockPos: "));
    ASU.Append (TmpResult, Positive'Image (DataReq.BlockPos));
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockSize: "));
    ASU.Append (TmpResult, Positive'Image (DataReq.BlockSize));

    if DataReq.Options = 1 then
      case DataReq.OptionType is
        when LProtocol.SIZEREQ         =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZEREQ"));
        when LProtocol.SIZE            =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZE"));
        when LProtocol.FILE_NOT_FOUND  =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: FILE_NOT_FOUND"));
        when LProtocol.BLOCK_NOT_FOUND =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: BLOCK_NOT_FOUND"));
        when others =>
            ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: Unknown option"));
      end case;
    end if;

    return ASU.To_String (TmpResult);
  end DataReqToString;

  function DataToString    ( Data      : TData ) return String is
    TmpResult     : ASU.Unbounded_String;
  begin
    TmpResult := ASU.To_Unbounded_String ("File: ");
    ASU.Append (TmpResult, Data.FileName);
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockPos: "));
    ASU.Append (TmpResult, Positive'Image (Data.BlockPos));
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockSize: "));
    ASU.Append (TmpResult, Positive'Image (Data.BlockSize));

    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockData: "));
    if Data.BlockData = null then
      ASU.Append (TmpResult, ASU.To_Unbounded_String (" NULL!!"));
    else
      ASU.Append (TmpResult, ASU.To_Unbounded_String (" Ok"));
    end if;

    if Data.Options = 1 then
      case Data.OptionType is
        when LProtocol.SIZEREQ         =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZEREQ"));
        when LProtocol.SIZE            =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZE"));
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Size: ")& Positive'Image(Data.Size));
        when LProtocol.FILE_NOT_FOUND  =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: FILE_NOT_FOUND"));
        when LProtocol.BLOCK_NOT_FOUND =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: BLOCK_NOT_FOUND"));
        when others =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: Unknown option"));
      end case;
    end if;

    return ASU.To_String (TmpResult);
  end DataToString;

  function DataErrToString ( DataErr   : TDataErr ) return String is
    TmpResult     : ASU.Unbounded_String;
  begin
    TmpResult := ASU.To_Unbounded_String ("File: ");
    ASU.Append (TmpResult, DataErr.FileName);
    ASU.Append (TmpResult, ASU.To_Unbounded_String (" BlockPos: "));
    ASU.Append (TmpResult, Positive'Image (DataErr.BlockPos));

    if DataErr.Options = 1 then
      case DataErr.OptionType is
        when LProtocol.SIZEREQ         =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZEREQ"));
        when LProtocol.SIZE            =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: SIZE"));
        when LProtocol.FILE_NOT_FOUND  =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: FILE_NOT_FOUND"));
        when LProtocol.BLOCK_NOT_FOUND =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: BLOCK_NOT_FOUND"));
        when others =>
          ASU.Append (TmpResult, ASU.To_Unbounded_String (" Option: Unknown option"));
      end case;
    end if;

    return ASU.To_String (TmpResult);
  end DataErrToString;

end Lince_FileProtocol;
