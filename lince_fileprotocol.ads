with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Unchecked_Deallocation;
with ADA.Streams;
with ADA.Exceptions;
with Ada.IO_Exceptions;
with Lince_Protocol;
with Lince_IO;


package Lince_FileProtocol is
  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package AS               renames ADA.Streams;
  package LProtocol        renames Lince_Protocol;
  package LIO              renames Lince_IO;


  -- Buffer sizes
  DATAREQBUFFER  : constant Positive := 1024;
  DATABUFFER     : constant Positive := 2048;
  DATAERRBUFFER  : constant Positive := 1024;

  -- Data block size
  DATABLOCKSIZE  : constant Positive := 1024;

  type TDataReq is record
    Options      : Natural;
    EPRes        : LLU.End_Point_Type;
    FileName     : ASU.Unbounded_String;
    BlockPos     : Positive;
    BlockSize    : Positive;
    OptionType   : LProtocol.TOption_Type;
  end record;

  -- This type is included due the imposibility to use unchecked deallocation
  -- with "access AS.Stream_Element_Array"
  type AccessBlockData is access AS.Stream_Element_Array;
  type TData	is record
    Options      : Natural;
    FileName     : ASU.Unbounded_String;
    BlockPos     : Positive;
    BlockSize    : Positive;
    BlockData	 : AccessBlockData;
    OptionType   : LProtocol.TOption_Type;
    Size         : Natural;
  end record;

  type TDataErr is record
    Options      : Natural;
    FileName     : ASU.Unbounded_String;
    BlockPos     : Positive;
    OptionType   : LProtocol.TOption_Type;
  end record;

  -- Send methods
  -- Sends a record thought a End_Point

  procedure SendDataReq ( To       : in LLU.End_Point_Type;
                         DataReq  : in TDataReq);

  procedure SendData    ( To       : in LLU.End_Point_Type;
                          Data     : in out TData);

  procedure SendDataErr ( To       : in LLU.End_Point_Type;
                          DataErr  : in TDataErr);

  -- Get methods
  -- Transform the data flow into a record.
  procedure GetDataReq ( Buffer   : access LLU.Buffer_Type;
                         DataReq  : out TDataReq);

  procedure GetData    ( Buffer   : access LLU.Buffer_Type;
                         Data     : out TData);

  procedure GetDataErr ( Buffer   : access LLU.Buffer_Type;
                         DataErr  : out TDataErr);

  -- Record to string conversion record
  -- Places all the record information in a string to ve
  -- displayed at logs or screen
  function DataReqToString ( DataReq   : TDataReq ) return String;
  function DataToString    ( Data      : TData ) return String;
  function DataErrToString ( DataErr   : TDataErr ) return String;


  -- Auxiliary procedures.AS.Stream_Element_Array
  procedure Delete_DataAccess is new Ada.Unchecked_Deallocation(AS.Stream_Element_Array, AccessBlockData);





end Lince_FileProtocol;
