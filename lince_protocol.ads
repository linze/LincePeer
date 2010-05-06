with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Streams;

package Lince_Protocol is

  package LLU renames Lower_Layer_UDP;
  package ASU renames ADA.Strings.Unbounded;
  package AS  renames ADA.Streams;

  -- Message types
  type TMessage_Type is (DATAREQ, DATA, DATAERR, HELLO, WELCOME, SEARCH, GOTIT);

  -- Option types
  type TOption_Type is (SIZEREQ, SIZE, FILE_NOT_FOUND, BLOCK_NOT_FOUND);

  -- Server End_Point
  EP_localserver : LLU.End_Point_Type;

end Lince_Protocol;
