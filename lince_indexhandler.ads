with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Lince_Config;
with Lince_FileProtocol;
with Lince_IO;

package Lince_IndexHandler is

  package A_IO            renames Ada.Text_IO;
  package ASU             renames Ada.Strings.Unbounded;
  package ADir            renames Ada.Directories;
  package LConfig         renames Lince_Config;
  package LFileProtocol   renames Lince_FileProtocol;
  package LIO             renames Lince_IO;

  procedure WriteNewIndex ( FileName : in ASU.Unbounded_String;
                           FileSize : in Natural );
  function GetNumberOfParts ( FileSize : in  Natural ) return Natural;

  function GetNextBlock ( FileName  : in ASU.Unbounded_String;
                         Number    : in Positive := 1) return Natural;

  function GetRemainingBlocks ( FileName   : in ASU.Unbounded_String ) return Natural;

  procedure RemoveFromIndex ( FileName   : in ASU.Unbounded_String;
                              BlockPos   : in Positive);

  procedure RemoveIndex ( FileName    : in ASU.Unbounded_String );


end Lince_IndexHandler;
