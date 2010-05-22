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
-- Foobar is distributed in the hope that it will be useful,
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
with Ada.Directories;
with Lince_Config;
with Lince_FileProtocol;
with Lince_IO;

use type Ada.Strings.Unbounded.Unbounded_String;

package body Lince_IndexHandler is

  -- Creates a new file including all the BlockPos of the download.
  -- Each BlockPos in one line.
  procedure WriteNewIndex ( FileName : in ASU.Unbounded_String;
                            FileSize : in Natural ) is
    File       : A_IO.File_Type;
    ActSize    : Positive;
  begin
    LIO.VerboseDebug ("LIndexHandler", "WriteNewIndex",
                      "Creating file...");
    A_IO.Create (File, A_IO.Out_File,
                 ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));

    LIO.VerboseDebug ("LIndexHandler", "WriteNewIndex",
                      "Writing into index...");
    ActSize := 1;
    while ActSize < Positive (FileSize) loop
      A_IO.Put_Line (File, Natural'Image (ActSize));
      ActSize := ActSize + LFileProtocol.DATABLOCKSIZE;
    end loop;
    LIO.VerboseDebug ("LIndexHandler", "WriteNewIndex",
                      "Closing index...");
    A_IO.Close (File);
    LIO.VerboseDebug ("LIndexHandler", "WriteNewIndex",
                      "Index for file " & ASU.To_String (FileName) & " created.");
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "WriteNewIndex", Ex);
  end WriteNewIndex;

  -- Obviously, returns de total number of parts of a download from their
  -- size.
  function GetNumberOfParts ( FileSize : in Natural ) return Natural is
    Parts : float;
  begin
    LIO.VerboseDebug ("LIndexHandler", "GetNumberOfParts",
                      "Getting number of parts. FileSize: " & Natural'Image (FileSize) &
                      ". BlockSize: " & Positive'Image (LFileProtocol.DATABLOCKSIZE));

    Parts := Float (Float(FileSize) / Float(LFileProtocol.DATABLOCKSIZE));

    LIO.VerboseDebug ("LIndexHandler", "GetNumberOfParts",
                      "Resulted into " & Float'Image(Parts) & " parts");
    LIO.VerboseDebug ("LIndexHandler", "GetNumberOfParts",
                      "They are " & Natural'Image (Positive(Float'Ceiling (Parts))) & " blocks");
    return Natural (Float'Ceiling (Parts));
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "GetNumberOfParts", Ex);
      raise;
  end GetNumberOfParts;

  -- Get the next block in the list to download. The parameter "Number"
  -- tells the number of jumps to give. If "Number=1" it will give the first
  -- block to download. If "Number=2" will give the second one. It is useful
  -- for when, in the example given, block 1 is still downloading and we want
  -- to download the next block that isn't in queue.
  function GetNextBlock ( FileName  : in ASU.Unbounded_String;
                          Number    : in Positive := 1) return Natural is
    File      : A_IO.File_Type;
    i         : integer := Number;
    BlockPos  : Positive;
    EOF       : boolean;
    Done      : boolean;
  begin
    -- TODO: Check if it's wise to implement this file access war method.
    -- Maybe it's too resources consuming and adding a little delay will help.
    Done := False;
    while not Done loop
      begin
        A_IO.Open (File, A_IO.In_File,
                   ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
        while not A_IO.End_Of_File (File) and (i /= 0) loop
          EOF := A_IO.End_Of_File (File);
          BlockPos := Positive'Value (A_IO.Get_Line (File));
          i := i - 1;
        end loop;
        A_IO.Close (File);
        Done := True;
      exception
        when A_IO.Use_Error => Done := False;
      end;
    end loop;

    if EOF then
      return 0;
    else
      return BlockPos;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "GetNextBlock", Ex);
      raise;
  end GetNextBlock;

  -- Return the number of remaining blocks to download. It counts
  -- the total number of lines of the index file
  function GetRemainingBlocks ( FileName   : in ASU.Unbounded_String ) return Natural is
    File       : A_IO.File_Type;
    LinesCount : natural := 0;
    Trash      : ASU.Unbounded_String;
    Done      : boolean;
  begin
    -- TODO: Check if it's wise to implement this file access war method.
    -- Maybe it's too resources consuming and adding a little delay will help.
    Done := False;
    while not Done loop
      begin
    	A_IO.Open (File, A_IO.In_File,
        	       ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
    	while not A_IO.End_Of_File (File) loop
     	 Trash := ASU.To_Unbounded_String (A_IO.Get_Line (File));
      	LinesCount := LinesCount + 1;
        end loop;
        A_IO.Close (File);
        Done := True;
      exception
        when A_IO.Use_Error => Done := False;
      end;
    end loop;

    return LinesCount;
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "GetRemainingBlocks", Ex);
      raise;
  end GetRemainingBlocks;

  -- Copy the entire index file to a new one, avoiding copying the
  -- BlockPos to remove. Then, delete the old index and rename the
  -- new to the original name.
  procedure RemoveFromIndex ( FileName   : in ASU.Unbounded_String;
                              BlockPos   : in Positive) is
    OldFile, NewFile : A_IO.File_Type;
    ProcessedPos     : Positive;
    Done             : boolean;
  begin
    LIO.VerboseDebug ("LIndexHandler", "RemoveFromIndex",
                      "Removing block " & Positive'Image (BlockPos) & " from file " &
                      ASU.To_String (FileName));
    -- TODO: Check if it's wise to implement this file access war method.
    -- Maybe it's too resources consuming and adding a little delay will help.
    Done := False;
    while not Done loop
      begin
        -- Open the original file in a read mode and creates a new one
        A_IO.Open (OldFile, A_IO.In_File, ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
        A_IO.Create (NewFile, A_IO.Out_File, ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION & ".new"));
        -- Copy all the lines except the one which contains the BlockPos
        while not A_IO.End_Of_File (OldFile) loop
          ProcessedPos := Positive'Value (A_IO.Get_Line (OldFile));
          if ProcessedPos /= BlockPos then
            A_IO.Put_Line (NewFile, Positive'Image (ProcessedPos));
          end if;
        end loop;
        -- Close both files
        A_IO.Close (OldFile);
        A_IO.Close (NewFile);
        Done := True;
      exception
        when A_IO.Use_Error => Done := False;
      end;
    end loop;

    -- Delete the original and renames the new.
    ADir.Delete_File (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
    ADir.Rename (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION & ".new")
                 , ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "RemoveFromIndex", Ex);
  end RemoveFromIndex;

  procedure RemoveIndex ( FileName    : in ASU.Unbounded_String ) is
  begin
    if ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION)) then
      ADir.Delete_File (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION));
    end if;
  exception
    when Ex : others => LIO.DebugError ("LIndexHandler", "RemoveIndex", Ex);
  end RemoveIndex;



end Lince_IndexHandler;
