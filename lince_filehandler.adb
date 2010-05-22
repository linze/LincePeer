with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Streams;
with ADA.Streams.Stream_IO;
with ADA.Exceptions;
with ADA.Directories;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IndexHandler;
with Lince_SearchHandler;
with Lince_DownloadsList;
with Lince_SearchesList;
with Lince_IO;
with gnutelight_contacts;

-- Applied to be able to compare Data.OptionType and a TOption_Type.
use type Lince_Protocol.TOption_Type;
-- Used to be able to concatenate Unbounded_Strings
use type Ada.Strings.Unbounded.Unbounded_String;
-- Applied to be able to compare file sizes
use type Ada.Directories.File_Size;

package body Lince_FileHandler is

  -- Check if a file exists. Return True if exists.
  -- TODO: Ada.Directories have a FileExists rutine. This method have to be deleted
  function FileExists   (FileName : in ASU.Unbounded_String) return Boolean is
    File            : AS_IO.File_Type;
  begin
    AS_IO.Open (File, AS_IO.In_File, ASU.To_String (LConfig.SHARINGDIR & FileName));
    AS_IO.Close (File);
    return True;
  exception
      when AS_IO.Name_Error => return False;
  end FileExists;

  -- Check if a block exists. Return True if exists. It tries to open the
  -- file and check the type of error.
  function BlockExists  (FileName : in ASU.Unbounded_String;
                         Position : in Positive) return Boolean is
    Size : Natural;
  begin
    Size := Natural(ADir.Size (ASU.To_String (LConfig.SHARINGDIR & FileName)));
    if Natural(Position) > Size then
      return False;
    else
      return True;
    end if;
  exception
    when others =>
      return False;
  end BlockExists;

  -- Reads a block of a file and places it in a stream.
  procedure GetBlock    ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in out Positive;
                          Block      : access AS.Stream_Element_Array) is
    File            : AS_IO.File_Type;
    LastByteRead    : AS.Stream_Element_Offset;
    Done            : boolean;
  begin
    LIO.VerboseDebug ("LFileHandler", "GetBlock",
                      "Reading block " & Positive'Image (Position) & " from file "
                      & ASU.To_String (LConfig.SHARINGDIR & FileName));
    Done := False;
    while not Done loop
      begin
        AS_IO.Open (File, AS_IO.In_File, ASU.To_String (LConfig.SHARINGDIR & FileName));
        AS_IO.Set_Index (File, AS_IO.Positive_Count(Position));
        AS_IO.Read (File, Block.all, LastByteRead);
        AS_IO.Close (File);
        Done := True;
      exception
        when AS_IO.Use_Error => Done := False;
      end;
    end loop;

    LIO.VerboseDebug ("LFileHandler", "GetBlock",
                      "Read " & AS.Stream_Element_Offset'Image (LastByteRead) & " bytes");
    Size := Positive(LastByteRead);
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","GetBlock",Ex);
  end GetBlock;

  -- Writes a stream into a file block.
  procedure WriteBlock  ( FileName   : in ASU.Unbounded_String;
                          Position   : in Positive;
                          Size       : in Positive;
                          Block      : access AS.Stream_Element_Array) is
    File            : AS_IO.File_Type;
    Done            : boolean;
  begin
    LIO.VerboseDebug ("LFileHandler", "WriteBlock",
                      "Writing bock " & Positive'Image (Position) & " into file "
                      & ASU.To_String (LConfig.SHARINGDIR & FileName));
    Done := False;
    while not Done loop
      begin
        if ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName)) then
          AS_IO.Open (File, AS_IO.Append_File, ASU.To_String (LConfig.SHARINGDIR & FileName));
        else
          AS_IO.Create (File, AS_IO.Out_File, ASU.To_String (LConfig.SHARINGDIR & FileName));
        end if;
        AS_IO.Set_Index (File, AS_IO.Positive_Count (Position));
        AS_IO.Write (File, Block.all (1..AS.Stream_Element_Offset(Size)));
        Done := True;
      exception
        when AS_IO.Use_Error => Done := False;
      end;
    end loop;
    AS_IO.Close (File);
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","WriteBlock",Ex);
  end WriteBlock;

  procedure CreateEmptyFile (FileName    : in ASU.Unbounded_String) is
    File : AS_IO.File_Type;
  begin
    if not ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName)) then
      AS_IO.Create (File, AS_IO.In_File, ASU.To_String (LConfig.SHARINGDIR & FileName));
      AS_IO.Close (File);
    end if;
  end CreateEmptyFile;


  -- Return to the other side the block requested. It check that the file and the block
  -- exists and if they do, reply. When giving the response, check if the SIZEREQ flag
  -- is active to return the SIZE too.
  procedure ServeBlock   ( DataReq   : in LFileProtocol.TDataReq) is
    Data      : LFileProtocol.TData;
  begin
    -- File exists check
    if not FileExists (DataReq.FileName) then
      LIO.VerboseDebug ("LFileHandler", "ServeBlock", "File not found");
      NotifyDataError (DataReq, LProtocol.FILE_NOT_FOUND);
    -- Block exists check
    elsif not BlockExists (DataReq.FileName, DataReq.BlockPos) then
      LIO.VerboseDebug ("LFileHandler", "ServeBlock", "Block not found");
      NotifyDataError (DataReq, LProtocol.BLOCK_NOT_FOUND);
    -- If file and block exists...
    elsif (ADir.Size (ASU.To_String (LConfig.SHARINGDIR & DataReq.FileName)) = 0) then
      LIO.VerboseDebug ("LFileHandler", "ServeBlock", "Empty file");
      NotifyDataError (DataReq, LProtocol.BLOCK_NOT_FOUND);
    else
      LIO.VerboseDebug ("LFileHandler", "ServeBlock", "Placing information into DATA message");
      -- Check if option SIZEREQ is turned on
      if DataReq.Options = 1 then
        if DataReq.OptionType = LProtocol.SIZEREQ then
          LIO.VerboseDebug ("LFileHandler", "ServeBlock", "Size asked.");
          -- Fill the option fields
          Data.OptionType := LProtocol.SIZE;
          Data.Size := Natural (ADir.Size (ASU.To_String (LConfig.SHARINGDIR & DataReq.FileName)));
        end if;
      end if;
      Data.Options := DataReq.Options;
      -- Fill the Data with the other reusable info in DataREQ
      Data.FileName := DataReq.FileName;
      Data.BlockPos := DataReq.BlockPos;
      -- They are no problem establishing the size before reading
      -- the file because GetBlock will update it if the block
      -- is smaller than the size.
      Data.BlockSize := DataReq.BlockSize;
      -- Reserve the BlockData memory
      Data.BlockData := new AS.Stream_Element_Array (1..AS.Stream_Element_Offset(LFileProtocol.DATABLOCKSIZE));
      if Data.Size = 0 then
        Data.BlockSize := 1;
      else
        GetBlock (Data.FileName, Data.BlockPos, Data.BlockSize, Data.BlockData);
      end if;

      -- Send the block requested
      LIO.VerboseDebug ("LFileHandler", "ServeBlock"
                        , "Data message completed." & LFileProtocol.DataToString (Data));
      LFileProtocol.SendData (DataReq.EPRes, Data);
    end if;
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","ServeBlock",Ex);
  end ServeBlock;


  -- Ask the first DATAREQ needed to start the download process. It
  -- check that the download limit isn't reached, add the download,
  -- block and server to their list and send a DATAREQ with SIZEREQ
  -- flag active.
  procedure StartDownload ( FileName  : in ASU.Unbounded_String) is
    From       : LLU.End_Point_Type;
    NodesCount : Positive;
    Nodes      : GNULContacts.Contacts_List_Type;
  begin
    -- Check the download limit
    if LDownloadsList.ActiveDownloads (DownloadsSlots) >= LConfig.MAX_PARALLEL_DOWNLOADS then
      LIO.Notify ("Max parallel downloads reached. Retry later.", LIO.mtERROR);
    elsif (ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName))) and
          not (ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION)))  then
      LIO.Notify ("File already downloaded.", LIO.mtERROR);
    -- If we are resuming a download...
    elsif ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName)) and
          ADir.Exists (ASU.To_String (LConfig.SHARINGDIR & FileName & LConfig.INDEX_EXTENSION)) then
      LIO.Notify ("Resuming download...", LIO.mtINFORMATION);

      -- The download can't be resumed if they're no nodes list!
      if not LSearchesList.IsSearchRequested (FileName, LSearchHandler.SearchesList) then
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "They are no servers for the selected file");
        LIO.Notify ("They are no servers for the selected file. Download aborted.", LIO.mtERROR);
      else
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "Including download in the download list");
        -- Add a download to the list
        LDownloadsList.AddDownload (DownloadsSlots, FileName);

        LSearchesList.GetServers ( Nodes, FileName, LSearchHandler.SearchesList);
        NodesCount := GNULContacts.Total (Nodes);
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "Getting the " & Natural'Image(NodesCount) & " servers");
        for i in 1 ..  NodesCount loop
          -- Adds the server to the list
          From := GNULContacts.Get_One (Nodes, i);
          LIO.VerboseDebug ("LFileHandler", "StartDownload", "    |-- Got node: " & LLU.Image(From));
          LDownloadsList.AddServer (DownloadsSlots, FileName, From);
        end loop;

        LDownloadsList.ToggleSizeReceived (FileName,DownloadsSlots);
--      AskMoreBlocks ( FileName );
        KeepDownloadAlive (FileName );
      end if;
    -- If we are starting a new download...
    else
      -- The download can't be resumed if they're no nodes list!
      if not LSearchesList.IsSearchRequested (FileName, LSearchHandler.SearchesList) then
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "They are no servers for the selected file");
        LIO.Notify ("They are no servers for the selected file. Download aborted.", LIO.mtERROR);
      else
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "Including download in the download list");
        -- Add a download to the list
        LDownloadsList.AddDownload (DownloadsSlots, FileName);

        LSearchesList.GetServers (Nodes, FileName, LSearchHandler.SearchesList);
        NodesCount := Positive(GNULContacts.Total (Nodes));
        LIO.VerboseDebug ("LFileHandler", "StartDownload", "Getting the " & Natural'Image(NodesCount) & " servers");
        for i in 1 ..  NodesCount loop
          -- Adds the server to the list
          From := GNULContacts.Get_One (Nodes, i);
          LIO.VerboseDebug ("LFileHandler", "StartDownload", "    |-- Got node: " & LLU.Image(From));
          LDownloadsList.AddServer (DownloadsSlots, FileName, From);
        end loop;

        -- We ask some blocks with the SIZEREQ flag active.
        FirstPopulate (FileName);

        KeepDownloadAlive (FileName);
      end if;
    end if;

  exception
    when Ex : others => LIO.DebugError ("LFileHandler","StartDownload",Ex);
  end StartDownload;

  procedure FirstPopulate ( FileName : in ASU.Unbounded_String) is
    i       : integer;
    DataReq : LFileProtocol.TDataReq;
    From    : LLU.End_Point_Type;
  begin
    -- Write the DataReq with SIZEREQ flag activated
    DataReq.Options := 1;
    DataReq.EPRes := LProtocol.EP_localserver;
    DataReq.FileName := FileName;
    DataReq.BlockSize := LFileProtocol.DATABLOCKSIZE;
    DataReq.OptionType := LProtocol.SIZEREQ;

    -- Ask blocks while the block windows permits it.
    i := 0;
    while i < LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      -- Calculate the new BlockPos to ask
      DataReq.BlockPos := i * LFileProtocol.DATABLOCKSIZE + 1;
      -- Get a random node from the list
      From := LDownloadsList.GetRandomServer (FileName, DownloadsSlots);
      -- Add to the asked blocks list
      LDownloadsList.AddBlock (DownloadsSlots, FileName, DataReq.BlockPos, From);
      -- Send the DataReq
      LFileProtocol.SendDataReq (From, DataReq);
      i := i + 1;
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LFileHandler", "FirstPopulate", Ex);
  end FirstPopulate;



  -- WARNING: The implementation may have a desing error.
  -- How the packets will be asked again if all the
  -- DataReq fails?
  -- TODO: Think about a parallel thread launched to ask timeout packets
  -- TODO: Think about moving the "askmoreblocks" option to this thread.
  -- TODO: Security check-> From address should be the same that Block.From.
  procedure ManageDownload  ( From      : in LLU.End_Point_Type;
                              Data      : in out LFileProtocol.TData) is
    IsRequested,IsCompleted : Boolean;
  begin
    IsRequested := LDownloadsList.IsBlockRequested (Data.FileName, Data.BlockPos, DownloadsSlots);
    IsCompleted := LDownloadsList.IsBlockCompleted (Data.FileName, Data.BlockPos, DownloadsSlots);

    -- Is the file and the dowload requested? <- Avoid arbitrary write
    if not IsRequested then
      LIO.VerboseDebug ("LFileHandler", "ManageDownload"
                        , "Received unasked block." & LFileProtocol.DataToString (Data));
    -- If the block is asked...
    else
      if not IsCompleted then
        -- Check if it is the first block containing the file size, needed to
        -- write the index.
        if LDownloadsList.IsWaitingForSize (Data.FileName, DownloadsSlots) then
          -- If they're no options (this shouldn't happens)...
          if Data.Options = 0 then
            LIO.VerboseDebug ("LFileHandler", "ManageDownload"
                              , "Waiting for first block with size and no size received.");
          -- If everything goes right..
          else
            LIO.VerboseDebug ("LFileHandler", "ManageDownload"
                              , "Size received." & LFileProtocol.DataToString (Data));
            -- This should not happens. We should have received a DataERR BLOCK_NOT_FOUND
            -- as response of the first DataREQ.
            if Data.Size = 0 then
              LIO.VerboseDebug ("LFileHandler", "ManageDownload"
                              , "Received packet indicating FileSize=0. Warning! This is outside protocol");
              -- Mark as completed to free the slot
              LDownloadsList.MarkDownloadAsCompleted (Data.FileName, DownloadsSlots);
              LIndexHandler.RemoveIndex (Data.FileName);
            else
              -- Creates the index file with the remaining blocks
              LIndexHandler.WriteNewIndex (Data.FileName, Data.Size);
            end if;
            -- Set as received size
            LDownloadsList.ToggleSizeReceived (Data.FileName, DownloadsSlots);
          end if;
        end if;
        -- Write the block into the file
        WriteBlock (Data.FileName, Data.BlockPos, Data.BlockSize, Data.BlockData);
        -- Notify it
        LIO.Notify ("Block " & Positive'Image (Data.BlockPos) & " from file " &
                    ASU.To_String (Data.FileName) & " completed.", LIO.mtRECEIVED);

        -- Makes the block slot usable for other block petition
        LDownloadsList.MarkBlockAsCompleted (Data.FileName, Data.BlockPos, DownloadsSlots);
      end if;

    -- Free the DataBlock
    LFileProtocol.Delete_DataAccess (Data.BlockData);

    end if;
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","ManageDownload",Ex);
  end ManageDownload;

  procedure AskMoreBlocks ( FileName     : in ASU.Unbounded_String ) is
    ContinueWorking   : boolean;
    ContinueSearching : boolean;
    ActiveBlocks      : Natural;
    RemainingBlocks   : integer;
    DownloadExists    : boolean;

    BlockPos          : Positive;
    Jumps             : integer := 1;

    DataReq           : LFileProtocol.TDataReq;
    randomEP          : LLU.End_Point_Type;
  begin
    DownloadExists := True;
    ActiveBlocks := LDownloadsList.ActiveBlocks (DownloadsSlots, FileName);
    RemainingBlocks := LIndexHandler.GetRemainingBlocks (FileName);
    ContinueWorking := (ActiveBlocks < LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD)
                       and (RemainingBlocks > ActiveBlocks);
    -- While they're space in the blocks window
    while ( ContinueWorking ) loop
      LIO.VerboseDebug ("LFileHandler", "AskMoreBlocks"
                        , "ActiveBlocks: " & Natural'Image (ActiveBlocks) &
                          "RemainingBlocks: " & Integer'Image (RemainingBlocks));
      -- Search a BlockPos from the index that isn't in the block list (if it's
      -- being downloading we don't want to download it twice or more)
      ContinueSearching := True;
      Jumps             := 1;
      while (ContinueSearching and Jumps <= RemainingBlocks) loop
        BlockPos := LIndexHandler.GetNextBlock (FileName,Jumps);
        if LDownloadsList.IsBlockRequested (FileName, BlockPos, DownloadsSlots) then
          Jumps := Jumps + 1;
        else
          -- They are avaiable slots and a block to process. Write the
          -- DataReq.
          DataReq.Options := 0;
          DataReq.EPRes := LProtocol.EP_localserver;
          DataReq.FileName := FileName;
          DataReq.BlockPos := BlockPos;
          DataReq.BlockSize := LFileProtocol.DATABLOCKSIZE;
          randomEP := LDownloadsList.GetRandomServer(FileName,DownloadsSlots);
          -- Send DataReq
          LFileProtocol.SendDataReq (randomEP, DataReq);
          -- Add to blocks list
          LDownloadsList.AddBlock (DownloadsSlots,FileName,BlockPos,randomEP);
          -- Stop doing more jumps
          ContinueSearching := False;
        end if;
      end loop;
      ActiveBlocks := ( LDownloadsList.ActiveBlocks (DownloadsSlots, FileName));
      RemainingBlocks := LIndexHandler.GetRemainingBlocks (FileName);
      DownloadExists := LDownloadsList.IsDownloadRequested (FileName,DownloadsSlots);
      ContinueWorking := (ActiveBlocks < LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD)
    			  and (RemainingBlocks > ActiveBlocks) and DownloadExists;
    end loop;

    LIO.VerboseDebug ("LFileHandler", "AskMoreBlocks"
                        , "Finished asking for more blocks");
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","AskMoreBlocks",Ex);
  end AskMoreBlocks;

  procedure NotifyDataError ( DataReq: in LFileProtocol.TDataReq;
                              Error  : in LProtocol.TOption_Type) is
    DataErr         : LFileProtocol.TDataErr;
  begin
    LIO.VerboseDebug ("LFileHandler", "NotifyDataErr", "Placing information into DataERR message");

    -- DataERR fields filling...
    DataErr.Options := 1;
    DataErr.FileName := DataReq.FileName;
    DataErr.BlockPos := DataReq.BlockPos;
    DataErr.OptionType := Error;

    -- Send the DataERR
      LIO.VerboseDebug ("LFileHandler", "NotifyDataErr"
                        , "DataErr message completed." & LFileProtocol.DataErrToString (DataErr));
    LFileProtocol.SendDataErr (DataReq.EPRes,DataErr);
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","NotifyDataErr",Ex);
  end NotifyDataError;

  procedure HandleDataErr ( From     : in LLU.End_Point_Type;
                           DataErr  : in LFileProtocol.TDataErr) is
    IsRequested : boolean;
  begin
    IsRequested := LDownloadsList.IsBlockRequested (DataErr.FileName, DataErr.BlockPos, DownloadsSlots);
    if not IsRequested then
      LIO.VerboseDebug ("LFileHandler", "HandleDataErr"
                        , "Message from a unknown server. Probably malicious." &
                        LFileProtocol.DataErrToString (DataErr));
    else
      if DataErr.Options = 0 then
        LIO.VerboseDebug ("LFileHandler", "HandleDataErr"
                          , "Received a DataErr without options. Probably malicious" &
                          LFileProtocol.DataErrToString (DataErr));
      else
        case DataErr.OptionType is
          when LProtocol.FILE_NOT_FOUND =>
            LIO.Notify ("File not found!", LIO.mtERROR);
            LDownloadsList.MarkDownloadAsCompleted (DataErr.FileName, DownloadsSlots);
            LIndexHandler.RemoveIndex (DataErr.FileName);
          when LProtocol.BLOCK_NOT_FOUND =>
            -- If the error comes in the first block, the file is a empty one.
            if DataErr.BlockPos = 1 then
              LIO.Notify ("Empty file downloaded", LIO.mtINFORMATION);
              CreateEmptyFile (DataErr.FileName);
              LDownloadsList.MarkDownloadAsCompleted (DataErr.FileName, DownloadsSlots);
              LIndexHandler.RemoveIndex (DataErr.FileName);
            -- If it's somewhere else, someone have deleted parts of the file or
            -- we are using a aggressive protocol.
            else
              LIO.VerboseDebug ("LFileHandler","HandlerDataErr","Received DataERR from a solicited block.");
              LDownloadsList.MarkBlockAsCompleted (DataErr.FileName, DataErr.BlockPos, DownloadsSlots);
            end if;

          when others => LIO.VerboseDebug ("LFileHandler","HandlerDataErr", "Strange OptionType received");
        end case;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","HandleDataErr",Ex);
  end HandleDataErr;

  procedure KeepDownloadAlive ( FileName : in ASU.Unbounded_String) is
    QueueIsEmpty, IndexIsEmpty : boolean := False;
    IsActive : Boolean;
  begin
    -- IsActive needed here too for avoid freezing the application when
    -- a empty file is received.
    IsActive := LDownloadsList.IsDownloadRequested(FileName, DownloadsSlots);
    while IsActive and (LDownloadsList.IsWaitingForSize (FileName, DownloadsSlots)) loop
      LIO.VerboseDebug ("LFileHandler", "KeepDownloadAlive",
                        "Waiting for size...");

      LDownloadsList.CheckForTimeOuts (DownloadsSlots);
      delay 1.0;
      IsActive := LDownloadsList.IsDownloadRequested(FileName, DownloadsSlots);
    end loop;

    IsActive := LDownloadsList.IsDownloadRequested(FileName, DownloadsSlots);
    while IsActive and not QueueIsEmpty and not IndexIsEmpty loop
      LIO.VerboseDebug ("LFileHandler", "KeepDownloadAlive",
                        "Refreshing the download process");
      LDownloadsList.PrintDownloadInformation (FileName, DownloadsSlots);
      LDownloadsList.UpdateIndex (FileName, DownloadsSlots);
      LDownloadsList.CheckForTimeOuts(DownloadsSlots);
      AskMoreBlocks (FileName);
      QueueIsEmpty := LDownloadsList.ActiveBlocks (DownloadsSlots, FileName) = 0;
      IndexIsEmpty := LIndexHandler.GetRemainingBlocks (FileName) = 0;
      IsActive := LDownloadsList.IsDownloadRequested(FileName, DownloadsSlots);
    end loop;
      -- Notify it
      LIO.VerboseDebug ("LFileHandler", "KeepDownloadAlive"
                       , "Download of " & ASU.To_String(FileName) & " completed");
      LIO.Notify ("Download of " & ASU.To_String (FileName) & " completed in " &
                   Duration'Image (LDownloadsList.GetDownloadTime(FileName,DownloadsSlots))
                   , LIO.mtINFORMATION);
      -- Mark as completed to free the slot
    LDownloadsList.MarkDownloadAsCompleted (FileName, DownloadsSlots);
    LIndexHandler.RemoveIndex (FileName);
  exception
    when Ex : others => LIO.DebugError ("LFileHandler","KeepDownloadAlive",Ex);
  end KeepDownloadAlive;




end Lince_FileHandler;
