with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Lince_Config;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IndexHandler;
with Lince_IO;

use type Ada.Strings.Unbounded.Unbounded_String;
use type Ada.Calendar.Time;

package body Lince_DownloadsList is

  -- Turn inactive all blocks of a download.
  procedure CreateBlockList ( BlocksList : in out TBlocksSlots ) is
  begin
    for i in 1 .. LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      BlocksList (i).Active     := False;
      BlocksList (i).Completed  := False;
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList","CreateBlockList",Ex);
  end CreateBlockList;

  -- Turn inactive all downloads, and call procedures to make inactive
  -- their blocks and servers.
  procedure CreateDownloadList ( DownloadsList : in out TDownloadsSlots ) is
  begin
    for i in 1 .. LConfig.MAX_PARALLEL_DOWNLOADS loop
      DownloadsList (i).Active     := False;
      CreateBlockList (DownloadsList (i).Blocks);
      CreateServerList (DownloadsList (i).Servers);
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "CreateDownloadList", Ex);
  end CreateDownloadList;

  -- Turn inactive all servers
  procedure CreateServerList ( ServersList : in out TServersSlots ) is
  begin
    for i in 1 .. LConfig.MAX_DOWNLOAD_SERVERS loop
      ServersList (i).Active     := False;
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "CreateServerList", Ex);
  end CreateServerList;

  -- Adds a new clean download to the list. Used when the first
  -- DataREQ is sent.
  procedure AddDownload ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String) is
    DIndex 	: Positive;
  begin
    -- If the download isn't previously requested...
    if not IsDownloadRequested (FileName, DownloadsList) then
      -- ... and they are download slots availables..
      if ActiveDownloads (DownloadsList) < LConfig.MAX_PARALLEL_DOWNLOADS then
        DIndex := Positive(GetInactiveDownloadPos (DownloadsList));
        DownloadsList (DIndex).Active := True;
        DownloadsList (DIndex).FileName := FileName;
        DownloadsList (DIndex).ReceivedSize := False;
        DownloadsList (DIndex).StartTime := ACal.Clock;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "AddDownload", Ex);
  end AddDownload;

  -- Add a block to the block list
  procedure AddBlock    ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String;
                          BlockPos      : in Positive;
                          From          : in LLU.End_Point_Type) is
    DIndex 	 : Natural;
    BIndex       : Positive;
  begin
    -- Get the download from the download list
    DIndex       := GetDownloadPosition (FileName, DownloadsList);
    -- If found...
    if DIndex /= 0 then
      -- ... and it have emty slots...
      if ActiveBlocks (DownloadsList (Positive(DIndex)).Blocks) < LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD then
        BIndex := Positive(GetInactiveBlockPos (DownloadsList(Positive(DIndex)).Blocks));
        DownloadsList (Positive(DIndex)).Blocks (BIndex).Active := True;
        DownloadsList (Positive(DIndex)).Blocks (BIndex).Completed := False;
        DownloadsList (Positive(DIndex)).Blocks (BIndex).BlockPos := BlockPos;
        DownloadsList (Positive(DIndex)).Blocks (BIndex).From := From;
        DownloadsList (Positive(DIndex)).Blocks (BIndex).SentTime := ACal.Clock;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "AddBlock", Ex);
  end AddBlock;

  -- Adds a server to the server list
  procedure AddServer   ( DownloadsList : in out TDownloadsSlots;
                          FileName      : in ASU.Unbounded_String;
                          Server        : in LLU.End_Point_Type) is
    DIndex 	 : Natural;
    SIndex       : Positive;
  begin
    -- Search the download in the download list
    DIndex       := GetDownloadPosition (FileName, DownloadsList);
    -- If found...
    if DIndex /= 0 then
      -- and the list is not full
      if ActiveServers (DownloadsList (Positive(DIndex)).Servers) < LConfig.MAX_DOWNLOAD_SERVERS then
        SIndex := Positive(ActiveServers (DownloadsList (Positive(DIndex)).Servers) + 1);
        DownloadsList (Positive(DIndex)).Servers (SIndex).Active := True;
        DownloadsList (Positive(DIndex)).Servers (SIndex).End_Point := Server;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "AddServer", Ex);
  end AddServer;


  -- Check for timeouts in all downloads and ask again the block if needed
  procedure CheckForTimeOuts   ( DownloadsList  : in out TDownloadsSlots) is
    BActive,BCompleted : boolean;
    Sent, Now          : ACal.Time;
    TimeWaiting        : Duration;
  begin
    -- For all downloads...
    for i in 1 .. LConfig.MAX_PARALLEL_DOWNLOADS loop
      -- that are active...
      if DownloadsList (i).Active then
        -- and all blocks...
        for j in 1 .. LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
          BActive := DownloadsList (i).Blocks (j).Active;
          BCompleted := DownloadsList (i).Blocks (j).Completed;
          -- that are actived and not completed...
          if BActive and not BCompleted then
            -- TODO: If timeouts fails, check this part
            Sent := DownloadsList (i).Blocks (j).SentTime;
            Now  := ACal.Clock;
            TimeWaiting :=  (Now - Sent);
            -- if it expired...
            if TimeWaiting > LConfig.MAX_PACKET_TIMEOUT then
              LIO.VerboseDebug ("LDownloadsList", "CheckForTimeOuts",
                                "Block " & Positive'Image (DownloadsList (i).Blocks (j).BlockPos) &
                                " expired. Asking again...");
              AskBlock (DownloadsList (i).FileName, DownloadsList (i).Blocks (j));
              DownloadsList (i).Blocks (j).SentTime := ACal.Clock;
            end if;
          end if;
        end loop;
      end if;
    end loop;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList","CheckForTimeOuts",Ex);
  end CheckForTimeOuts;

  function GetDownloadTime ( FileName : in ASU.Unbounded_String;
                            DownloadsList : in TDownloadsSlots) return Duration is
    DPos : Positive;
  begin
    DPos := GetDownloadPosition (FileName, DownloadsList);
    return (ACal.Clock - DownloadsList(DPos).StartTime);
  end GetDownloadTime;


  function IsDownloadRequested( FileName       : in ASU.Unbounded_String;
                                DownloadsList  : in TDownloadsSlots) return Boolean is
    DownloadPosit : natural;
  begin
    DownloadPosit := GetDownloadPosition (FileName, DownloadsList);
    if DownloadPosit = 0 then
      return False;
    else
      return DownloadsList(Positive(DownloadPosit)).Active;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "IsDownloadRequested", Ex);
      raise;
  end IsDownloadRequested;


  function IsBlockRequested    ( FileName       : in ASU.Unbounded_String;
                                 BlockPos       : in Positive;
                                 DownloadsList  : in TDownloadsSlots) return Boolean is
    DownloadPosit, BlockPosit : natural;
  begin
    DownloadPosit := GetDownloadPosition (FileName, DownloadsList);
    if DownloadPosit = 0 then
      return False;
    else
      BlockPosit := GetBlockPosition (BlockPos, DownloadsList (Positive(DownloadPosit)).Blocks);
      if BlockPosit = 0 then
        return False;
      else
        return True;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "IsBlockRequested", Ex);
      raise;
  end IsBlockRequested;


  function IsBlockCompleted    ( FileName       : in ASU.Unbounded_String;
                                 BlockPos       : in Positive;
                                 DownloadsList  : in TDownloadsSlots) return Boolean is
    DPos, BPos : Natural;
  begin
    DPos := GetDownloadPosition (FileName, DownloadsList);
    if DPos = 0 then
      return True;
    else
      BPos := GetBlockPosition (BlockPos, DownloadsList (Positive (DPos)).Blocks);
      if BPos = 0 then
        return True;
      else
        return DownloadsList (Positive (DPos)).Blocks (Positive (BPos)).Completed;
      end if;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "IsBlockCompleted", Ex);
      raise;
  end IsBlockCompleted;

  -- Return either if a download is waiting or not for size.
  function IsWaitingForSize   ( FileName        : in ASU.Unbounded_String;
                                DownloadsList   : in TDownloadsSlots) return Boolean is
  begin
    return (not DownloadsList (GetDownloadPosition (FileName, DownloadsList)).ReceivedSize);
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "IsWaitingForSize", Ex);
      raise;
  end IsWaitingForSize;

  procedure MarkDownloadAsCompleted ( FileName     : in ASU.Unbounded_String;
                                      DownloadsList : in out TDownloadsSlots) is
    DownloadPosit    : natural;
  begin
    DownloadPosit := GetDownloadPosition (FileName, DownloadsList);
    DownloadsList (DownloadPosit).Active := False;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "MarkDownloadAsCompleted", Ex);
  end MarkDownloadAsCompleted;

  procedure MarkBlockAsCompleted ( FileName     : in ASU.Unbounded_String;
                                   BlockPos     : in Positive;
                                   DownloadsList : in out TDownloadsSlots) is
    DownloadPosit, BlockPosit : natural;
  begin


    DownloadPosit := GetDownloadPosition (FileName, DownloadsList);
    BlockPosit    := GetBlockPosition (BlockPos, DownloadsList (DownloadPosit).Blocks);
    DownloadsList (DownloadPosit).Blocks (BlockPosit).Completed := True;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "MarkBlockAsCompleted", Ex);
  end MarkBlockAsCompleted;


  procedure AskBlock           ( FileName       : in ASU.Unbounded_String;
                                 Block          : in TBlock) is
    DataReq          : LFileProtocol.TDataReq;
  begin
    if Block.BlockPos = 1 then
      DataReq.Options    := 1;
      DataReq.OptionType := LProtocol.SIZEREQ;
    else
      DataReq.Options    := 0;
    end if;
    DataReq.FileName      := FileName;
    DataReq.EPRes         := LProtocol.EP_localserver;
    DataReq.BlockPos      := Block.BlockPos;
    DataReq.BlockSize     := LFileProtocol.DATABLOCKSIZE;
    LFileProtocol.SendDataReq (Block.From, DataReq);
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList","AskBlock",Ex);
  end AskBlock;

  function ActiveDownloads     ( DownloadsList  : in TDownloadsSlots) return Natural is
    i                 : natural := 1;
    Count             : natural := 0;
    EndOfActives      : boolean := False;
  begin
    for i in 1..LConfig.MAX_PARALLEL_DOWNLOADS loop
      if DownloadsList (i).Active then
        Count        := Count + 1;
      end if;
    end loop;
    return Count;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "ActiveDownloads", Ex);
      raise;
  end ActiveDownloads;

  function ActiveBlocks ( BlocksList     : in TBlocksSlots) return Natural is
    i                 : natural := 1;
    Count             : natural := 0;
    EndOfActives      : boolean := False;
  begin
    for i in 1..LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      if BlocksList (i).Active then
        Count        := Count + 1;
      end if;
    end loop;

    LIO.VerboseDebug ("LDownloadsList", "ActiveBlocks",
                      "Counted " & Integer'Image (Count) & " active blocks");
    return Count;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "ActiveBlocks", Ex);
      raise;
  end ActiveBlocks;

  function ActiveBlocks ( DownloadsList     : in TDownloadsSlots;
                          FileName          : in ASU.Unbounded_String) return Natural is
    i                 : natural := 1;
    Count             : natural := 0;
    EndOfActives      : boolean := False;
    BlocksList        : TBlocksSlots;
    DPos              : Natural;
  begin
    DPos := GetDownloadPosition (FileName, DownloadsList);

    if DPos = 0 then
      LIO.VerboseDebug ("LDownloadsList", "ActiveBlocks",
                        "WARNING: No download with this FileName!! FileName: " & ASU.To_String(FileName));
    end if;

    BlocksList := DownloadsList(DPos).Blocks;
    for i in 1..LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      if BlocksList (i).Active then
        Count        := Count + 1;
      end if;
    end loop;

    return Count;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "ActiveBlocks", Ex);
      raise;
  end ActiveBlocks;

  function ActiveServers ( ServersList     : in TServersSlots) return Natural is
    i                 : natural := 1;
    Count             : natural := 0;
    EndOfActives      : boolean := False;
  begin
    for i in 1..LConfig.MAX_DOWNLOAD_SERVERS loop
      if ServersList (i).Active then
        Count        := Count + 1;
      end if;
    end loop;

    return Count;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "ActiveBlocks", Ex);
      raise;
  end ActiveServers;

  function GetDownloadPosition ( FileName       : in ASU.Unbounded_String;
                                 DownloadsList  : in TDownloadsSlots ) return Natural is
    i       : integer := 1;
    Found   : boolean := False;
  begin
    while not Found and i <= LConfig.MAX_PARALLEL_DOWNLOADS loop
      Found := (DownloadsList (i).FileName = FileName);
      if not Found then
        i := i + 1;
      end if;
    end loop;

    if Found then
      return i;
    else
      return 0;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "GetDownloadPosition", Ex);
      raise;
  end GetDownloadPosition;

  function GetBlockPosition    ( BlockPos       : in Positive;
                                 BlocksList     : in TBlocksSlots ) return Natural is
    i       : integer := 1;
    Found   : boolean := False;
  begin
    while not Found and i <= LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      Found := (BlocksList (i).BlockPos = BlockPos);
      if not Found then
        i := i + 1;
      end if;
    end loop;

    if Found then
      return i;
    else
      return 0;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "GetBlockPosition", Ex);
      raise;
  end GetBlockPosition;

  function GetInactiveDownloadPos ( DownloadsList : TDownloadsSlots) return Natural is
    i     : Positive := 1;
    Found : Boolean := False;
  begin
    while not Found and i <= LConfig.MAX_PARALLEL_DOWNLOADS loop
      if not DownloadsList (i).Active then
        Found := True;
        return Natural (i);
      else
        i := i + 1;
      end if;
    end loop;

    -- If no inactive positions found
    return 0;

  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "GetInactiveDownloadPos", Ex);
      raise;
  end GetInactiveDownloadPos;

  function GetInactiveBlockPos ( BlocksList : TBlocksSlots) return Natural is
    i     : Positive := 1;
    Found : Boolean := False;
  begin
    while not Found and i <= LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      if not BlocksList (i).Active then
        Found := True;
        return Natural (i);
      else
        i := i + 1;
      end if;
    end loop;

    -- If no inactive positions found
    return 0;

  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "GetInactiveBlockPos", Ex);
      raise;
  end GetInactiveBlockPos;

  function GetRandomServer ( FileName      : in ASU.Unbounded_String;
                             DownloadsList : in TDownloadsSlots) return LLU.End_Point_Type is
    subtype Count is Integer range 1 ..
      ActiveServers (DownloadsList (GetDownloadPosition (FileName, DownloadsList)).Servers);
    package Random_Server is new Ada.Numerics.Discrete_Random (Count);
    G     : Random_Server.Generator;
    C     : Count;
  begin
    Random_Server.Reset (G);
    C  := Random_Server.Random (G);

    return DownloadsList (GetDownloadPosition (FileName, DownloadsList)).Servers (C).End_Point;

  exception
    when Ex : others => LIO.DebugError ("LDownloadsList", "GetRandomServer", Ex);
      raise;
  end GetRandomServer;

  procedure ToggleSizeReceived ( FileName      : in ASU.Unbounded_String;
                                 DownloadsList : in out TDownloadsSlots) is
  begin
    DownloadsList (GetDownloadPosition (FileName, DownloadsList)).ReceivedSize := TRUE;
  exception
    when Ex : others => LIO.DebugError ("LDownloadsList","Toggle size received",Ex);
  end ToggleSizeReceived;

 procedure UpdateIndex ( FileName      : in ASU.Unbounded_String;
                         DownloadsList  : in out TDownloadsSlots) is
    DPos : Positive;
  begin
    DPos := GetDownloadPosition (FileName, DownloadsList);

    for i in 1 .. LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      if (DownloadsList (DPos).Blocks (i).Completed = TRUE) and
        (DownloadsList (DPos).Blocks (i).Active = TRUE)  then
        LIndexHandler.RemoveFromIndex (FileName, DownloadsList (DPos).Blocks (i).BlockPos);
        DownloadsList (DPos).Blocks (i).Active := FALSE;
        DownloadsList (DPos).Blocks (i).Completed := TRUE;
        LIO.VerboseDebug ("LDownloadsList", "UpdateIndex",
                          "Block " & Positive'Image (DownloadsList (DPos).Blocks (i).BlockPos) &
                          " have been donwloaded. Slot " & Positive'Image(i) & " set as free.");
      end if;
    end loop;
  end UpdateIndex;

  procedure PrintDownloadInformation  ( FileName      : in ASU.Unbounded_String;
                                       DownloadsList  : in out TDownloadsSlots) is
    DPos : Positive;
  begin
    DPos := Positive (GetDownloadPosition (Filename, DownloadsList));
    for I in 1 .. LConfig.MAX_PARALLEL_BLOCKS_PER_DOWNLOAD loop
      LIO.VerboseDebug ("LDownloadList", "PrintDownloadInformation",
                        "Position: " & Positive'Image (I) & " Active: " &
                        Boolean'Image (DownloadsList (DPos).Blocks (I).Active) & " Completed: " &
                        Boolean'Image (DownloadsList (DPos).Blocks (I).Completed) & " Block: " &
                        Positive'Image (DownloadsList (DPos).Blocks (I).BlockPos));
    end loop;
  end PrintDownloadInformation;




end Lince_DownloadsList;
