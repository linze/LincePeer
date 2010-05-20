with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Exceptions;
with Ada.Calendar;
with Lince_Config;
with Lince_Protocol;
with Lince_IO;
with gnutelight_contacts;

use type Ada.Calendar.Time;
use type Lower_Layer_UDP.End_Point_Type;

package body Lince_NodeHandler is

  procedure Connect ( ToHost  : in ASU.Unbounded_String;
                      ToPort  : in ASU.Unbounded_String) iS
    RemoteEP         : LLU.End_Point_Type;
    TimeWaiting      : Duration;
  begin
    LIO.VerboseDebug ("LNodeHandler", "Connect", "Trying to connect to remote node...");
    RemoteEP := LLU.Build (LLU.To_IP (ASU.To_String (ToHost)), Integer'Value (ASU.To_String (ToPort)));
    SayHello (RemoteEP);
    Sent  := ACal.Clock;
    Tries    := 1;
    while not ListReceived and Tries <= LNodeProtocol.MAXHELLORETRIES loop
      TimeWaiting := ACal.Clock - Sent;
      if TimeWaiting  > LConfig.MAX_PACKET_TIMEOUT then
        SayHello (RemoteEP);
        Tries := Tries + 1;
        Sent  := ACal.Clock;
        LIO.VerboseDebug ("LNodeHandler", "Connect", "#" & Positive'Image(Tries) & " try...");
      end if;
    end loop;

    -- If the remote server is offline, they're no point into adding it
    if ListReceived then
      -- WARNING: Here I've make the asumption that we were connecting
      -- to the remote EPsvc
      GNULContacts.Add_One (NodesSlots, RemoteEP);
    end if;
    LIO.VerboseDebug ("LNodeHandler", "Connect", "Finished trying to connect remote node.");
  exception
    when Ex : others => LIO.DebugError ("LNodeHandler","Connect",Ex);
  end Connect;


  procedure SayHello ( To  : in LLU.End_Point_Type) is
    Hello   :  LNodeProtocol.THello;
  begin
    Hello.Options := 0;
    Hello.EPres := LProtocol.EP_localserver;
    Hello.EPsvc := LProtocol.EP_localserver;
    LNodeProtocol.SendHello (To, Hello);
  exception
    when Ex : others => LIO.DebugError ("LNodeHandler","SayHello",Ex);
  end SayHello;


  procedure GiveWelcome ( To     : in LLU.End_Point_Type;
                          NodeEP : in LLU.End_Point_Type) is
    Welcome    : LNodeProtocol.TWelcome;
    ContactEP  : LLU.End_Point_Type;
  begin
    Welcome.Options := 0;
    Welcome.N := GNULContacts.Total (NodesSlots);
    for i in 1 .. Welcome.N loop
      ContactEP := GNULContacts.Get_One (NodesSlots, i);
      -- Avoid sending itself as contact
      if ContactEP = NodeEP then
        Welcome.N := Welcome.N - 1;
      else
        GNULContacts.Add_One (Welcome.Peers, ContactEP );
      end if;
    end loop;
    LNodeProtocol.SendWelcome(To,Welcome);
  exception
    when Ex : others => LIO.DebugError ("LNodeHandler","GiveWelcome",Ex);
  end GiveWelcome;

  procedure HandleHello ( From  : in LLU.End_Point_Type;
                          Hello : in LNodeProtocol.THello) is
  begin
    GNULContacts.Add_One (NodesSlots,Hello.EPsvc);
    GiveWelcome (Hello.EPres, Hello.EPsvc);
  exception
    when Ex : others => LIO.DebugError ("LNodeHandler","HandleHello",Ex);
  end HandleHello;

  procedure HandleWelcome ( From    : in LLU.End_Point_Type;
                            Welcome : in LNodeProtocol.TWelcome) is
  begin
    LIO.VerboseDebug ("LNodeHandler", "HandleWelcome", "Welcome received!");
    if not ListReceived then
      for i in 1 .. Welcome.N loop
        GNULContacts.Add_One (NodesSlots,GNULContacts.Get_One(Welcome.Peers,i));
      end loop;
      ListReceived := True;
    end if;
  exception
    when Ex : others => LIO.DebugError ("LNodeHandler","HandleWelcome",Ex);
  end HandleWelcome;


end Lince_NodeHandler;
