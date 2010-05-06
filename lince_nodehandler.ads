with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Exceptions;
with Ada.Calendar;
with Lince_Config;
with Lince_Protocol;
with Lince_NodeProtocol;
with Lince_IO;
with gnutelight_contacts;

package Lince_NodeHandler is

  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package ACal             renames Ada.Calendar;
  package LConfig          renames Lince_Config;
  package LProtocol        renames Lince_Protocol;
  package LNodeProtocol    renames Lince_NodeProtocol;
  package LIO              renames Lince_IO;
  package GNULContacts     renames gnutelight_contacts;

  NodesSlots     : GNULContacts.Contacts_List_Type;
  ListReceived   : boolean;
  Tries          : Positive;
  Sent           : ACal.Time;

  procedure Connect ( ToHost  : in ASU.Unbounded_String;
                      ToPort  : in ASU.Unbounded_String);

  procedure SayHello ( To  : in LLU.End_Point_Type);

  procedure HandleHello ( From  : in LLU.End_Point_Type;
                          Hello : in LNodeProtocol.THello);

  procedure GiveWelcome ( To     : in LLU.End_Point_Type;
                          NodeEP : in LLU.End_Point_Type);

  procedure HandleWelcome ( From    : in LLU.End_Point_Type;
                            Welcome : in LNodeProtocol.TWelcome);


end Lince_NodeHandler;
