with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Streams;
with ADA.Exceptions;
with Ada.IO_Exceptions;

with Lince_Protocol;
with Lince_IO;
with gnutelight_contacts;

package Lince_NodeProtocol is

  package LLU              renames Lower_Layer_UDP;
  package ASU              renames ADA.Strings.Unbounded;
  package AS               renames ADA.Streams;
  package LProtocol        renames Lince_Protocol;
  package LIO              renames Lince_IO;
  package GNULContacts     renames gnutelight_contacts;

  -- Buffer sizes
  HELLOBUFFER    : constant Positive := 1024;
  WELCOMEBUFFER  : constant Positive := 1024;

  -- Max hello retries
  MAXHELLORETRIES: constant Positive := 3;


  type THello   is record
    Options      : Natural;
    EPres        : LLU.End_Point_Type;
    EPsvc        : LLU.End_Point_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;


  type TWelcome is record      -- TODO : Implement the node list!!
    Options      : Natural;
    N            : Natural;
    Peers        : GNULContacts.Contacts_List_Type;
    OptionType   : LProtocol.TOption_Type;
  end record;

  -- Send methods
  -- Sends a record thought a End_Point

  procedure SendHello  ( To       : in LLU.End_Point_Type;
                         Hello    : in THello);

  procedure SendWelcome( To       : in LLU.End_Point_Type;
                         Welcome  : in TWelcome);

  -- Get methods
  -- Transform the data flow into a record.
  procedure GetHello   ( Buffer   : access LLU.Buffer_Type;
                         Hello    : out THello);

  procedure GetWelcome ( Buffer   : access LLU.Buffer_Type;
                         Welcome  : out TWelcome);

  -- Record to string conversion record
  -- Places all the record information in a string to ve
  -- displayed at logs or screen
  function HelloToString      ( Hello   : in THello ) return String;
  function WelcomeToString    ( Welcome : in TWelcome ) return String;

end Lince_NodeProtocol;
