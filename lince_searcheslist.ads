with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Lince_Config;
with Lince_Protocol;
with Lince_SearchProtocol;
with Lince_IndexHandler;
with Lince_IO;
with gnutelight_contacts;

package Lince_SearchesList is


  package LLU             renames Lower_Layer_UDP;
  package ACal            renames Ada.Calendar;
  package ASU             renames Ada.Strings.Unbounded;
  package LConfig         renames Lince_Config;
  package LProtocol       renames Lince_Protocol;
  package LSearchProtocol renames Lince_SearchProtocol;
  package LIndexHandler   renames Lince_IndexHandler;
  package LIO             renames Lince_IO;
  package GNULContacts    renames gnutelight_contacts;

  type TSearchStatus is (stINACTIVE, stPROGRESS, stCOMPLETED);

  type TSearchItem is record
    Status     : TSearchStatus;
    FileName   : ASU.Unbounded_String;
    StartTime  : ACal.Time;
    Contacts   : GNULContacts.Contacts_List_Type;
    Tries      : Positive;
    LastTime   : ACal.Time;
  end record;

  type TSearchesList is array (1 .. LConfig.MAX_SAVED_SEARCHES) of TSearchItem;

  procedure CreateSearchesList ( SearchesList : in out TSearchesList);

  procedure AddSearch ( FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure UpdateTimeAndTries (FileName     : in ASU.Unbounded_String;
                                SearchesList : in out TSearchesList);

  function GetSearchTries ( FileName     : in ASU.Unbounded_String;
                            SearchesList : in TSearchesList) return Positive;

  procedure MarkAsCompleted ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in out TSearchesList);

  procedure AddServer (Server       : in LLU.End_Point_Type;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure AddServer (ServerHost   : in ASU.Unbounded_String;
                       ServerPort   : in ASU.Unbounded_String;
                       FileName     : in ASU.Unbounded_String;
                       SearchesList : in out TSearchesList);

  procedure GetServers ( To           : out GNULContacts.Contacts_List_Type;
                         FileName     : in ASU.Unbounded_String;
                         SearchesList : in TSearchesList);

  function IsMessageTimedOut ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean;

  function IsSearchTimedOut ( FileName     : in ASU.Unbounded_String;
                              SearchesList : in TSearchesList) return boolean;

  function IsSearchRequested ( FileName     : in ASU.Unbounded_String;
                               SearchesList : in TSearchesList) return boolean;

  function GetSearchPosition ( FileName    : in ASU.Unbounded_String;
                               SearchesList: in TSearchesList ) return Positive;

  procedure PrintServers ( FileName    : in ASU.Unbounded_String;
                           SearchesList : in TSearchesList );
private
  function GetFreePosition ( FileName     : in ASU.Unbounded_String;
                             SearchesList : in TSearchesList )  return Natural;
  function GetInactivePosition ( FileName     : in ASU.Unbounded_String;
                                SearchesList : in TSearchesList) return Natural;
  function GetCompletedPosition ( FileName     : in ASU.Unbounded_String;
                                  SearchesList : in TSearchesList) return Natural;
end Lince_SearchesList;
