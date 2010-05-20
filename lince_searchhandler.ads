with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Exceptions;
with Ada.Calendar;
with Ada.Directories;
with Lince_Config;
with Lince_Protocol;
with Lince_SearchProtocol;
with Lince_NodeHandler;
with Lince_IO;
with Lince_SearchesList;
with gnutelight_contacts;

package Lince_SearchHandler is

  package LLU                 renames Lower_Layer_UDP;
  package ASU                 renames ADA.Strings.Unbounded;
  package ACal                renames Ada.Calendar;
  package ADir                renames Ada.Directories;
  package LConfig             renames Lince_Config;
  package LProtocol           renames Lince_Protocol;
  package LSearchProtocol     renames Lince_SearchProtocol;
  package LNodeHandler        renames Lince_NodeHandler;
  package LIO                 renames Lince_IO;
  package LSearchesList       renames Lince_SearchesList;
  package GNULContacts        renames gnutelight_contacts;

  SearchesList    : LSearchesList.TSearchesList;

  procedure StartSearch  ( FileName      : in ASU.Unbounded_String );
  procedure HandleSearch ( From      : in LLU.End_Point_Type;
                           Search    : in LSearchProtocol.TSearch);
  procedure HandleGotIt ( From      : in LLU.End_Point_Type;
                          GotIt     : in LSearchProtocol.TGotIt);

end Lince_SearchHandler;
