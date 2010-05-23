with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Directories;
with Lince_IO;
with Lince_Protocol;
with Lower_Layer_UDP;
with gnutelight_contacts;

package Lince_Security is

  package ASU              renames Ada.Strings.Unbounded;
  package A_IO             renames Ada.Text_IO;
  package ADir             renames Ada.Directories;
  package LIO              renames Lince_IO;
  package LProtocol        renames Lince_Protocol;
  package LLU              renames Lower_Layer_UDP;
  package GNULContacts     renames gnutelight_contacts;

  procedure AddBlacklistedEP (EP : in LLU.End_Point_Type);
  function IsBlacklisted (EP : in LLU.End_Point_Type) return boolean;
  function IsTrasversalDirectory ( FileName : ASU.Unbounded_String ) return boolean;

private
  BlackListedEPs  : GNULContacts.Contacts_List_Type;

end Lince_Security;
