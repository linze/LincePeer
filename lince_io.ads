with Ada.Text_IO;
with Ada.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with ADA.Streams.Stream_IO;
with ADA.Exceptions;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Lince_Config;

package Lince_IO is

  package ASU              renames ADA.Strings.Unbounded;
  package ASU_IO           renames ADA.Strings.Unbounded.Text_IO;
  package A_IO             renames ADA.Text_IO;
  package AS_IO            renames ADA.Streams.Stream_IO;
  package ACal             renames Ada.Calendar;
  package GCT_IO           renames Gnat.Calendar.Time_IO;
  package LConfig          renames Lince_Config;

  type TOutputType is (mtNORMAL, mtRECEIVED, mtSENT, mtINFORMATION, mtERROR, mtASKINFO, mtDEBUG, mtOTHER);

  procedure Notify (Message : String; MessageType : in TOutputType);
  procedure GetInfo ( Question : in String; Answer : out ASU.Unbounded_String );

  procedure VerboseDebug ( PackageName     : String;
                           MethodName      : String;
                           Message         : String);

  procedure DebugError ( PackageName     : String;
                         MethodName      : String;
                         Ex              : ADA.Exceptions.Exception_Occurrence);

  procedure StartLog;
  procedure Log (Message : in String);
end Lince_IO;
