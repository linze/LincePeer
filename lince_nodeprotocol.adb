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
-- LincePeer is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with LincePeer.  If not, see <http://www.gnu.org/licenses/>.
--
------------------------------------------------------------------------


with Lower_Layer_UDP;
with ADA.Strings.Unbounded;
with ADA.Unchecked_Deallocation;
with ADA.Streams;
with ADA.Exceptions;
with Ada.IO_Exceptions;
with Lince_Protocol;
with Lince_IO;

use type Ada.Strings.Unbounded.Unbounded_String;

package body Lince_NodeProtocol is

  procedure SendHello  ( To       : in LLU.End_Point_Type;
                         Hello    : in THello) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset(HELLOBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.HELLO);
    Natural'Output (Buffer'Access, Hello.Options);
    LLU.End_Point_Type'Output (Buffer'Access, Hello.EPres);
    LLU.End_Point_Type'Output (Buffer'Access, Hello.EPsvc);
    if Hello.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, Hello.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LNodeProtocol", "SendHello"
                      ,"Sent Hello petition " & HelloToString(Hello));
  exception
    when Ex : others => LIO.DebugError ("LNodeProtocol","SendHello",Ex);
  end SendHello;

  procedure SendWelcome ( To       : in LLU.End_Point_Type;
                          Welcome  : in TWelcome) is
    Buffer      : aliased LLU.Buffer_Type (AS.Stream_Element_Offset(WELCOMEBUFFER));
  begin
    LLU.Reset (Buffer);
    LProtocol.TMessage_Type'Output (Buffer'Access, LProtocol.WELCOME);
    Natural'Output (Buffer'Access, Welcome.Options);
    Natural'Output (Buffer'Access, GNULContacts.Total (Welcome.Peers));
    for I in 1 .. GNULContacts.Total (Welcome.Peers) loop
      LLU.End_Point_Type'Output (Buffer'Access, GNULContacts.Get_One (Welcome.Peers,I));
    end loop;
    if Welcome.Options = 1 then
      LProtocol.TOption_Type'Output (Buffer'Access, Welcome.OptionType);
    end if;

    LLU.Send (To, Buffer'Access);

    LIO.VerboseDebug ("LNodeProtocol", "SendWelcome"
                      ,"Sent Welcome response " & WelcomeToString(Welcome));
  exception
    when Ex : others => LIO.DebugError ("LNodeProtocol","SendWelcome",Ex);
  end SendWelcome;

  procedure GetHello   ( Buffer   : access LLU.Buffer_Type;
                         Hello    : out THello) is
  begin
    Hello.Options := Natural'Input (Buffer);
    Hello.EPres := LLU.End_Point_Type'Input (Buffer);
    Hello.EPsvc := LLU.End_Point_Type'Input (Buffer);
    if Hello.Options = 1 then
      Hello.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LNodeProtocol", "GetHello"
                      ,"Readed " & HelloToString(Hello));
  exception
    when Ex : others => LIO.DebugError ("LNodeProtocol","GetHello",Ex);
  end GetHello;

  procedure GetWelcome ( Buffer   : access LLU.Buffer_Type;
                         Welcome  : out TWelcome) is
  begin
    Welcome.Options    := Natural'Input (Buffer);
    Welcome.N          := Natural'Input (Buffer);
    for I in 1 .. Welcome.N loop
      GNULContacts.Add_One (Welcome.Peers, LLU.End_Point_Type'Input(Buffer));
    end loop;

    if Welcome.Options = 1 then
      Welcome.OptionType := LProtocol.TOption_Type'Input (Buffer);
    end if;

    LIO.VerboseDebug ("LNodeProtocol", "GetWelcome"
                      ,"Readed " & WelcomeToString(Welcome));
  exception
    when Ex : others => LIO.DebugError ("LNodeProtocol","GetWelcom",Ex);
  end GetWelcome;

  function HelloToString ( Hello : in THello) return String is
    UString      : ASU.Unbounded_String;
  begin
    UString := "Options: " & Natural'Image (Hello.Options) &
               " EPres: " & ASU.To_Unbounded_String(LLU.Image (Hello.EPres)) &
               " EPsvc: " & ASU.To_Unbounded_String (LLU.Image (Hello.EPsvc));
    if Hello.Options = 1 then
      UString := UString & " Option: ";
      case Hello.OptionType is
        when LProtocol.SIZEREQ         => UString := UString & "SIZEREQ";
        when LProtocol.SIZE            => UString := UString & "SIZE";
        when LProtocol.FILE_NOT_FOUND  => UString := UString & "FILE_NOT_FOUND";
        when LProtocol.BLOCK_NOT_FOUND => UString := UString & "BLOCK_NOT_FOUND";
        when others => UString := UString & "Unknown option";
      end case;
    end if;
    return ASU.To_String (UString);
  end HelloToString;

  function WelcomeToString ( Welcome : in TWelcome) return String is
    UString      : ASU.Unbounded_String;
  begin
    UString := "Options: " & ASU.To_Unbounded_String(Natural'Image (Welcome.Options)) &
               " N: " & ASU.To_Unbounded_String(Natural'Image (Welcome.N));

    for i in 1 .. Welcome.N loop
      UString := UString & " EndPoint" & Positive'Image (i) & " ";
      UString := UString & ASU.To_Unbounded_String(LLU.Image(GNULContacts.Get_One (Welcome.Peers,i)));
    end loop;

    if Welcome.Options = 1 then
      UString := UString & " Option: ";
      case Welcome.OptionType is
        when LProtocol.SIZEREQ         => UString := UString & "SIZEREQ";
        when LProtocol.SIZE            => UString := UString & "SIZE";
        when LProtocol.FILE_NOT_FOUND  => UString := UString & "FILE_NOT_FOUND";
        when LProtocol.BLOCK_NOT_FOUND => UString := UString & "BLOCK_NOT_FOUND";
        when others => UString := UString & "Unknown option";
      end case;
    end if;
    return ASU.To_String (UString);
  end WelcomeToString;




end Lince_NodeProtocol;
