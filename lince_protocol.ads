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
with ADA.Streams;

package Lince_Protocol is

  package LLU renames Lower_Layer_UDP;
  package ASU renames ADA.Strings.Unbounded;
  package AS  renames ADA.Streams;

  -- Message types
  type TMessage_Type is (DATAREQ, DATA, DATAERR, HELLO, WELCOME, SEARCH, GOTIT);

  -- Option types
  type TOption_Type is (SIZEREQ, SIZE, FILE_NOT_FOUND, BLOCK_NOT_FOUND);

  -- Server End_Point
  EP_localserver : LLU.End_Point_Type;

end Lince_Protocol;
