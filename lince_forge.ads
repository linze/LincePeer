with ADA.Strings.Unbounded;
with ADA.Strings.Unbounded.Text_IO;
with ADA.Text_IO;
with ADA.Streams;
with Lower_Layer_UDP;
with Lince_Protocol;
with Lince_FileProtocol;
with Lince_IO;


package Lince_Forge is

  package ASU              renames ADA.Strings.Unbounded;
  package ASU_IO           renames ADA.Strings.Unbounded.Text_IO;
  package A_IO             renames ADA.Text_IO;
  package AS               renames ADA.Streams;
  package LLU              renames Lower_Layer_UDP;
  package LProtocol        renames Lince_Protocol;
  package LFileProtocol    renames Lince_FileProtocol;
  package LIO              renames Lince_IO;


  -- Data forge
  procedure ForgeDataReq;
  procedure ForgeData;
  procedure ForgeDataErr;

  -- Nodes forge
  -- Search forge
end Lince_Forge;
