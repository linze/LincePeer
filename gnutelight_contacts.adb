package body Gnutelight_Contacts is

  use type LLU.End_Point_Type;

  protected body Contacts_List_Type is

    function Is_Added (EP : LLU.End_Point_Type) return Boolean is
      begin
         for I in 1..Current loop
            if EP = The_List(I) then
               return True;
            end if;
         end loop;
         return False;
      end Is_Added;


      procedure Add_One (EP: in LLU.End_Point_Type) is
      begin
         if not Is_Added (EP) then
            if Current < MAX_CONTACTS then
               Current := Current + 1;
               The_List (Current) := EP;
            end if;
         end if;
      end Add_One;

      function Total return Natural is
      begin
         return Current;
      end;

      function Get_One (N: Natural) return LLU.End_Point_Type is
      begin
         if N <= Current then
            return The_List(N);
         else
            raise No_Such_Contact;
         end if;
      end Get_One;

   end Contacts_List_Type;


   procedure Add_One (CL: in out Contacts_List_Type;
                      EP: LLU.End_Point_Type) is
   begin
      CL.Add_One (EP);
   end Add_One;


   function Total (CL: Contacts_List_Type) return Natural is
   begin
      return CL.Total;
   end Total;


   function Get_One (CL: Contacts_List_Type;
                     N: Natural)
                     return LLU.End_Point_Type is
   begin
      return CL.Get_One (N);
   end Get_One;

   procedure Copy ( From : in Contacts_List_Type;
                    To   : out Contacts_List_Type) is
   begin
     for I in 1..Total(From) loop
       Add_One (To, Get_One (From, I));
     end loop;
  end Copy;

  function Is_Added  (CL : Contacts_List_Type;
                     EP : LLU.End_Point_Type) return Boolean is
  begin
    return CL.Is_Added(EP);
  end Is_Added;

end Gnutelight_Contacts;
