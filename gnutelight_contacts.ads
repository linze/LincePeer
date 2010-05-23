with Lower_Layer_UDP;


package Gnutelight_Contacts is

   package LLU renames Lower_Layer_UDP;

   -- tipo para representar una lista de End_Points que se conocen
   type Contacts_List_Type is limited private;

   -- añade un End_Point a una lista de contactos SI NO ESTÁ YA EN ELLA
   --                    (si ya estuvier en ella, no hace nada)
   procedure Add_One (CL: in out Contacts_List_Type;
                      EP:     in LLU.End_Point_Type);

   -- devuelve el número de elementos que hay ahora en la lista de contactos
   function Total (CL: Contacts_List_Type) return Natural;

   -- devuelve el End_Point del contacto número N de la lista de contactos
   function Get_One (CL: Contacts_List_Type;
                     N: Natural)
                     return LLU.End_Point_Type;

   -- copia el contenido de una lista a otra
   procedure Copy ( From : in Contacts_List_Type;
                    To   : out Contacts_List_Type);

  -- devuelve si un EP está en la lista o no
  function Is_Added (CL : Contacts_List_Type;
                     EP : LLU.End_Point_Type) return Boolean;

   -- excepción elevada por Get_One si se le pide una posición N inexistente
   No_Such_Contact: exception;

private

   MAX_CONTACTS: constant := 100;
   type Contacts_Array is array (1..MAX_CONTACTS) of LLU.End_Point_Type;

  protected type Contacts_List_Type is
      function Is_Added (EP : LLU.End_Point_Type) return Boolean;
      procedure Add_One  (EP: LLU.End_Point_Type);
      function Total return Natural;
      function Get_One  (N: Natural) return LLU.End_Point_Type;
   private
      The_List: Contacts_Array;
      Current: Natural := 0;
   end Contacts_List_Type;


end Gnutelight_Contacts;
