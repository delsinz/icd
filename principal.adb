with System.Address_Image;
with System.Address_To_Access_Conversions;
with Ada.Text_IO;

Package body Principal is
   
   function HasRole(P : in Principal; R : Role) return Boolean is
   begin
      return (R = P.MyRole);
   end HasRole;
   
   procedure InitPrincipalForRole(P : out Principal; R : Role) is
   begin
      P.MyRole := R;
   end InitPrincipalForRole;   
   
   -- there is a bit of magic required to convert a PrincipalPtr to a string
   function PrincipalPtrToString(P : in PrincipalPtr) return String is
      package ATAC is new System.Address_To_Access_Conversions(Principal);
   begin
      return System.Address_Image(ATAC.To_Address(ATAC.Object_Pointer(P)));
   end PrincipalPtrToString;
   
   procedure DebugPrintPrincipalPtr(P : in PrincipalPtr) is
   begin
      Ada.Text_IO.Put(PrincipalPtrToString(P));
      Ada.Text_IO.Put(" -> ");
      DebugPrintPrincipal(P.all);
   end DebugPrintPrincipalPtr;
   
   procedure DebugPrintPrincipal(P : in Principal) is
      R : Role := P.MyRole;
   begin
      Ada.Text_IO.Put(R'Image);
   end DebugPrintPrincipal;
end Principal;
