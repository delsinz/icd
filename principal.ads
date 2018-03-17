package Principal is
   
   -- Roles
   type Role is (Cardiologist, ClinicalAssistant, Patient);
   
   type Principal is private;
   type PrincipalPtr is access all Principal;
   
   -- Convert a pointer to a Principal to a string
   -- This string is the memory address of the Principal object
   -- that the pointer points to.
   function PrincipalPtrToString(P : in PrincipalPtr) return String;
   
   -- Determines whether a particular role is associated with the given principal
   function HasRole(P : in Principal; R : Role) return Boolean;
   
   -- Initialise a principal for a specific role.
   -- This is how roles are assigned to Principals, i.e. they are
   -- assigned once at the time that Principals are created
   procedure InitPrincipalForRole(P : out Principal; R : Role);
   
   -- Debug print routine for Principals
   procedure DebugPrintPrincipal(P : in Principal);
   
   -- Debug print routine for PrincipalPtrs
   procedure DebugPrintPrincipalPtr(P : in PrincipalPtr);   
   
private
   type Principal is record
     MyRole : Role;
   end record;
end Principal;
   
