-- This package provides some basic measures and limit functions for
--  the ICD case study.
package Measures is
   
   MIN_JOULES : constant Integer := 0;
   MAX_JOULES : constant Integer := 45;
   
   -- The type for joules (unit of energy used in generator)
   subtype Joules is Integer range MIN_JOULES .. MAX_JOULES;
   
   MIN_BPM : constant Integer := -1;
   MAX_BPM : constant Integer := 300;
   
   -- The type for heart rate: beats per minute
   subtype BPM is Integer range MIN_BPM .. MAX_BPM;
   
   -- The type for the current time, measured in number of ticks
   type TickCount is mod 2**32;  -- 32-bit unsigned integers
   
   -- A function to limit floats
   function Limit(Input : in Integer; Fst : in Integer; Lst : in Integer) 
		 return Integer;
   --# pre Fst <= Lst;
   --# return Output => (Fst <= Output and Output <= Lst);
   
   -- A function to limit MmHg measures
   function LimitBPM(Input : in Integer) return BPM;
   --# return Output => (MmHg'First <= Output and Output <= MmHg'Last);
   
   -- A function to limit Dosage measures
   function LimitJoules(Input : in Integer) return Joules;
   --# return Output => (Dosage'First <= Output and Output <= MmHg'Last);
      
end Measures;
