with Measures;

-- This package defines a very simple and crude simulation of a heart.
--  The default behaviour is for the heart to speed up, unless it
--  receives a small shock, in which case it will slow down, or a
--  large shock, in which case it will cease working
package Heart is

   -- A type representing a heart
   type HeartType is
      record
	 -- The heart rate for the patient
	 Rate : Measures.BPM;
	 
	 -- The impulse to be administered in the next tick
	 Impulse : Measures.Joules;
	 
      end record;
   
   -- Initialise the heart, setting a heart rate from a normal
   -- probability distribution.
   procedure Init(Heart : out HeartType);
   
   -- Set the amount of joules to be administered at the next clock tick.
   -- This should only be called by the impulse generator.
   procedure SetImpulse(Heart : in out HeartType; Joules : in Measures.Joules);
   
   -- Read the amount of joules to be administered at the next clock tick.
   function GetImpulse(Heart : in HeartType) return Measures.Joules;
   
   -- Access the heart's real BPM.
   -- This should only be called by the HRM.
   procedure GetRate(Heart : in HeartType;
		     Rate : out Measures.BPM);
   
   -- Tick the clock, providing an impulse to the heart.
   procedure Tick(Heart : in out HeartType);

 end Heart;

