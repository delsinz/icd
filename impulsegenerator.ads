with Measures; 
with Heart;

-- This package simulates a simple impulse generator for an ICD. It is
--  provided with an amount to discharge, and provides this amount to
--  a heart in the next 'tick'.
package ImpulseGenerator is
   
   -- The generator type
   type GeneratorType is
      record
	 -- The current impulse; to be administered to the heart at
	 --  the next clock tick
	 Impulse : Measures.Joules;  
   
	 -- Indicates whether the generator has been turned on.
	 IsOn : Boolean;
      end record;
   
   -- Create and initialise a new pump.
   procedure Init(Gen : out GeneratorType);
   
   -- Turn on the pump, but do not administer any impulse yet.
   procedure On(Gen : in out GeneratorType);
   
   -- Turn off the pump;
   procedure Off(Gen : in out GeneratorType);
   
   -- Query the status of the pump (on/off)
   function IsOn(Gen : in GeneratorType) return Boolean;
   
   -- Set the impulse to be administered
   procedure SetImpulse(Gen : in out GeneratorType; J : in Measures.Joules);
   
   -- Tick the clock, providing an impulse to the heart.
   procedure Tick(Gen : in GeneratorType; Hrt : in out Heart.HeartType);
   
end ImpulseGenerator;

