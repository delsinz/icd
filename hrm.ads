with Measures;
with Heart;

-- This package simulates a heart rate monitor. It is provided with a
--  HeartType record, and must read the heart rate from this. Some
--  random error is introduced into the readings..
package HRM is
   
   -- The record type for a heart rate monitor
   type HRMType is
      record
	 -- The measure heart rate for the heart
	 Rate : Measures.BPM;
	 
	 IsOn : Boolean;  -- Indicates whether the HRM is on.
      end record;
   
   -- Create and initialise a HRM.
   procedure Init(Hrm : out HRMType);
   
   -- Turn on the HRM and get a first reading from the heart.
   procedure On(Hrm : out HRMType; Hrt : in Heart.HeartType);
      
   -- Turn off the HRM.
   procedure Off(Hrm : in out HRMType);
   
   -- Get the status of the HRM (on/off)
   function IsOn(Hrm : in HRMType) return Boolean;
   --# return B.IsOn;
   
   -- Access the *measured* heart rate
   procedure GetRate(Hrm : in HRMType;
		     Rate : out Measures.BPM);
      
   -- Tick the clock, reading the heart rate from the heart
   procedure Tick(Hrm : in out HRMType; Hrt : in Heart.HeartType);

end HRM;
