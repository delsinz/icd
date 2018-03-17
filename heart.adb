with Measures; 
with RandomNumber;

package body Heart is
   
   -- Some boundaries for impulse shocks
   NoShock : constant Measures.Joules := 0;
   SmallShock : constant Measures.Joules := 5;
   
   -- Parameters for generating random heart rate upon initialisation
   HeartRateMu : constant Measures.BPM := 80;
   HeartRateSigma : constant Measures.BPM := 10;
   
   -- Used to simulate volatility of the patient's response to treatment
   Volatility : constant Float := 0.02;
   
   -- The amount to add to the heart rate to simulate rising rate
   DefaultChange : Integer := 1;
   
   -- The effect of a small shock on this heart
   
   -- A local function to limit Dosage measures
   procedure Init(Heart : out HeartType) is
   begin
      -- Generate a random systolic pressure
      Heart.Rate := 
	Measures.LimitBPM(RandomNumber.
			    NormalInteger(HeartRateMu,
					  HeartRateSigma));      
      Heart.Impulse := 0;
   end Init;
   
   procedure SetImpulse(Heart : in out HeartType; 
			Joules : in Measures.Joules) is
   begin
      Heart.Impulse := Joules;
   end SetImpulse;
   
   procedure GetRate(Heart : in HeartType;
		     Rate : out Measures.BPM) is
   begin
      Rate := Heart.Rate;
   end GetRate;
   
   function GetImpulse(Heart : in HeartType) return Measures.Joules is
   begin
      return Heart.Impulse;
   end GetImpulse;
   
   procedure Tick(Heart : in out HeartType) is
   begin
      if (Heart.Impulse = NoShock) then
	 -- No impulse, and the default behaviour of this heart is to increase
	 -- the rate slowly.
	 Heart.Rate := 
	   Measures.LimitBPM(Heart.Rate + DefaultChange);
      elsif (Heart.Impulse < SmallShock) then
	 -- A crude slowing of the heart given a shock
	 Heart.Rate := Measures.LimitBPM(Heart.Rate - Heart.Impulse);
      else -- a large shock
	 Heart.Rate := 0;
	 DefaultChange := 0;
      end if;
      
      -- Insert some random volatility
      Heart.Rate := 
	Measures.LimitBPM(RandomNumber.UniformIntegerWithError(Heart.Rate,
							       Volatility));
      
   end Tick;
   
end Heart;
