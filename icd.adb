with HRM;
with Measures; use Measures;
with ImpulseGenerator;
with Network; use Network;
with Heart;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is
	
	-- Create and initialise a new ICD software 
	procedure Init(ICDInst: out ICDType) is		
	begin -- Init 	
		-- Initialise the time 
		ICDInst.CurrentTime := 0;
		-- The initail state of ICD is off 
		ICDInst.IsModeOn := False;
		-- Initialise the ICD settings;
		ICDInst.ICDSettings.TachBound := DEFAULT_TACH_BOUND;
		ICDInst.ICDSettings.JoulesToDeliver := DEFAULT_JOULES_TO_DELIVER;

		-- Initialise the array,which is used to stored rate history
		for I in Integer range 1..5 loop
			ICDInst.RateHis(I).Rate := -1;
			ICDInst.RateHis(I).Time := 0;
		end loop;

		-- Initail the array,which store two records before rate history
		-- array, this two arrays are used to determine if VenFibrillation
		-- happens. 
		for J in Integer range 1..2 loop
			ICDInst.BeforeHis(J).Rate := -1;
			ICDInst.BeforeHis(J).Time := 0;
		end loop;

		-- Determine if heart is in treatment state
		ICDInst.InTtreatment := False;
		-- Initailise the number needs to count 
		ICDInst.TreatCount := TREATMENT_TIMES;
		-- Initailise the counter used to determine if need to send impulse
		-- if TickCounter equal to the frequency of sending joules then 
		-- send impulse. 
		ICDInst.TickCounter := 0;
		ICDInst.ImpulseFreq := -1;
	end Init;


	-- Function used to detect tachycardia.
	function IsTachycardia(CurrentRate : in Network.RateRecord; 
				ICDInst: in out ICDType) return Boolean is		
	begin 
		if CurrentRate.Rate > ICDInst.ICDSettings.TachBound then
			-- Use to debug
			Put("Tachycardia Detected!!!");New_Line;
			-- Calculate the frequency of sending impulse
			ICDInst.ImpulseFreq := 
				DSECOND_PER_MIN/(CurrentRate.Rate+OBOVE_RATE);
			return True;
		end if;
		return False;
	end IsTachycardia;

	-- Function used to detect Ventricle Fibrillation
	function IsVenFibrillation(ICDInst : in  ICDType) return Boolean is	
		type FullHistory is array (Integer range 1..7) of Integer; 
		AveChange : Integer := 0;
		TempValue : Integer := 0;
		-- Array contains the rate infomation of rate history array and 
		-- before history rate array. 
		FullHis: FullHistory := (0,0,0,0,0,0,0);

	begin -- IsVenFibrillation

		-- Check if the first element in the before history array. If it is
		-- equal to -1, which means there is no 7 stored rate record and can 
		-- not determine if there is Ventricle Fibrillation.
		if ICDInst.BeforeHis(1).Rate = Measures.BPM'First then
			return False;
		end if;
 
		-- Transfer the records in BeforeHis array to FullHis array.
		for I in Integer range 1..2 loop
			FullHis(I) := ICDInst.BeforeHis(I).Rate;
		end loop;

		-- Transfer the records in RateHis array to FullHis array.
		for I in Integer range 3..7 loop
			FullHis(I) := ICDInst.RateHis(I-2).Rate;

		end loop;

		-- Calculte average of 6 differences by using FullHis array. 
		for I in Integer range 1..6 loop

			TempValue := abs(FullHis(I+1) - FullHis(I)) + TempValue;
		end loop;
			AveChange := TempValue/6;

		if AveChange >= 10 then

			return True;
		else
			return False;
		end if;
	end IsVenFibrillation;

	-- Function to trun off ICD 
	procedure Off(HRMInst: in out HRM.HRMType; 
		Gen: in out ImpulseGenerator.GeneratorType; 
		ICDInst: in out ICDType) is
		
	begin -- Off
		-- Turn off hrm, impluse generator  
		HRM.Off(HRMInst);
		ImpulseGenerator.SetImpulse(Gen,0);
		ImpulseGenerator.Off(Gen);
		ICDInst.IsModeOn := False;
		ICDInst.InTtreatment := False;

		-- Clear all history record
		for I in Integer range 1..5 loop
			ICDInst.RateHis(I).Rate := -1;
			ICDInst.RateHis(I).Time := 0;
		end loop;

		for J in Integer range 1..2 loop
			ICDInst.BeforeHis(J).Rate := -1;
			ICDInst.BeforeHis(J).Time := 0;
		end loop;

	end Off;

	-- Function used to trun on ICD 
	procedure On(ICDInst: in out ICDType) is
	begin -- On

		Put("ICD is on"); New_Line;
		ICDInst.IsModeOn := True;
	end On;

	-- Function used to process read rate history message from network. 
	function ReadRateHistory(Msg: in Network.NetworkMessage; 
		ICDInst : in ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ReadRateHistoryResponse);
	begin -- ReadRateHistory
		Response.HDestination := Msg.HSource;
		Response.History := ICDInst.RateHis;
		return Response;
	end ReadRateHistory;

	-- Function used to process read settings message from network. 
	function ReadSettings(Msg: in Network.NetworkMessage; 
		ICDInst : in ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ReadSettingsResponse);
	begin 

		Response.RDestination := Msg.RSource;
		Response.RTachyBound := ICDInst.ICDSettings.TachBound;
		Response.RJoulesToDeliver := ICDInst.ICDSettings.JoulesToDeliver;
		return Response;
	end ReadSettings;

	-- Function used to process change settings message from network.
	function ChangeSettings(Msg: in Network.NetworkMessage; 
		ICDInst : in out ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ChangeSettingsResponse);
	begin -- ChangeSettings
		
		Response.CDestination := Msg.CSource;
		ICDInst.ICDSettings.TachBound := Msg.CTachyBound;
		ICDInst.ICDSettings.JoulesToDeliver := Msg.CJoulesToDeliver;
		return Response;
	end ChangeSettings;

	-- Function used to add the current rate to the rate history array. 
	procedure AddHistory(ICDInst : in out ICD.ICDType; 
		CurrentRate : in Network.RateRecord) is
		
	begin 
		-- Throw away the first element in BeforeHis array
		-- move the rest elements forward, and add current 
		-- record to the last postion. 
		ICDInst.BeforeHis(1) := ICDInst.BeforeHis(2);
		ICDInst.BeforeHis(2) := ICDInst.RateHis(1);
		for J in Integer range 1..4 loop
			ICDInst.RateHis(J) := ICDInst.RateHis(J+1);
		end loop;
			
			ICDInst.RateHis(5) := CurrentRate;
			
	end AddHistory;

	-- Tick the clock, ICD start to work.
	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType; 
		Gen: in out ImpulseGenerator.GeneratorType) is
		CurrentRate : Network.RateRecord;
	begin -- Tick

		-- Determine if the ICD is on 
		if ICDInst.IsModeOn then
			-- Record the current rate and time 
			CurrentRate.Rate := HRMInst.Rate;
			CurrentRate.Time := ICDInst.CurrentTime +1;

			-- Add current rate to history rate array 
			AddHistory(ICDInst, CurrentRate);
			
			-- Determine if the ICD is in treatment state 
			if ICDInst.InTtreatment then
				-- Used to debug 
				Put(ICDInst.ImpulseFreq); New_Line;
				-- Check if need to send impulse
				if ICDInst.TickCounter = ICDInst.ImpulseFreq then
						-- Time to send impulse, initilise the 
						-- initialse tick couter, the treatment time
						-- needs to substract one 
						ICDInst.TickCounter  := 0;
						ICDInst.TreatCount := ICDInst.TreatCount -1;
						-- send two joules
						ImpulseGenerator.SetImpulse(Gen,
							TACHY_JOULES_TO_DELIVER);
						-- Used to debug 
						Put("Give 2 Joules"); New_Line;
						-- determine the remaining treat could is 0 
						if ICDInst.TreatCount = 0 then																								
							ICDInst.InTtreatment := False;
							ICDInst.ImpulseFreq := -1;
							ICDInst.TickCounter := 0;
							ImpulseGenerator.SetImpulse(Gen,NO_JOULES);
						end if;
				
				else 
					-- Don't need to send impulse 
					ICDInst.TickCounter := ICDInst.TickCounter + 1; 
					ImpulseGenerator.SetImpulse(Gen,NO_JOULES);		
				end if;
			else 
				-- If Tachycardia happens 
				if IsTachycardia(CurrentRate, ICDInst) then
					
					-- Set the treatment state be true, initialise
					-- the treat cound and tick counter. 
					ICDInst.InTtreatment := True;
					ICDInst.TreatCount := TREATMENT_TIMES ;
					ICDInst.TickCounter := 0;
					
					-- If Tachycardia and VenFibrillation happen at 
					-- same time. 
					if IsVenFibrillation(ICDInst) then
						-- Send impules to handle VenFibrillation.					
						ImpulseGenerator.SetImpulse(Gen,
							DEFAULT_JOULES_TO_DELIVER);												
					else
						ImpulseGenerator.SetImpulse(Gen, NO_JOULES);
					end if;
			
				else
					-- If Tachycardia not happens but VenFibrillation 
					-- happens. Send impulse to handle VenFibrillation.
					if IsVenFibrillation(ICDInst) then
						
						ImpulseGenerator.SetImpulse(Gen,
							DEFAULT_JOULES_TO_DELIVER);
					else
						ImpulseGenerator.SetImpulse(Gen, NO_JOULES);
					end if;	
			
				end if;
			end if;	
	
					
		end if;
		-- Record the current time.
		ICDInst.CurrentTime := ICDInst.CurrentTime+1;
	end Tick;
end ICD;