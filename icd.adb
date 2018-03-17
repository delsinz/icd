with HRM;
with Measures;
with ImpulseGenerator;

package body ICD is
	

	procedure Init(ICDInst: out ICDType) is		
	begin -- Init
		ICDInst.Rate := Measures.BPM'First;
		ICDInst.Impulse := Measures.Joules'First; 	
		ICDInst.RateHistory := (Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First);
        --ICD.RateHis := (Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First);	
	end Init;


	-- function used to detect tachycardia
	function IsTachycardia(Rate: in Measures.BPM) return Boolean is		
	begin -- DetectTach
		if Rate > TACH_BOUND then
			return True;
		end if;
		return False;
	end IsTachycardia;

	-- function used to detect tachycardia
	function IsVenFibrillation(RateHis: in ICD.RateHistoryType) return Boolean is	
		AveChange : Integer := 0;
		TempValue : Integer := 0;
	begin -- IsVenFibrillation
	
		if RateHis(1) = Measures.BPM'First then
			return False;
		end if;

		for I in Integer range 1..6 loop

			TempValue := abs(RateHis(I+1) - RateHis(I)) + TempValue;
		end loop;
			AveChange := TempValue/6;

		if AveChange >= 10 then
			return True;
		else
			return False;
		end if;
	end IsVenFibrillation;

	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; RateHis: in out RateHistoryType ) is
		
	begin -- Tick
		-- read heart rate from the hrm 
		HRM.GetRate(HRMInst, ICDInst.Rate);

		-- Record the lastest heart rate 
		for J in Integer range 1..6 loop
			RateHis(J) := RateHis(J+1);
		end loop;

		RateHis(7) := ICDInst.Rate;

		if InTtreatment = True then
			ImpulseFreq := DSECOND_PER_MIN/(ICDInst.Rate+OBOVE_RATE);
			if TickCount = ImpulseFreq then
				TickCount := 0;
				TreatCount := TreatCount -1;
				ImpulseGenerator.SetImpulse(Gen,2);
				if TreatCount = 0 then
					InTtreatment := False;
				end if;
			else 
				TickCount := TickCount + 1; 		
			end if;
		else 
			if IsTachycardia(ICDInst.Rate) then
				InTtreatment := True;
				TreatCount := 9;
				TickCount := 0;
				
				if IsVenFibrillation(RateHis) then
					ImpulseGenerator.SetImpulse(Gen,30);
				else
					ImpulseGenerator.SetImpulse(Gen, 0);
				end if;
			
			else

				if IsVenFibrillation(RateHis) then
					ImpulseGenerator.SetImpulse(Gen,30);
				else
					ImpulseGenerator.SetImpulse(Gen, 0);
				end if;	
			
			end if;
		end if;
	end Tick;
end ICD;