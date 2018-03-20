with HRM;
with Measures; use Measures;
with ImpulseGenerator;
with Network; use Network;
with Heart;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is
	

	procedure Init(ICDInst: out ICDType) is		
	begin -- Init
		--ICDInst.Rate := Measures.BPM'First;
		ICDInst.Impulse := Measures.Joules'First; 	
		ICDInst.CurrentTime := 0;
		ICDInst.IsModeOn := False;
		--ICDInst.IsModeOn := True;
		ICDInst.ICDSettings.TachBound := DEFAULT_TACH_BOUND;
		ICDInst.ICDSettings.JoulesToDeliver := DEFAULT_JOULES_TO_DELIVER;

		for I in Integer range 1..5 loop
			ICDInst.RateHis(I).Rate := -1;
			ICDInst.RateHis(I).Time := 0;
		end loop;

		for J in Integer range 1..2 loop
			ICDInst.BeforeHis(J).Rate := 0;
			ICDInst.BeforeHis(J).Time := 0;
		end loop;

		--ICDInst.RateHistory := (Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First,Measures.BPM'First);
        --ICDInst.ModeStatus : Boolean := False;
        --ICDInst.RateHistory : Networks.RateHistory;
	end Init;


	-- function used to detect tachycardia
	function IsTachycardia(Rate: in Measures.BPM; ICDInst: in ICDType) return Boolean is		
	begin -- DetectTach
		if Rate > ICDInst.ICDSettings.TachBound then
			return True;
		end if;
		return False;
	end IsTachycardia;

	-- function used to detect tachycardia
	function IsVenFibrillation(ICDInst : in out ICDType) return Boolean is	
		type FullHistory is array (Integer range 1..7) of Integer; 
		AveChange : Integer := 0;
		TempValue : Integer := 0;
		FullHis: FullHistory := (0,0,0,0,0,0,0);

	begin -- IsVenFibrillation
		

		if ICDInst.BeforeHis(1).Rate = Measures.BPM'First then
			return False;
		end if;

		for I in Integer range 1..2 loop
			FullHis(I) := ICDInst.BeforeHis(I).Rate;
		end loop;


		for I in Integer range 3..7 loop
			FullHis(I) := ICDInst.RateHis(I-2).Rate;

		end loop;


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


	--procedure ProcessMessage(MsgType :in Networks.NetworkMessageType) is
		
	--begin -- ProcessMessage
	--	c
		
	--end ProcessMessage;
	procedure Off(HRMInst: in out HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; ICDInst: in out ICDType) is
		
	begin -- Off
		
		HRM.Off(HRMInst);
		ImpulseGenerator.Off(Gen);
		ICDInst.IsModeOn := False;
	end Off;

	procedure On(HRMInst: in out HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; ICDInst: in out ICDType; Hrt: in Heart.HeartType) is
	begin -- On
		HRM.On(HRMInst, Hrt);
		ImpulseGenerator.On(Gen);
		Put("ICD is on"); New_Line;
		ICDInst.IsModeOn := True;
	end On;

	function ReadRateHistory(Msg: in Network.NetworkMessage; ICDInst : in ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ReadRateHistoryResponse);
	begin -- ReadRateHistory
		--Network.DebugPrintMessage(Msg); 
		--Network.SendMessage(Net,
        --               (MessageType => Network.ReadRateHistoryResponse,
        --                HDestination => Msg.HSource,
        --                History => ICDInst.RateHis));

		--Response.MessageType := ReadRateHistoryResponse;
		Response.HDestination := Msg.HSource;
		Response.History := ICDInst.RateHis;
		return Response;
	end ReadRateHistory;


	function ReadSettings(Msg: in Network.NetworkMessage; ICDInst : in ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ReadSettingsResponse);
	begin -- name
		--Network.DebugPrintMessage(Msg); 
		--Network.SendMessage(Net,
        --               (MessageType => ReadSettingsResponse,
        --                RDestination => Msg.RSource,
        --                RTachyBound => ICDInst.ICDSettings.TachBound,
        --                RJoulesToDeliver => ICDInst.ICDSettings.JoulesToDeliver));

		--Response.MessageType := ReadSettingsResponse;
		Response.RDestination := Msg.RSource;
		Response.RTachyBound := ICDInst.ICDSettings.TachBound;
		Response.RJoulesToDeliver := ICDInst.ICDSettings.JoulesToDeliver;
		return Response;
	end ReadSettings;

	function ChangeSettings(Msg: in Network.NetworkMessage; ICDInst : in out ICDType) return Network.NetworkMessage is
		Response : Network.NetworkMessage(ChangeSettingsResponse);
	begin -- ChangeSettings
		--Network.DebugPrintMessage(Msg); 
		--Network.SendMessage(Net,
        --               (MessageType => ChangeSettingsResponse,
        --                CDestination => Msg.CSource,
        --                ICDSettings.TachBound => Msg.CTachyBound,
        --                ICDSettings.CJoulesToDeliver => Msg.CJoulesToDeliver));
		
		Response.CDestination := Msg.CSource;
		ICDInst.ICDSettings.TachBound := Msg.CTachyBound;
		ICDInst.ICDSettings.JoulesToDeliver := Msg.CJoulesToDeliver;
		return Response;
	end ChangeSettings;

	procedure AddHistory(ICDInst : in out ICD.ICDType; CurrentRate : in Network.RateRecord) is
		
	begin -- Add
		Put("");
		ICDInst.BeforeHis(1) := ICDInst.BeforeHis(2);
		ICDInst.BeforeHis(2) := ICDInst.RateHis(1);
		for J in Integer range 1..4 loop
			ICDInst.RateHis(J) := ICDInst.RateHis(J+1);
		end loop;
			
			ICDInst.RateHis(5) := CurrentRate;
			
	end AddHistory;

	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; Hrt: in out Heart.HeartType) is
		CurrentRate : Network.RateRecord;
		--Temp : Network.RateRecord;
	begin -- Tick

		--HRM.GetRate(HRMInst, CurrentRate.Rate);
		--Put("Measured heart rate  = ");
		--Put(Item => CurrentRate.Rate);
		--New_Line;

		-- HRM.GetRate(HRMInst, ICDInst.Rate);
		--Put("Measured heart rate  = ");
      	--Put(Item => ICDInst.Rate);
      	--New_Line;
		if ICDInst.IsModeOn then

			-- read heart rate from the hrm
			--ICDInst.CurrentTime := ICDInst.CurrentTime + 1; 
		
			--HRM.GetRate(HRMInst, ICDInst.Rate);

			HRM.GetRate(HRMInst, CurrentRate.Rate);
			Put("Measured heart rate  = ");
			Put(Item => CurrentRate.Rate);
			New_Line;
			CurrentRate.Time := ICDInst.CurrentTime;
			ICDInst.CurrentTime := ICDInst.CurrentTime+1;
			-- debug
			--Put("Measured heart rate  = ");
      		--Put(Item => ICDInst.Rate);
      		--New_Line;
			-- Record the lastest heart rate

			AddHistory(ICDInst, CurrentRate);

			--ICDInst.BeforeHis(1) := ICDInst.BeforeHis(2);
			--ICDInst.BeforeHis(2) := ICDInst.RateHis(1);
			--for J in Integer range 1..4 loop
			--	ICDInst.RateHis(J) := ICDInst.RateHis(J+1);
			--end loop;
			--Temp.Rate := CurrentRate.Rate;
			--ICDInst.RateHis(5) := CurrentRate;
			--ICDInst.RateHis(5).Rate := Temp.Rate;


			--ICDInst.RateHis(5).Time := ICDInst.CurrentTime +1;
		

			if InTtreatment = True then
				ImpulseFreq := DSECOND_PER_MIN/(ICDInst.Rate+OBOVE_RATE);
				if TickCounter = ImpulseFreq then
					TickCounter := 0;
					TreatCount := TreatCount -1;
					ImpulseGenerator.SetImpulse(Gen,2);
					if TreatCount = 0 then
						InTtreatment := False;
					end if;
				else 
					TickCounter := TickCounter + 1; 		
				end if;
			else 
				if IsTachycardia(ICDInst.Rate, ICDInst) then
					InTtreatment := True;
					TreatCount := 9;
					TickCounter := 0;
				
					if IsVenFibrillation(ICDInst) then
						--ImpulseGenerator.SetImpulse(Gen,30);
						null;
					else
						ImpulseGenerator.SetImpulse(Gen, 0);
					end if;
			
				else

					if IsVenFibrillation(ICDInst) then
						null;
						--ImpulseGenerator.SetImpulse(Gen,30);
					else
						ImpulseGenerator.SetImpulse(Gen, 0);
					end if;	
			
				end if;
			end if;	
		else
			ICDInst.CurrentTime := ICDInst.CurrentTime+1;		
		end if;
		
	end Tick;
end ICD;