with HRM;
with Measures;
with ImpulseGenerator;
with Network;
with Heart;
package ICD is
	-- The default tach bound.  b
	DEFAULT_TACH_BOUND : constant Integer := 100;
	-- Joules needs to deliver when VenFibrillation happens.
	DEFAULT_JOULES_TO_DELIVER : constant Integer := 30;
	-- Joules needs to deliver when Tachycardia happens.
	TACHY_JOULES_TO_DELIVER : constant Integer := 2;
	NO_JOULES : constant Integer := 0;
	-- The length of the array used to store 7 rate record. 
	HISTORY_LENGTH : constant Integer := 6;
	OBOVE_RATE : constant Integer := 15;
	DSECOND_PER_MIN :constant Integer := 600;
	BEFORE_HISTORY_LENGTH : constant Integer := 2;
	-- The times of treatment for Tachycardia. 
	TREATMENT_TIMES : constant Integer := 10;
	
	-- store two records before rate history array
	type BeforeHistoryType is array (Integer range 1..BEFORE_HISTORY_LENGTH)
	 of Network.RateRecord;

	type Settings is record
		TachBound : Measures.BPM;
		JoulesToDeliver :Measures.Joules;

	end record;


	Type ICDType is
		record
		RateHis: Network.RateHistory;
		BeforeHis: BeforeHistoryType;
		CurrentTime: Measures.TickCount;
		IsModeOn : Boolean;	
		ICDSettings : Settings;
		InTtreatment :Boolean;
		TreatCount : Integer;
		TickCounter : Integer;
		ImpulseFreq : Integer;
		end record;
	-- Create and initialise a new ICD software 	
	procedure Init(ICDInst: out ICDType);

	-- Function to trun off ICD 
    procedure Off(HRMInst: in out HRM.HRMType; 
    			Gen: in out ImpulseGenerator.GeneratorType; 
    			ICDInst: in out ICDType);

    -- Turn off hrm, impluse generator
    procedure On(ICDInst: in out ICDType);
	-- -- Function used to detect tachycardia.
	function IsTachycardia(CurrentRate : in Network.RateRecord; 
				ICDInst: in out ICDType) return Boolean;

	-- Function used to detect Ventricle Fibrillation
    function IsVenFibrillation(ICDInst : in  ICDType) return Boolean; 

    
	-- Function used to process read rate history message from network.
	function ReadRateHistory(Msg: in Network.NetworkMessage; 
				ICDInst : in ICDType) return Network.NetworkMessage;

	-- Function used to process read settings message from network
	function ReadSettings(Msg: in Network.NetworkMessage; ICDInst : in ICDType)
				return Network.NetworkMessage;

	-- Function used to process change settings message from network.
	function ChangeSettings(Msg: in Network.NetworkMessage; 
				ICDInst : in out ICDType) return Network.NetworkMessage;

	-- Function used to add the current rate to the rate history array. 
	procedure AddHistory(ICDInst : in out ICD.ICDType; 
				CurrentRate : in Network.RateRecord);

	-- Tick the clock, ICD start to work.
	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType;
			    Gen: in out ImpulseGenerator.GeneratorType);


end ICD;