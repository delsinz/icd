with HRM;
with Measures;
with ImpulseGenerator;
package ICD is
	
	--Type RateHistory is array(1..6) of Integer;
	TACH_BOUND : constant Integer := 100;
	HISTORY_LENGTH : constant Integer := 6;
	OBOVE_RATE : constant Integer := 15;
	DSECOND_PER_MIN :constant Integer := 600;

	InTtreatment : Boolean := False;
	TreatCount : Integer := 0;
	TickCount : Integer := 0;
	ImpulseFreq : Integer := 0;
	--RateHis : array(1..7) of Integer;

	Type RateHistoryType is array(1..7) of Integer;


	Type ICDType is
		record
		
		Rate : Measures.BPM;
		Impulse : Measures.Joules;
		RateHistory: RateHistoryType;
		--RateHis : RateHistory;
		--RateHis : array(1..6) of Integer;
		end record;

	procedure Init(ICDInst: out ICDType);

	function IsTachycardia(Rate: in Measures.BPM) return Boolean;

	function IsVenFibrillation(RateHis: in RateHistoryType) return Boolean; 

	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; RateHis: in out ICD.RateHistoryType );


end ICD;