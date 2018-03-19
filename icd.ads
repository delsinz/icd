with HRM;
with Measures;
with ImpulseGenerator;
with Network;
package ICD is
	
	--Type RateHistory is array(1..6) of Integer;
	DEFAULT_TACH_BOUND : constant Integer := 100;
	DEFAULT_JOULES_TO_DELIVER : constant Integer := 30;
	HISTORY_LENGTH : constant Integer := 6;
	OBOVE_RATE : constant Integer := 15;
	DSECOND_PER_MIN :constant Integer := 600;
	BEFORE_HISTORY_LENGTH : constant Integer := 2;
	--FULL_HISTORY_LENGTH: constant Integer : = 7;

	InTtreatment : Boolean := False;
	TreatCount : Integer := 0;
	TickCounter : Integer := 0;
	ImpulseFreq : Integer := 0;
	--RateHis : array(1..7) of Integer;

	--Type RateHistoryType is array(1..7) of Integer;
	
	type BeforeHistoryType is array (Integer range 1..BEFORE_HISTORY_LENGTH) of Network.RateRecord;

	type Settings is record
		TachBound : Measures.BPM;
		JoulesToDeliver :Measures.Joules;

	end record;


	Type ICDType is
		record
		
		Rate : Measures.BPM;
		Impulse : Measures.Joules;
		RateHis: Network.RateHistory;
		BeforeHis: BeforeHistoryType;
		CurrentTime: Measures.TickCount;
		IsModeOn : Boolean;	
		ICDSettings : Settings;
		end record;

	procedure Init(ICDInst: out ICDType);

	function IsTachycardia(Rate: in Measures.BPM; ICDInst: in ICDType) return Boolean;

    function IsVenFibrillation(ICDInst : in out ICDType) return Boolean; 

    procedure Off(HRMInst: in out HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; ICDInst: in out ICDType);

    procedure On(HRMInst: in out HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType; ICDInst: in out ICDType; Hrt: in Heart.HeartType);

	function ReadRateHistory(Msg: in Network.NetworkMessage; ICDInst : in ICDType) return Network.NetworkMessage;

	function ReadSettings(Msg: in Network.NetworkMessage; ICDInst : in ICDType) return  Network.NetworkMessage;

	function ChangeSettings(Msg: in Network.NetworkMessage; ICDInst : in out ICDType) return Network.NetworkMessage;

	procedure Tick(ICDInst : in out ICD.ICDType; HRMInst: in HRM.HRMType; Gen: in out ImpulseGenerator.GeneratorType );


end ICD;