with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with ICD;
with Principal;

package ClosedLoop is

		Hrt : Heart.HearType;
		HRMInst : HRM.HRMType; 
		Generator : ImpulseGenerator.GeneratorType;
		Net : Network.Network；
		ICDInst: ICD.ICDType;
	
	procedure Init is
		-- create three Principals 
   		Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
   		Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
  		Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
		
		KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2);
		
	begin -- Init
		
		-- set up the principals with the correct roles
   		Principal.InitPrincipalForRole(Card.all,Principal.Cardiologist);
   		Principal.InitPrincipalForRole(Clin.all,Principal.ClinicalAssistant);
   		Principal.InitPrincipalForRole(Patient.all,Principal.Patient);
   		KnownPrincipals(0) := Card;
   		KnownPrincipals(1) := Clin;
   		KnownPrincipals(2) := Patient;
   
   		Put("Known Principals: "); New_Line;
   		Principal.DebugPrintPrincipalPtr(Card); New_Line;
   		Principal.DebugPrintPrincipalPtr(Clin); New_Line;
   		Principal.DebugPrintPrincipalPtr(Patient); New_Line;	

   		Heart.Init(Hrt);
   		HRM.Init(HRMInst);
   		ImpulseGenerator.Init(Generator);
   		Network.Init(Net, KnownPrincipals);
   		ICDInst.Init(ICDInst);

   		ICDInst.On(HRMInst, Generator, ICDInst, Hrt);


	end Init;

	procedure Tick() is
		
	begin -- Tick
		
	end Tick;


end ClosedLoop;