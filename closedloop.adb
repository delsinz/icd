with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
-- with ICD;
with Principal;

package ClosedLoop is

	procedure Init is

	begin -- Init
		Hrt : Heart.HearType;
		Monitor : HRM.HRMType; 
		Generator : ImpulseGenerator.GeneratorType;
		Net : Network.Network；
		-- Icd : ICD.ICDType;
		
		
	end Init;


end ClosedLoop;