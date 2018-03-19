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
		Gen : ImpulseGenerator.GeneratorType;
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
   		ImpulseGenerator.Init(Gen);
   		Network.Init(Net, KnownPrincipals);
   		ICDInst.Init(ICDInst);

   		ICDInst.On(HRMInst, Generator, ICDInst, Hrt);


	end Init;

	procedure Tick() is

	    Response : Network.NetworkMessage;
	begin -- Tick

	   	Network.GetNewMessage(Net,MsgAvailable,Msg);
      	  
          if MsgAvailable then

        	    Network.DebugPrintMessage(Msg);
        	    
              case Msg.MessageType is 
                  when ModeOn =>

                      if Msg.MOnSource = Card or Mes.MOnSource = Clin then
                          ICDInst.On(HRMInst, Gen, ICDInst, Hrt);
                          Put("ICD system is on!")
                      else
                          Put("No Authorised!");
                      end if;

                  when ModeOff =>
                      if Msg.MOffSource = Card or Msg.MOffSource = Clin then
                          ICDInst.Off(HRMInst,Gen,ICDInst);
                          Put("ICD system is shutted down!")
                      else
                          Put("No Authorised!")
                      end if;
                  when ReadRateHistoryRequest =>
                      if ICDInst.IsModeOn then
                          if Msg.HSource = Card or Msg.HSource = Clin then
                              Response := ICDInst.ReadRateHistory(Msg, ICDInst);
                              Net.DebugPrintMessage(Response);
                              Net.SendMessage(Net, Response);

                          end if;
                      else
                          Put("Can not read history when ICD is ModeOff");
                      end if;
                  when ReadSettingsRequest =>
                      if ICDInst.IsModeOn then
                          Response := ICDInst.ReadSettings(Msg,ICDInst);
                          Net.DebugPrintMessage(Response);
                          Net.SendMessage(Net, Response);
                      end if;
                  when ChangeSettingsRequest => 
                      if ICDInst.IsModeOn then
                          Put("Settings can not be changed when ICD is ModeOn");
                      else
                          if Msg.CSource = Card or Msg.CSource = Clin then
                              Response := ICDInst.ChangeSettings(Msg, ICDInst);
                              Net.DebugPrintMessage(Response);
                              Net.SendMessage(Net, Response);
                          end if;
                      end if;       
              end case;


      	  end if;

          Heart.Tick(Hrt);
          HRM.Tick(HRMInst, Hrt);
          ICD.Tick(ICDInst, HRMInst, Gen, Hrt);
          ImpulseGenerator.Tick(Gen, Hrt);
          Network.Tick(Net);

				
	end Tick;


end ClosedLoop;