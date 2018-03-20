with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network; use Network;
with ICD; 
with Principal; use Principal;

package body ClosedLoop is

		Hrt : Heart.HeartType;
		HRMInst : HRM.HRMType; 
		Gen : ImpulseGenerator.GeneratorType;
		Net : Network.Network;
		ICDInst: ICD.ICDType;
    MsgAvailable : Boolean:= False;
    Msg : Network.NetworkMessage;
	  KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2); 
    -- create three Principals 
    Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
    Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
    Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
   

	procedure Init is
		  
		  
		
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
   		ICD.Init(ICDInst);

	end Init;


	procedure Tick is
      Response : Network.NetworkMessage;
	begin -- Tick

	   	Network.GetNewMessage(Net,MsgAvailable,Msg);
      --Network.DebugPrintMessage(Msg);	  
          if MsgAvailable then
        	    
              case Msg.MessageType is 
                  when ModeOn =>
                      
                      if ICDInst.IsModeOn then
                          null;
                      else
                          if Msg.MOnSource = Card or  Msg.MOnSource = Clin then
                              --ICD.On(HRMInst, Gen, ICDInst, Hrt);
                              ICD.On(ICDInst);
                              HRM.On(HRMInst,Hrt);
                              ImpulseGenerator.On(Gen);
                              Network.DebugPrintMessage(Msg);
                          
                              
                          end if;
                      end if;

                  when ModeOff =>
                      
                      if ICDInst.IsModeOn then
                          if Msg.MOffSource = Card or Msg.MOffSource = Clin then
                              ICD.Off(HRMInst,Gen,ICDInst);
                              Network.DebugPrintMessage(Msg);
                          
                          end if;
                      else
                          null;
                      end if;

                  when ReadRateHistoryRequest =>
                    
                      if ICDInst.IsModeOn and (Msg.HSource = Card or Msg.HSource = Clin)then
                              
                              Network.DebugPrintMessage(Msg); 
                              Response := ICD.ReadRateHistory(Msg, ICDInst);
                              Network.SendMessage(Net, Response);
                      end if;
                      
                  when ReadSettingsRequest =>

                      if ICDInst.IsModeOn then
                          --ReadSettings(Msg,ICDInst,Net);
                          Network.DebugPrintMessage(Msg);New_Line;
                          Put("Reading Settins");
                          Response := ICD.ReadSettings(Msg,ICDInst);
                          Network.SendMessage(Net, Response);

                      end if;


                  when ChangeSettingsRequest => 

                      if (not ICDInst.IsModeOn) and (Msg.CSource = Card or Msg.CSource = Clin) then
                              Network.DebugPrintMessage(Msg);New_Line;
                              Response := ICD.ChangeSettings(Msg, ICDInst);
                              Network.SendMessage(Net, Response);

                      end if;
                  when others =>
                      Put("Null"); New_Line;
              end case;           
      	  end if;

          Heart.Tick(Hrt);
          HRM.Tick(HRMInst, Hrt);
          Network.Tick(Net);
          ICD.Tick(ICDInst, HRMInst, Gen);

          if ICDInst.IsModeOn then
              ImpulseGenerator.Tick(Gen, Hrt);
          end if;
          
          -- Use for debug 
          Put("heart rate  = ");
          Put(Item => hrt.Rate);
          Put("      ");
          Put("Measured heart rate  = ");
          Put(Item => HRMInst.Rate);
          New_Line;
	
  end Tick;


end ClosedLoop;