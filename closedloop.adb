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
    --Response : Network.NetworkMessage;

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
      
      HRM.Init(HRMInst);
   		Heart.Init(Hrt);
   		--HRM.Init(HRMInst);
   		ImpulseGenerator.Init(Gen);
   		Network.Init(Net, KnownPrincipals);
   		ICD.Init(ICDInst);

   		ICD.On(HRMInst, Gen, ICDInst, Hrt);


	end Init;

	procedure Tick is
      Response : Network.NetworkMessage;
	begin -- Tick

	   	Network.GetNewMessage(Net,MsgAvailable,Msg);
      --Network.DebugPrintMessage(Msg);	  
          if MsgAvailable then

        	    --Network.DebugPrintMessage(Msg);
        	    
              case Msg.MessageType is 
                  when ModeOn =>
                      
                      if ICDInst.IsModeOn then
                          null;
                      else
                          if Msg.MOnSource = Card or  Msg.MOnSource = Clin then
                              ICD.On(HRMInst, Gen, ICDInst, Hrt);
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
                      

                      --Network.DebugPrintMessage(Response); New_Line;
                      
                      if ICDInst.IsModeOn then
                          if Msg.HSource = Card or Msg.HSource = Clin then
                              --ICDInst.ReadRateHistory(Msg, ICDInst,Net);
                              Network.DebugPrintMessage(Msg); 
                              Response := ICD.ReadRateHistory(Msg, ICDInst);

                              --Network.DebugPrintMessage(Response); New_Line;
                              --Put("ahaa");
                              Network.SendMessage(Net, Response);
                              --Put("ahaa");


                          end if;
                      else
                          null;
                      end if;
                      
                  when ReadSettingsRequest =>

                      --Network.DebugPrintMessage(Msg);New_Line;

                      if ICDInst.IsModeOn then
                          --ReadSettings(Msg,ICDInst,Net);
                          Network.DebugPrintMessage(Msg);New_Line;
                          Put("Reading Settins");
                          Response := ICD.ReadSettings(Msg,ICDInst);
                          --Network.DebugPrintMessage(Response);New_Line;
                          Network.SendMessage(Net, Response);

                      end if;


                  when ChangeSettingsRequest => 
                      --Put("11111111");
                      --Network.DebugPrintMessage(Msg);New_Line;

                      if ICDInst.IsModeOn then
                          null;
                      else
                          if Msg.CSource = Card or Msg.CSource = Clin then
                              --ChangeSettings(Msg,ICDInst,Net);
                              Network.DebugPrintMessage(Msg);New_Line;
                              Response := ICD.ChangeSettings(Msg, ICDInst);
                              --Network.DebugPrintMessage(Response);New_Line;
                              Network.SendMessage(Net, Response);
                          end if;
                      end if;

                  when others =>
                      Put("Null"); New_Line;
              end case;
              delay 0.1;

      	  end if;
          
          
          HRM.Tick(HRMInst, Hrt);
          ImpulseGenerator.Tick(Gen, Hrt);
                    

          Heart.Tick(Hrt);
          Network.Tick(Net);
          
          ICD.Tick(ICDInst, HRMInst, Gen, Hrt);
          
          --Network.Tick(Net);
          Put("Measured heart rate  = ");
          Put(Item => HRMInst.Rate);
          New_Line;
				  
	end Tick;


end ClosedLoop;