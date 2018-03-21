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
    KnownPrincipals : access Network.PrincipalArray 
                := new Network.PrincipalArray(0..2); 
    
    -- A cardiologist
    Card : Principal.PrincipalPtr := new Principal.Principal;
    -- A clinical assistant  
    Clin : Principal.PrincipalPtr := new Principal.Principal;
    -- A patient
    Patient : Principal.PrincipalPtr := new Principal.Principal;

-- initialise the whole system
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
      
      -- Initialse heart, heart moniter, impulse generator,
      -- newwork and ICD. 
      Heart.Init(Hrt);
      HRM.Init(HRMInst);
      ImpulseGenerator.Init(Gen);
      Network.Init(Net, KnownPrincipals);
      ICD.Init(ICDInst);

  end Init;

  -- Tick the clock, the whole system start to work. 
  procedure Tick is
      Response : Network.NetworkMessage;
  begin -- Tick

      -- Get a new message from network.
      Network.GetNewMessage(Net,MsgAvailable,Msg);
          
          -- Check if the message is available 
          if MsgAvailable then             
              case Msg.MessageType is 
                  -- Process the message with mode on type. 
                  when ModeOn =>
                      -- If the ICD software is off, only card and clin could
                      -- turn it on. 
                      if(not ICDInst.IsModeOn) and (Msg.MOnSource = Card 
                          or  Msg.MOnSource = Clin) then 
                          -- Turn on all components.
                          ICD.On(ICDInst);
                          HRM.On(HRMInst,Hrt);
                          ImpulseGenerator.On(Gen);
                          Network.DebugPrintMessage(Msg);
                      end if;

                  -- Process the message with mode off type. 
                  when ModeOff =>
                      -- If the ICD software is on, only card and clin could
                      -- turn it off.  
                      if ICDInst.IsModeOn and (Msg.MOffSource = Card or 
                          Msg.MOffSource = Clin)then
                          -- Trun off all components.
                          ICD.Off(HRMInst,Gen,ICDInst);
                          -- Set the impulse to be 0.
                          Heart.SetImpulse(Hrt,0);
                          Network.DebugPrintMessage(Msg);
                      end if;
                  -- Process the message with ReadRateHistoryRequest type.
                  when ReadRateHistoryRequest =>
                      -- All roles could read history when the ICD is on.
                      if ICDInst.IsModeOn then
                          Network.DebugPrintMessage(Msg); 
                          -- Get the response message.
                          Response := ICD.ReadRateHistory(Msg, ICDInst);
                          -- Sent the response message.
                          Network.SendMessage(Net, Response);
                      end if;
                  
                  -- Process the message with ReadSettingsRequest type. 
                  when ReadSettingsRequest =>
                      -- Only card and clin could read settings when the ICD 
                      -- is off.
                      if (not ICDInst.IsModeOn) and (Msg.RSource = Card or
                            Msg.RSource = Clin ) then
                          Network.DebugPrintMessage(Msg);New_Line;
                          -- Get the reponse message. 
                          Response := ICD.ReadSettings(Msg,ICDInst);
                          -- Send the reponse messge.
                          Network.SendMessage(Net, Response);

                      end if;

                  -- Process the message with ChangeSettingsRequest type. 
                  when ChangeSettingsRequest => 
                      -- Only card and clin could read settings when the ICD 
                      -- is on.
                      if (not ICDInst.IsModeOn) and (Msg.CSource = Card or
                          Msg.CSource = Clin) then        
                          Network.DebugPrintMessage(Msg);New_Line;
                          -- Get the reponse message. 
                          Response := ICD.ChangeSettings(Msg, ICDInst);
                          -- Send the reponse messge.
                          Network.SendMessage(Net, Response);
                      end if;

                  when others =>
                      Put("Null"); New_Line;
              end case;
              

          end if;
          -- Tick all components.
          Heart.Tick(Hrt);
          HRM.Tick(HRMInst, Hrt);
          Network.Tick(Net);
          ICD.Tick(ICDInst, HRMInst, Gen);
          ImpulseGenerator.Tick(Gen, Hrt);

          -- Used to debug
          Put("heart rate  = ");
          Put(Item => hrt.Rate);
          Put("      ");
          Put("Measured heart rate  = ");
          Put(Item => HRMInst.Rate);
          
          
          New_Line;
                    
          delay 0.1;
  end Tick;


end ClosedLoop;