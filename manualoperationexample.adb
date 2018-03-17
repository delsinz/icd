with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;

-- This procedure demonstrates a simple composition of the network,
-- heart rate  monitor (HRM), heart, and impulse generator, with three
-- known principals (a cardiologist, clinical assistant and patient).
procedure ManualOperationExample is
   Hrt : Heart.HeartType;                -- The simulated heart
   Monitor : HRM.HRMType;                -- The simulated heart rate monitor
   Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
   HeartRate : BPM;
   Net : Network.Network;                -- The simulated network
   Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
   Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
   Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
   
   -- an array of known principals to use to initialise the network
   -- but note that the network can generate messages from other, unknown,
   -- principals too
   KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2); 
   
   -- stores whether there was a message available on the network
   MsgAvailable : Boolean := False;
   
   -- stores the current message read from the network (if one was available)
   Msg : Network.NetworkMessage;
   
   -- stores some history information on measured heart rate
   History : Network.RateHistory;
   HistoryPos : Integer := History'First;
   CurrentTime : TickCount := 0;  -- current time as measured in ticks
begin
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
   
   -- Initialise the components and turn the machines on
   Heart.Init(Hrt);
   HRM.Init(Monitor);
   ImpulseGenerator.Init(Generator);
   Network.Init(Net,KnownPrincipals);
   
   HRM.On(Monitor, Hrt);
   ImpulseGenerator.On(Generator);
   
   -- Set the new impulse to 0
   ImpulseGenerator.SetImpulse(Generator, 0); 
   
   -- Loop 100 times with zero joule impulse
   for I in Integer range 0..100 loop
      
      -- read messages from the network but don't act on them here,
      -- just print them out
      Network.GetNewMessage(Net,MsgAvailable,Msg);
      if MsgAvailable then
         Network.DebugPrintMessage(Msg);
      end if;
      
      -- Read and print the current measured heart rate
      HRM.GetRate(Monitor, HeartRate);
      Put("Measured heart rate  = ");
      Put(Item => HeartRate);
      New_Line;
      
      -- record the initial history only
      if HistoryPos <= History'Last then
         History(HistoryPos) := (Rate => HeartRate, Time => CurrentTime);
         HistoryPos := HistoryPos + 1;
      end if;
      
      -- Tick all components to simulate the passage of one decisecond
      ImpulseGenerator.Tick(Generator, Hrt);
      HRM.Tick(Monitor, Hrt);
      Heart.Tick(Hrt);
      Network.Tick(Net);
      
      CurrentTime := CurrentTime + 1;
      delay 0.1;
   end loop;
   
   -- Turn off the monitor: should return -1.0 for the next readings
   HRM.Off(Monitor);
   
   HRM.GetRate(Monitor, HeartRate);
   ImpulseGenerator.Tick(Generator, Hrt);
   HRM.Tick(Monitor, Hrt);
   Heart.Tick(Hrt);
   Network.Tick(Net);
   CurrentTime := CurrentTime + 1;
   
   Put("Heart rate should be -1 = ");
   Put(Item => HeartRate);
   New_Line;
   
   -- Turn the machine back on
   HRM.On(Monitor, Hrt);
   
   -- Ramp up the impulse 
   ImpulseGenerator.SetImpulse(Generator, 4);

   for I in Integer range 0..100 loop
      HRM.GetRate(Monitor, HeartRate);
      
      Put("After turn on = ");
      Put(Item => HeartRate);
      New_Line;     
      
      ImpulseGenerator.Tick(Generator, Hrt);
      HRM.Tick(Monitor, Hrt);
      Heart.Tick(Hrt);
      Network.Tick(Net);
      CurrentTime := CurrentTime + 1;
      delay 0.1;
   end loop; 
   
   -- send a rate history response message even though none was requested
   -- and it contains history that is not current
   Network.SendMessage(Net,
                       (MessageType => Network.ReadRateHistoryResponse,
                        HDestination => Patient,
                        History => History));
   
end ManualOperationExample;
