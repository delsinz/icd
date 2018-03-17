with Measures;
with RandomNumber;
with Principal;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Network is
   
   -- for the purposes of this crude simulation, we create three "unknown"
   -- principals that might also send messages on the network, besides the
   -- known principals supplied when Init is called
   UnknownCard : Principal.PrincipalPtr := new Principal.Principal;    
   UnknownClin : Principal.PrincipalPtr := new Principal.Principal;    
   UnknownPatient : Principal.PrincipalPtr := new Principal.Principal;    
   
   -- we implement a very crude probabilistic model of the network
   -- the folloing is the probability that any message arrives each clock tick
   NewMessageProbabilityPerTick : constant Uniformly_Distributed := 0.1;

   -- generate a random message for the ICD on the Network
   function GenerateRandomMessage(Principals : in PrincipalArray) 
                                 return NetworkMessage is
      MsgType : NetworkMessageType :=
        NetworkMessageType'Val(
            RandomNumber.UniformInteger(FIRST_TOICD_MESSAGE_INDEX,
                                        LAST_TOICD_MESSAGE_INDEX)
                             );
      NewJoules : Measures.Joules :=
        Measures.LimitJoules(
               RandomNumber.UniformInteger(Measures.MIN_JOULES,
                                           Measures.MAX_JOULES)
                          );
      NewBPM : Measures.BPM :=
        Measures.LimitBPM(
               RandomNumber.UniformInteger(Measures.MIN_BPM,
                                           Measures.MAX_BPM)
                          );
      
      Prin : Principal.PrincipalPtr;
   begin
      Prin :=
        Principals(
                   RandomNumber.UniformInteger(Principals'First,
                                               Principals'Last)
                  );
      
      case MsgType is
         when ModeOn =>
            return (MessageType => ModeOn,
                    MOnSource => Prin);
         when ModeOff =>
            return (MessageType => ModeOff,
                    MOffSource => Prin);
         when ReadRateHistoryRequest =>
            return (MessageType => ReadRateHistoryRequest,
                    HSource => Prin);
         when ReadSettingsRequest =>
            return (MessageType => ReadSettingsRequest,
                    RSource => Prin);
         when ChangeSettingsRequest =>
            return (MessageType => ChangeSettingsRequest,
                    CSource => Prin,
                    CTachyBound => NewBPM,
                    CJoulesToDeliver => NewJoules);
         when others =>
            -- shouldn't get here
            raise Ada.Assertions.Assertion_Error;
      end case;
   end GenerateRandomMessage;

   -- initialise the network by generating its first state
   procedure Init(Net : out Network; KnownPrincipals : access PrincipalArray) is
   begin
      -- the princpals array for the network accommodates the 3 extra unknown
      -- ones too
      Net.Principals := new PrincipalArray(KnownPrincipals'First..KnownPrincipals'Last+3);
      -- copy of the known principals
      for Index in KnownPrincipals'Range loop
         Net.Principals(Index) := KnownPrincipals(Index);
      end loop;
      -- initialise the unknown principals
      Principal.InitPrincipalForRole(UnknownCard.all,Principal.Cardiologist);
      Principal.InitPrincipalForRole(UnknownClin.all,Principal.ClinicalAssistant);
      Principal.InitPrincipalForRole(UnknownPatient.all,Principal.Patient);
      
      -- add the three unknown ones
      Net.Principals(KnownPrincipals'Last+1) := UnknownCard;
      Net.Principals(KnownPrincipals'Last+2) := UnknownClin;      
      Net.Principals(KnownPrincipals'Last+3) := UnknownPatient;            
      
      -- generate the first message
      -- Net.CurrentMessageAvailable is a boolean type 
      Net.CurrentMessageAvailable :=
        RandomNumber.RandomBooleanWithBias(NewMessageProbabilityPerTick);
      if Net.CurrentMessageAvailable then
         Net.CurrentMessage := GenerateRandomMessage(Net.Principals.all);
      end if;
   end Init;

   procedure GetNewMessage(Net : in out Network;
                           NewMessageAvailable : out Boolean;
                           Message : out NetworkMessage) is
   begin
      NewMessageAvailable := Net.CurrentMessageAvailable;
      if Net.CurrentMessageAvailable then
         -- this holds the currently available message
         Message := Net.CurrentMessage;
      end if;
   end GetNewMessage;



   procedure SendMessage(Net : in Network;
                         Message : in NetworkMessage) is
   begin
      -- in this simulation, just print the message to mimic sending it
      Ada.Text_IO.Put("Sending message on Network: "); New_Line;
      DebugPrintMessage(Message);
   end SendMessage;
   
   -- only partial implementation provided
   procedure DebugPrintMessage(Message : in NetworkMessage) is
   begin
      case Message.MessageType is
         when ModeOn =>
            Put("ModeOn (MOnSource: ");
            Principal.DebugPrintPrincipalPtr(Message.MOnSource);
            Put(")"); New_Line;
         when ModeOff =>
            Put("ModeOff (MOffSource: ");
            Principal.DebugPrintPrincipalPtr(Message.MOffSource);
            Put(")"); New_Line;
         when ReadRateHistoryRequest =>
            Put("ReadRateHistoryRequest (HSource: ");
            Principal.DebugPrintPrincipalPtr(Message.HSource);
            Put(")"); New_Line;
         when ReadRateHistoryResponse =>
           Put("ReadRateHistoryRequest (HDestination: ");
           Principal.DebugPrintPrincipalPtr(Message.HDestination);
           Put("; History: "); 
           for Index in Message.History'Range loop
              Ada.Integer_Text_IO.Put(Integer(Message.History(Index).Rate));
              Put(" @ "); Ada.Integer_Text_IO.Put(Integer(Message.History(Index).Time));
              Put(", ");
           end loop;
           Put(")"); New_Line;
         when others =>
            -- you should implement these for your own debugging if you wish
            null;
      end case;
   end DebugPrintMessage;

   procedure Tick(Net : in out Network) is
   begin
      Net.CurrentMessageAvailable :=
        RandomNumber.RandomBooleanWithBias(NewMessageProbabilityPerTick);
      if Net.CurrentMessageAvailable then
         -- generate a random Prin from Net.Principals.all
         Net.CurrentMessage := GenerateRandomMessage(Net.Principals.all);
      end if;
   end Tick;
   
end Network;
