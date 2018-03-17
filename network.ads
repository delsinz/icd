with Principal;
with Measures;
with Ada.Containers.Doubly_Linked_Lists;

-- package to represnet the network component
package Network is
   
   -- a type representing a network
   type Network is private;
   
   -- the various network message types
   -- Note that all message types for messages going TO the ICD
   -- (i.e. those received by the ICD) are listed together purposefully
   type NetworkMessageType is (ModeOn,
                               ModeOff,
                               ReadRateHistoryRequest,
                               ChangeSettingsRequest,
                               ReadSettingsRequest,
                               ChangeSettingsResponse,
                               ReadSettingsResponse,
                               ReadRateHistoryResponse);
   
   -- the position of the first message type above that is received
   -- by the ICD
   FIRST_TOICD_MESSAGE_INDEX : constant Integer :=
     NetworkMessageType'Pos(ModeOn);

   -- the position of the last message type above that is received
   -- by the ICD
   LAST_TOICD_MESSAGE_INDEX : constant Integer :=
     NetworkMessageType'Pos(ReadSettingsRequest);
   
   -- a record that stores a heart rate measurement and the time at which
   -- the measurement was taken. These are for ReadRateHistoryResponse messages
   type RateRecord is record
      Rate : Measures.BPM;
      Time : Measures.TickCount;
   end record;
   
   -- rate histories, for ReadRateHistoryResponse messages
   HISTORY_LENGTH : constant Integer := 5;
   type RateHistory is array (Integer range 1..HISTORY_LENGTH) of RateRecord;
   
   
   -- the type for Network Messages. Note that some field names
   -- have a leading character or two to ensure that field names for
   -- the various message types are disjoint, as required by Ada.
   -- Thus e.g. the Source field of the ModeOn message 
   -- has the name MOnSource below; likewise the Destination field of
   -- ChangeSettingsResponse messages is called here CDestination, and so on.
   type NetworkMessage(MessageType : NetworkMessageType 
                         := ReadSettingsRequest) is record
      case MessageType is
         when ReadSettingsRequest =>
            RSource : Principal.PrincipalPtr; 
         when ReadSettingsResponse =>
            RDestination : Principal.PrincipalPtr;
            RTachyBound : Measures.BPM;
            RJoulesToDeliver : Measures.Joules;
         when ChangeSettingsRequest =>
            CSource : Principal.PrincipalPtr;
            CTachyBound : Measures.BPM;
            CJoulesToDeliver : Measures.Joules;
         when ChangeSettingsResponse =>
            CDestination : Principal.PrincipalPtr;
         when ModeOn =>
            MOnSource : Principal.PrincipalPtr;
         when ModeOff =>
            MOffSource : Principal.PrincipalPtr;
         when ReadRateHistoryRequest =>
            HSource : Principal.PrincipalPtr;
         when ReadRateHistoryResponse =>
            HDestination : Principal.PrincipalPtr;
            History : RateHistory;
         when others =>
            null;
      end case;
   end record;
   
   -- array of principals (e.g. passed into the Init procedure)
   type PrincipalArray is  array(Integer range <>) of Principal.PrincipalPtr;
   
   -- initialise the Network with the given array of known Principals who 
   -- might send messages on it. The network might also generate messages
   -- from other unknown Principals
   procedure Init(Net : out Network; KnownPrincipals : access PrincipalArray);
   
   -- send a message on the network
   procedure SendMessage(Net : in Network;
                         Message : in NetworkMessage);
   
   -- Whether a new message is available is placed in the NewMessageAvailable
   -- boolean. When this is set to True, the new message is placed in Message
   procedure GetNewMessage(Net : in out Network;
                           NewMessageAvailable : out Boolean;
                           Message : out NetworkMessage);
   
   -- for debugging to print out Network messages (partial implementation only)
   procedure DebugPrintMessage(Message : in NetworkMessage);

   -- called once per clock tick to update the network
   procedure Tick(Net : in out Network);
   
   
   
private
   type Network is record
      -- the Principals on the network
      Principals : access PrincipalArray;
      
      -- whether there is a message currently available to be read on the bus
      CurrentMessageAvailable : Boolean;

      -- if CurrentMessageAvailable, this holds the currently available message
      CurrentMessage : NetworkMessage;
   end record;

end Network;
