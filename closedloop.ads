-- This file defines the top-level interface of the closed loop system
-- that you will implement. Do not change this interface as we may run
-- automatic tests against your code that assume that it implements this
-- interface exactly.
package ClosedLoop is
  
  -- This procedure should create three Principals: one Patient, one 
  -- Cardiologist and one Clinical Assistant. These are the authorised 
  -- principals for the device, i.e. the Patient is one who has the
  -- device implanted in them, the Cardiologist is the patient's assigned
  -- cardiologist and the Clinical Assistant is their assigned
  -- clinical assistant.
  -- It should then create and initialise each of the components of the
  -- closed loop system, with the Network initialised so that the three
  -- principals mentioned above are the "known" principals (see network.ads)
  procedure Init;
  
  -- This procesure simulates one clock tick (decisecond)
  -- Besides calling the Tick procedures of each of the closed-loop
  -- components, it also needs to do things like handling network messages
  -- from authorised principals (e.g. by calling procedures of the ICD 
  -- package that you will write)
  procedure Tick;
  
end ClosedLoop;
