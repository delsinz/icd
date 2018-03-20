with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with closedloop;

procedure test is
	
begin -- test
	closedloop.Init;

	for I in Integer range 1..1000 loop
		closedloop.Tick;
	end loop; 
end test;