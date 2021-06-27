with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with graph; use graph;
procedure main is
   Name_junk : Ada.Strings.Unbounded.Unbounded_String;
   n: Natural;
   k: Natural;
   d: Natural;
   mySender : sender;
begin
   n := Integer'Value(Argument(1));
   d := Integer'Value(Argument(2));
   k := Integer'Value(Argument(3));
   initializeGraph(n,d);  --also initialize arrayOfTasks
   myReceiver.Start(k);
   myPrinter.Start(k, n);
   for i in 0..n-1 loop
      arrayOfTasks(i) := new NodeTask(i, allEdges(i));
   end loop;
   mySender.Start(k);
end main;
