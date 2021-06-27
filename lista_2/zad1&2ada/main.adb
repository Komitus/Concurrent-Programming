with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with graph; use graph;
procedure main is
   n, d, b, k, h, numOfTraps : Natural;
begin
   n := Natural'Value(Argument(1));
   d := Natural'Value(Argument(2));
   b := Natural'Value(Argument(3));
   k := Natural'Value(Argument(4));
   h := Natural'Value(Argument(5));
   numOfTraps := Integer'Value(Argument(6));

   startSimulation(n, d, b, k, h, numOfTraps);

end main;
