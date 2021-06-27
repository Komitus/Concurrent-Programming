with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with graph; use graph;
procedure main is
   n, d : Natural;
begin
   n := Natural'Value(Argument(1));
   d := Natural'Value(Argument(2));

   startSimulation(n, d);
end main;
