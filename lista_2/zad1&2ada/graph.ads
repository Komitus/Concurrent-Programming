with Ada.Containers.Vectors;
package graph is

   procedure startSimulation (n, d, b, k, h, numOfTraps : in Natural);
   procedure initializeGraph(n, d, b : Natural);

   type Packet is record
      id : Natural;
      ttl : Natural;
   end record;

   -- I use ptr types bcs i want use unconstrained types!!!
   package SuccessorsOfNode is new Ada.Containers.Vectors
   (Index_Type   => Natural,
    Element_Type => Natural);
   type SuccessorsOfNodePtr is access SuccessorsOfNode.Vector;
   type EveryEdge is array (Natural range <>) of SuccessorsOfNodePtr;
   type EveryEdgePtr is access EveryEdge;

   task type NodeTask (id : Natural; sc : SuccessorsOfNodePtr; n : Natural) is
      entry handlePacket (myPacket : Packet);
      entry madeTrap;
   end NodeTask;
   type NodeTaskPtr is access NodeTask;

   type TasksArray is array (Natural range <>) of NodeTaskPtr;
   type TasksArrayPtr is access TasksArray;

   task type sender is
     entry Start(k, h : Natural);
   end sender;

   task type receiver is
      entry Start(k : Natural);
      entry receive(receivedPacket : Packet);
      entry packetDied;
   end receiver;

   task type hunter is
      entry Start(numOfTraps, n : Natural);
   end hunter;

   task type printer is
      entry Start(n, k : Natural);
      entry printJump(packetId, nodeId : Natural);
      entry packetReceived(packetId: Natural);
      entry packetDied(packetId, nodeId : Natural);
      entry trapMade(nodeId : Natural);
      entry packetTrapped(packetId, nodeId : Natural);
      entry makeReport;
   end printer;

end graph;
