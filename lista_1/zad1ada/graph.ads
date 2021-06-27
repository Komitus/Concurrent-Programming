with Ada.Containers.Vectors;
package graph is
   -- I use ptr types bcs i want use unconstrained types!!!

   n : Natural;
   package SuccessorsOfNode is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);

   type SuccessorsOfNodePtr is access SuccessorsOfNode.Vector;
   type EveryEdge is array (Natural range <>) of SuccessorsOfNodePtr;
   type EveryEdgePtr is access EveryEdge;
   allEdges : EveryEdgePtr;

   type Packet is record
         id : Natural;
   end record;

   task type NodeTask (id : Natural; sc : SuccessorsOfNodePtr) is
      entry handlePacket (myPacket : Packet);
   end NodeTask;
   type NodeTaskPtr is access NodeTask;

   type TasksArray is array (Natural range <>) of NodeTaskPtr;
   type TasksArrayPtr is access TasksArray;
   arrayOfTasks : TasksArrayPtr;

   procedure initializeGraph(n: Natural; d: Natural);

   task type sender is
     entry Start(k: Natural);
   end sender;

   task type receiver is
      entry Start(k:Natural);
      entry receive(receivedPacket : Packet);
   end receiver;

   myReceiver : receiver;

   task type printer is
      entry Start (k: Natural; n: Natural);
      entry printJump(nodeId: Natural; packetId: Natural);
      entry packetReceived(packetId: Natural);
      entry makeReport;
   end printer;

   package VectorForPrinter is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);
   type VectorForPrinterPtr is access VectorForPrinter.Vector;

   type NodesForPacket is array (Natural range <>) of VectorForPrinterPtr;
   type NodesForPacketPtr is access NodesForPacket;
   type PacketsForNode is array (Natural range <>) of VectorForPrinterPtr;
   type PacketsForNodePtr is access PacketsForNode;


   myPrinter : printer;


end graph;
