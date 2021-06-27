with Ada.Containers.Vectors;

package graph is

   procedure startSimulation (n, d : in Natural);
   procedure initializeGraph(n, d : in Natural);

   type PacketData is record
      node_id : Natural;
      cost : Natural;
   end record;

   package OfferVector is new Ada.Containers.Vectors
   (Index_Type   => Natural,
    Element_Type => PacketData);

   type Packet is record
      cameFrom : Natural;
      offer : OfferVector.Vector;
   end record;


   -- I use ptr types bcs i want use unconstrained types!!!
   package SuccessorsOfNode is new Ada.Containers.Vectors
   (Index_Type   => Natural,
    Element_Type => Natural);
   type SuccessorsOfNodePtr is access SuccessorsOfNode.Vector;
   type EveryEdge is array (Natural range <>) of SuccessorsOfNodePtr;
   type EveryEdgePtr is access EveryEdge;

   type RoutingCell is record
         nextHop : Natural;
         cost : Natural;
         changed : Boolean;
   end record;
   type RoutingTable is array (Natural range <>) of RoutingCell;
   type RoutingTablePtr is access RoutingTable;
   type EveryRoutingTable is array (Natural range <>) of RoutingTablePtr;
   type EveryRoutingTablePtr is access EveryRoutingTable;

   task type NodeTask (id : Natural; n : Natural; sc : SuccessorsOfNodePtr; passedRT : RoutingTablePtr) is
      entry handlePacket (receivedPacket : Packet);
      entry stop;
   end NodeTask;
   type NodeTaskPtr is access NodeTask;



   type TasksArray is array (Natural range <>) of NodeTaskPtr;
   type TasksArrayPtr is access TasksArray;

   task type PrinterTask(n : Natural)  is
      entry print(message : String);
   end PrinterTask;

   type PrinterTaskPtr is access PrinterTask;

end graph;
