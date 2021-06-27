with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Numerics.Float_Random; 


package body graph is
  
   myPrinter : printer;
   myReceiver : receiver;
   mySender : sender;
   myHunter : hunter;
   arrayOfTasks : TasksArrayPtr;
   allEdges : EveryEdgePtr;
   
   procedure startSimulation (n, d, b, k, h, numOfTraps : in Natural) is
   begin
     
      if d > (n-3)*n/2+1 or b > (n-1)*n/2 then
         Put_Line("Too big amount of shortcuts. Impossible to make this");
         return;
      end if;
      initializeGraph(n, d, b);
      
      arrayOfTasks := new TasksArray(0..n-1);
      
      myReceiver.Start(k);
      myPrinter.Start(n, k);
      for i in 0..n-1 loop
         arrayOfTasks(i) := new NodeTask(i, allEdges(i), n);
      end loop;
      myHunter.Start(numOfTraps, n);  --i start hunter even if i want 0 traps
      mySender.Start(k, h);           --that bcs otherwise program don't terminate
                                      --but in this case everything works fine
   end startSimulation;
   
   procedure initializeGraph(n, d, b : Natural) is
      
      floatGenerator : Ada.Numerics.Float_Random.Generator;
      src : Natural;
      dest : Natural;
      repeated : Boolean;
      c: Natural;
   begin
      
      Ada.Numerics.Float_Random.Reset(floatGenerator);
        
      allEdges := new EveryEdge(0..n-1);
      for i in 0..n-2 loop
         allEdges(i) := new SuccessorsOfNode.Vector;
         allEdges(i).all.Append(i+1);
      end loop;
      allEdges(n-1) := new SuccessorsOfNode.Vector;
         
      --forward shortcuts
      c := 0;
      while c < d loop
            
         repeated := false;
         src := Natural(Ada.Numerics.Float_Random.Random(floatGenerator)*Float(n-2)) mod (n-2);
         dest := src + 2 + Natural(Ada.Numerics.Float_Random.Random(floatGenerator)*Float(n-src-2)) mod (n-src-2);
          
         for j in 0..allEdges(src).Last_Index loop
            if allEdges(src).Element(j) = dest then
               repeated := true;
            end if;
         end loop;

         if repeated = false then
            allEdges(src).all.Append(dest);
            c:=c+1;
         end if;
         
      end loop;
         
      --backward shortcuts
      c := 0;
      while c < b loop
            
         repeated := false;
         src := 1 + Natural(Ada.Numerics.Float_Random.Random(floatGenerator)*Float(n-1)) mod (n-1);
         dest :=  Natural(Ada.Numerics.Float_Random.Random(floatGenerator)*Float(src)) mod (src);
          
         for j in 0..allEdges(src).Last_Index loop
            if allEdges(src).Element(j) = dest then
               repeated := true;
            end if;
         end loop;

         if repeated = false then
            allEdges(src).all.Append(dest);
            c:=c+1;
         end if;
         
      end loop;
         
      for I in 0..n-1 loop
         Put_Line("Node "&Integer'Image(I)&": ");
         Put("     -->:");
         for J in 0..allEdges(I).Last_Index loop
            Put(Integer'Image(allEdges(I).Element(J))&",");
         end loop;
         Put_Line("");
      end loop;
   end;
   
   --task type NodeTask (id : Natural; sc : SuccessorsOfNodePtr; n : Natural)
   task body NodeTask is
      subtype Random_Range_Index is Natural range 0 .. sc.Last_Index;
      package RRI is new Ada.Numerics.Discrete_Random (Random_Range_Index);
      GRI : RRI.Generator;
      nextTask : Natural;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      localPacket : Packet;
      trapped : Boolean;
   begin
      RRI.Reset (GRI);
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      trapped := False;
      loop
         select
            accept handlePacket (myPacket : Packet) do
               localPacket := myPacket;
            end handlePacket;
            if trapped = False then
               
               if localPacket.ttl > 0 then
           
                  myPrinter.printJump(packetId => localPacket.id, nodeId   => id);
                  localPacket.ttl := localPacket.ttl - 1;
                  delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
                  if id = n-1 then
                     myReceiver.receive(receivedPacket => localPacket);
                  else
                     nextTask := sc.Element(RRI.Random(GRI));
                     arrayOfTasks.all(nextTask).handlePacket(myPacket => localPacket);
                  end if;
               else 
                  myPrinter.packetDied(packetId => localPacket.id, nodeId => id);
               end if;
            else 
               trapped := False;
               myPrinter.packetTrapped(packetId => localPacket.id, nodeId => id);
            end if;
         or
            accept madeTrap  do
               trapped := True;
            end madeTrap;
         or
            terminate;
         end select;
      end loop;
   end;

   task body sender is
      packetToSend : Packet;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      ttl : Natural;
      k : Natural;
   begin
      
     accept Start(k, h : Natural) do 
         ttl := h;
         sender.k := k;
     end Start;
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      for i in 0..k-1 loop
         delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
         packetToSend := Packet'(id => i, ttl => ttl);
         arrayOfTasks.all(0).handlePacket(myPacket => packetToSend);
      end loop;
   end;

   task body receiver is
      packetsCounter : Natural;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      packetId : Natural;
      k : Natural;
   begin
      accept Start(k : Natural) do
         receiver.k := k;
      end Start;
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      packetsCounter := 0;
      
      while packetsCounter < k loop  
         select
            accept receive(receivedPacket : Packet) do
               packetId := receivedPacket.id;
            end receive;
            myPrinter.packetReceived(packetId);
            packetsCounter:=packetsCounter+1;
            delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
         or accept packetDied  do
               packetsCounter := packetsCounter + 1;
            end packetDied;
         end select;
      end loop;
      myPrinter.makeReport;
   end;
   
   task body hunter is
      floatGenerator : Ada.Numerics.Float_Random.Generator;
      numOfTraps : Natural;
      nodeToTrap : Natural;
      n : Natural;
   begin
      accept Start(numOfTraps, n : Natural) do
         hunter.numOfTraps := numOfTraps;
         hunter.n := n;
      end Start;
      Ada.Numerics.Float_Random.Reset(floatGenerator);
      for i in 0..numOfTraps-1 loop 
         nodeToTrap := Natural(Ada.Numerics.Float_Random.Random(floatGenerator) * Float(n)) mod n; 
         myPrinter.trapMade(nodeId => nodeToTrap);     --i want firstly pass info about that to avoid situation that
         arrayOfTasks.all(nodeToTrap).madeTrap;        --packet will be trapped before "trap was made" is printed
         delay Standard.Duration(Ada.Numerics.Float_Random.Random(floatGenerator));
      end loop;

   end;
   
   task body printer is
      package VectorForPrinter is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Natural);
      type VectorForPrinterPtr is access VectorForPrinter.Vector;
      type NodesForPacket is array (Natural range <>) of VectorForPrinterPtr;
      type NodesForPacketPtr is access NodesForPacket;
      type PacketsForNode is array (Natural range <>) of VectorForPrinterPtr;
      type PacketsForNodePtr is access PacketsForNode;
      packetsArray : NodesForPacketPtr;
      nodesArray : PacketsForNodePtr;
      localPacketId : Natural;
      localNodeId : Natural;
      n : Natural;
      k: Natural;
   begin
      accept Start(n, k : Natural) do
         printer.n := n;
         printer.k := k;
      end Start;
      
      nodesArray := new PacketsForNode(0..n-1);
      packetsArray := new NodesForPacket(0..k-1);
      
      for i in 0..k-1 loop
         packetsArray(i) := new VectorForPrinter.Vector;
      end loop;
      for i in 0..n-1 loop
         nodesArray(i) := new VectorForPrinter.Vector;
      end loop;

      loop
         select
            accept printJump (packetId : in Natural; nodeId : in Natural) do
               localPacketId := packetId;
               localNodeId := nodeId;
            end printJump;
            Put_Line("Packet" & Natural'Image(localPacketId) & " in Node"&Natural'Image(localNodeId));
            packetsArray(localPacketId).all.Append(localNodeId);
            nodesArray(localNodeId).all.Append(localPacketId);
         or
            accept packetReceived (packetId : in Natural) do
               localPacketId := packetId;
            end packetReceived;
                Put_Line(ESC & "[38;5;51m" & "Packet" & Natural'Image(localPacketId) & " received "& ESC & "[0m");
         or accept packetDied (packetId : in Natural; nodeId : in Natural) do
               localPacketId := packetId;
            end packetDied;
               Put_Line(ESC & "[38;5;196m" & "Packet" & Natural'Image(localPacketId) & " died in" &
                          Natural'Image(localNodeId) & " node" & ESC & "[0m" );
               packetsArray(localPacketId).all.Append(localNodeId);
               nodesArray(localNodeId).all.Append(localPacketId);
               myReceiver.packetDied; 
         or accept packetTrapped (packetId : in Natural; nodeId : in Natural) do
               localPacketId := packetId;
               localNodeId := nodeId;
            end packetTrapped;
               Put_Line(ESC & "[38;5;156m" & "Packet" & Natural'Image(localPacketId) & " got trapped in" &
                          Natural'Image(localNodeId) & " node" & ESC & "[0m" );
               packetsArray(localPacketId).all.Append(localNodeId);
               nodesArray(localNodeId).all.Append(localPacketId);
               myReceiver.packetDied;  -- no difference packet died or was trapped, i just want increment counter
         or accept trapMade (nodeId : in Natural) do
               localNodeId := nodeId;
            end trapMade;
               Put_Line("Hunter is attempting to make a trap in node " & Natural'Image(localNodeId));
                 
         or
            accept makeReport  do
               Put_Line("");
               Put_Line("########################################");
               Put_Line("######## Report of simulation  #########");
               Put_Line("########################################");

               Put_Line("Print packets for every Node");
               for i in 0..n-1 loop
                  Put_Line("Node"&Natural'Image(i)&":");
                  Put("  Packets: ");
                  for j in 0..nodesArray(i).Last_Index loop
                     Put(Natural'Image(nodesArray(i).Element(j))&",");
                  end loop;
                  Put_Line("");
               end loop;
               Put_Line("");

               Put_Line("Print Nodes for every Packet");
               for i in 0..k-1 loop
                  Put_Line("Packet"&Natural'Image(i)&":");
                  Put("  Nodes: ");
                  for j in 0 .. packetsArray(i).Last_Index loop
                     Put(Natural'Image(packetsArray(i).Element(j))&",");
                  end loop;
                  Put_Line("");
               end loop;
            end makeReport;
            exit;
         end select;
      end loop;
   end;
   
end graph;
