with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;
with Ada.Numerics.Float_Random;
package body graph is



   task body NodeTask is
      subtype Random_Range_Index is Natural range 0 .. NodeTask.sc.Last_Index;
      package RRI is new Ada.Numerics.Discrete_Random (Random_Range_Index);
      GRI : RRI.Generator;
      nextTask : Natural;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      localPacket : Packet;
   begin
      RRI.Reset (GRI);
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      loop
         select
            accept handlePacket (myPacket : Packet) do
               localPacket := myPacket;
            end handlePacket;
            myPrinter.printJump(id, localPacket.id);
            delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
            if NodeTask.id = graph.n-1 then
               myReceiver.receive(receivedPacket => localPacket);
            else
               nextTask := NodeTask.sc.Element(RRI.Random(GRI));
                  arrayOfTasks.all(nextTask).handlePacket(myPacket => localPacket);
            end if;
         or
            terminate;
         end select;
      end loop;
   end;




   procedure initializeGraph(n: Natural; d: Natural) is
      procedure GenerateEdges  is
         subtype Random_Range is Integer range 0 .. n-1;
         package R is new Ada.Numerics.Discrete_Random (Random_Range);
         G : R.Generator;
         src : Random_Range;
         dest : Random_Range;
         repeated : Boolean;
         c: Natural;
      begin

         allEdges := new EveryEdge(0..n-1);
         for i in 0..n-2 loop
           allEdges(i) := new SuccessorsOfNode.Vector;
            allEdges(i).all.Append(i+1);
         end loop;
         allEdges(n-1) := new SuccessorsOfNode.Vector;

         c := 0;
         while c < d loop
            repeated := false;
            src := R.Random(G);
            dest := R.Random(G);
            if src+1 < dest then

               for j in 0..allEdges(src).Last_Index loop
                  if allEdges(src).Element(j) = dest then
                     repeated := true;
                  end if;
               end loop;

               if repeated = false then
               allEdges(src).all.Append(dest);
               c:=c+1;
               end if;

            end if;
         end loop;

         for I in 0..n-1 loop
            Put_Line("Node "&Integer'Image(I)&": ");
            Put("     -->:");
            for J in 0..allEdges(I).Last_Index loop

               Put(Integer'Image(allEdges(I).Element(J))&",");
               null;
            end loop;
            Put_Line("");
          end loop;
      end;

   begin
      graph.n := n;
      GenerateEdges;
      arrayOfTasks := new TasksArray(0..n-1);
   end;

   task body sender is
      packetToSend : Packet;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      tmpK : Natural;
   begin
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      accept Start(k : Natural) do
            tmpK := k-1;
      end Start;
      for i in 0..tmpK loop
         delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
         packetToSend := Packet'(id => i);
         arrayOfTasks.all(0).handlePacket(myPacket => packetToSend);
      end loop;
   end;

   task body receiver is
      packetCounter : Natural;
      tmpK : Natural;
      DelayGenerator  : Ada.Numerics.Float_Random.Generator;
      packetId : Natural;
   begin
      accept Start (k : in Natural) do
         Ada.Numerics.Float_Random.Reset(DelayGenerator);
         tmpK := k;
      end Start;
      packetCounter := 0;
      while packetCounter < tmpK loop  
          accept receive(receivedPacket : Packet) do
             packetId := receivedPacket.id;
          end receive;
          myPrinter.packetReceived(packetId);
          packetCounter:=packetCounter+1;
          delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
      end loop;
      myPrinter.makeReport;
   end;

   task body printer is
      tmpK : Natural;
      tmpN: Natural;
      packetsArray : NodesForPacketPtr;
      nodesArray : PacketsForNodePtr;
   begin
      accept Start (k : in Natural; n : in Natural) do
         tmpK := k-1;
         tmpN := n-1;
      end Start;
          packetsArray := new NodesForPacket(0..tmpK);
          nodesArray := new PacketsForNode(0..tmpN);


          for i in 0..tmpK loop
            packetsArray(i) := new VectorForPrinter.Vector;
          end loop;
          for i in 0..tmpN loop
            nodesArray(i) := new VectorForPrinter.Vector;
          end loop;

      loop
         select
            accept printJump (nodeId : in Natural; packetId : in Natural) do
               Put_Line("Packet"&Natural'Image(packetId)&" in Node "&Natural'Image(nodeId));
               packetsArray(packetId).all.Append(nodeId);
               nodesArray(nodeId).all.Append(packetId);
            end printJump;
         or
            accept packetReceived (packetId : in Natural) do
               Put_Line(Ada.Characters.Latin_1.ESC &"[33m"&"Packet"&Natural'Image(packetId)&" received "&Ada.Characters.Latin_1.ESC&"[0m");
            end packetReceived;
         or
            accept makeReport  do
               Put_Line("");
               Put_Line("########################################");
               Put_Line("######## Report of simulation  #########");
               Put_Line("########################################");

               Put_Line("Print packets for every Node");
               for i in 0..tmpN loop
                  Put_Line("Node"&Natural'Image(i)&":");
                  Put("  Packets: ");
                  for j in 0..nodesArray(i).Last_Index loop
                     Put(Natural'Image(nodesArray(i).Element(j))&",");
                  end loop;
                  Put_Line("");
               end loop;
               Put_Line("");

               Put_Line("Print Nodes for every Packet");
               for i in 0..tmpK loop
                  Put_Line("Packet"&Natural'Image(i)&":");
                  Put("  Nodes: ");
                  for j in 0..packetsArray(i).Last_Index loop
                     Put(Natural'Image(packetsArray(i).Element(j))&",");
                  end loop;
                  Put_Line("");
               end loop;
               return;
            end makeReport;
         or 
            terminate;
            
         end select;
      end loop;
   end;





end graph;
