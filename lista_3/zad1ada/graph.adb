with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random; 
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Real_Time; 
package body graph is
   
   allEdges : EveryEdgePtr;
   arrayOfTasks : TasksArrayPtr;
   arrayOfRTs : EveryRoutingTablePtr;
   printer : PrinterTaskPtr;
   procedure startSimulation (n, d : in Natural) is
   begin
      
      initializeGraph(n, d);
      arrayOfTasks := new TasksArray(0..n-1);
      arrayOfRTs := new EveryRoutingTable(0..n-1);
      printer := new PrinterTask(n);
      for i in 0..n-1 loop
         arrayOfRTs(i) := new RoutingTable(0..n-1);
         arrayOfTasks(i) := new NodeTask(i, n, allEdges(i), arrayOfRTs(i));
      end loop;
     
   end startSimulation;
   
   procedure initializeGraph(n, d : Natural) is
      
      type randRange is new Natural range 0..n-1;
      package Rand_Natural is new ada.numerics.discrete_random(randRange);
      gen : Rand_Natural.Generator;
      src : Natural;
      dest : Natural;
      repeated : Boolean;
      c: Natural;
   begin
      
      Rand_Natural.Reset(gen);
      allEdges := new EveryEdge(0..n-1);
      
      allEdges(0) := new SuccessorsOfNode.Vector;
      for i in 1..n-1 loop
         allEdges(i) := new SuccessorsOfNode.Vector;
         allEdges(i).all.Append(i-1);
         allEdges(i-1).all.Append(i);
      end loop;
      allEdges(n-1) := new SuccessorsOfNode.Vector;
      allEdges(n-1).all.Append(n-2);
         
      c := 0;
      while c < d loop
            
         repeated := false;
         src := Natural(Rand_Natural.Random(gen));
         dest := Natural(Rand_Natural.Random(gen));
          
         if src = dest then
            goto Continue;
         end if;
         for j in 0..allEdges(src).Last_Index loop
            if allEdges(src).Element(j) = dest then
               repeated := true;
               exit;
            end if;
         end loop;

         if repeated = false then
            allEdges(src).all.Append(dest);
            allEdges(dest).all.Append(src);
            c:=c+1;
         end if;
         <<Continue>>
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
   
   
   task body NodeTask is
      
      protected ProtectedRT is
         procedure initialize;
         function CheckAndSetCell(cameFrom : Natural; packetCell : PacketData) return Boolean;
         function CheckAndSetFalseOnCell(j : Natural; cost : out Natural) return Boolean;
         private
           rt: RoutingTablePtr;
      end ProtectedRT;
      
      protected body ProtectedRT is
         
         function CheckAndSetCell(cameFrom : Natural; packetCell : PacketData) return Boolean is
            newCost : Natural;
         begin
            newCost := packetCell.cost + 1;
            if newCost < rt.all(packetCell.node_id).cost then
               rt.all(packetCell.node_id).nextHop := cameFrom;
               rt.all(packetCell.node_id).cost := newCost;
               rt.all(packetCell.node_id).changed := True;
               return True;
            else 
               return False;
            end if;
         end CheckAndSetCell;
         function CheckAndSetFalseOnCell(j : Natural; cost : out Natural) return Boolean is
         begin
            if rt.all(j).changed = True then
               rt.all(j).changed := False;
               cost := rt.all(j).cost;
               return True;
            else 
               return False;
            end if;
         end CheckAndSetFalseOnCell;
         procedure initialize is
         begin
            rt := passedRT;
            for j in 0..n-1 loop
               if j = id then
                  rt.all(j).nextHop := j;
                  rt.all(j).cost := 0;
                  rt.all(j).changed := false;
               elsif sc.all.Contains(j) then
                  rt.all(j).nextHop := j;
                  rt.all(j).cost := 1;
                  rt.all(j).changed := true;
               else
                  rt(j).cost := abs (id-j);
                  if id < j then
                     rt.all(j).nextHop := id + 1;
                  else 
                     rt.all(j).nextHop := id - 1;
                  end if;
                  rt.all(j).changed := true;
               end if;
            
            end loop;
         end;
   end ProtectedRT;
    
      task type Receiver is
         entry start;
         entry handlePacket (receivedPacket : Packet);
      end Receiver;
      
      task body Receiver is
         localReceivedPacket : Packet;
         package Str renames Ada.Strings.Unbounded;
         
         procedure printPacket is
            retString : Str.Unbounded_String := Str.Null_Unbounded_String;
         begin
            Str.Append(Source   => retString,
                       New_Item => "Node " & Natural'Image(id) & " recived [");
            for i in 0..localReceivedPacket.offer.Last_Index loop
                       Str.Append(Source   => retString,
                                  New_Item => 
                                    "{" & Ada.Strings.Fixed.Trim(Natural'Image(localReceivedPacket.offer.Element(i).node_id), Ada.Strings.Left) & 
                                    Natural'Image(localReceivedPacket.offer.Element(i).cost) & "} ");
            end loop;
            Str.Replace_Element(Source => retString,
                                Index  => Str.Length(Source => retString),
                                By     => ']');
            Str.Append(Source   => retString,
                       New_Item => " from node " & Natural'Image(localReceivedPacket.cameFrom));
            printer.all.print(message => Str.To_String(retString));
         end;          
      begin
         accept start;
         loop 
            select
               accept handlePacket (receivedPacket : in Packet) do
                  localReceivedPacket := receivedPacket;
               end handlePacket;
               printPacket;
               for j in 0..localReceivedPacket.offer.Last_Index loop
                  if ProtectedRT.CheckAndSetCell(cameFrom   => localReceivedPacket.cameFrom,
                                                 packetCell => localReceivedPacket.offer.Element(j)) then
                     printer.all.print("RT in " & Natural'Image(id) & " node at " & Natural'Image(localReceivedPacket.offer.Element(j).node_id) &
                                     ":  nextHop = " & Natural'Image(localReceivedPacket.cameFrom) & ", cost = " 
                                   & Natural'Image(localReceivedPacket.offer.Element(j).cost+1) & ", changed = true");
                  end if;
                  
               end loop;
               --Put_Line("RECEIVER ALIVE");
            or 
               terminate;
               
            end select;
         end loop;
      end Receiver;
                           
       task type Sender is
         entry start;
         entry stop;
      end Sender;
      
      task body Sender is
         DelayGenerator  : Ada.Numerics.Float_Random.Generator;
         cost : Natural;
         packetToSend : Packet;
         offerToSend : OfferVector.Vector;
         package Str renames Ada.Strings.Unbounded;
         packetTemplate : Str.Unbounded_String;
         
         function printPacket return Str.Unbounded_String is
            retString : Str.Unbounded_String := Str.Null_Unbounded_String;
         begin
            Str.Append(Source   => retString,
                       New_Item => "Node" & Natural'Image(id) & " is sending [");
            for i in 0..offerToSend.Last_Index loop
                       Str.Append(Source   => retString,
                                  New_Item => 
                                    "{" & Ada.Strings.Fixed.Trim(Natural'Image(offerToSend.Element(i).node_id), Ada.Strings.Left) & 
                                    Natural'Image(offerToSend.Element(i).cost) & "} ");
            end loop;
            Str.Replace_Element(Source => retString,
                                Index  => Str.Length(Source => retString),
                                By     => ']');
            Str.Append(Source   => retString,
                       New_Item => " to node ");
            
                       return retString;
         end;
         
      begin
         Ada.Numerics.Float_Random.Reset(DelayGenerator);
         accept start;
         loop 
            select
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
               offerToSend.Clear;
            
               for j in 0..n-1 loop
                  if id /= j then
                     if ProtectedRT.CheckAndSetFalseOnCell(j      => j,
                                                           cost   => cost) = True then
                        printer.all.print("RT in " & Natural'Image(id) & " node at " & Natural'Image(j) &
                                        " : changed = false");
                        offerToSend.Append(PacketData'(node_id => j, cost => cost));
                     end if;
                  end if;
               end loop;
               if not offerToSend.Is_Empty then
                  packetToSend := Packet'(cameFrom => id, offer => offerToSend);
                  packetTemplate := printPacket;
                  for i in 0..sc.Last_Index loop
                     printer.all.print(message => Str.To_String(packetTemplate) & Natural'Image(sc.Element(i)));
                     arrayOfTasks.all(sc.Element(i)).handlePacket(receivedPacket => packetToSend);
                  end loop;
               end if;
               --Put_Line("Sender ALIVE");
            or 
               accept stop;
               --Put_Line("Sender ended");
               exit;
            end select;
            
         end loop;
        
      end Sender;
      
      myReceiver : Receiver;                    
      mySender : Sender;    
   begin
      ProtectedRT.initialize; 
      myReceiver.start;
      mySender.start;
      loop
         select 
            accept handlePacket (receivedPacket : in Packet) do
               myReceiver.handlePacket(receivedPacket => receivedPacket);
            end handlePacket;
         or
            accept stop;
                --Put_Line("Requested sender end");
                mySender.stop;
                exit;
         end select;
      end loop;
      
   end;
   
   task body PrinterTask is
      package Str renames Ada.Strings.Unbounded;
      cellLength : Natural;
      tmpString : Str.Unbounded_String;
      procedure printReport is
      begin
         Str.Append(tmpString, "{"& Natural'Image(arrayOfRTs.all(0).all(1).nextHop) &
                    Natural'Image(arrayOfRTs.all(0).all(1).cost) & " " &
                                         Boolean'Image(arrayOfRTs.all(0).all(1).changed) & " }");
         cellLength := Ada.Strings.Unbounded.Length(tmpString); 
         Put_Line("");
         Put_Line("########################################################");
         Put_Line("################ FINAL REPORT ##########################");
         Put_Line("########################################################");
         Put_Line("");
         for a in 0..n-1 loop
            Put("Node " & Natural'Image(a) & ": " & "[");
            for b in 0..n-1 loop
               if a /= b then
                Put(" {"& Natural'Image(arrayOfRTs.all(a).all(b).nextHop) &
                    Natural'Image(arrayOfRTs.all(a).all(b).cost) & " " &
                      Boolean'Image(arrayOfRTs.all(a).all(b).changed) & " } ");
               else 
                  Put(" ");
                  for i in 0..cellLength-1 loop
                     Put("#");
                  end loop; 
                  Put(" ");
               end if;
            end loop;
            Put_Line("]");
         end loop;
         
      end;
      lasTime : Ada.Real_Time.Time;
   begin
      lasTime := Ada.Real_Time.Clock;
      loop
         select 
          
            accept print (message : in String) do
               Put_Line(message);
               lasTime := Ada.Real_Time.Clock;
            end print;
         or 
            delay Standard.Duration(0.5);
            if Ada.Real_Time.">"(Left  => Ada.Real_Time."-"(Left  => Ada.Real_Time.Clock,
                                                            Right => lasTime),
                                Right => Ada.Real_Time.Seconds(2)) then
              for i in 0..n-1 loop
                 arrayOfTasks.all(i).stop;
              end loop;  
              printReport;
              exit;
            end if;
         end select;
      end loop;
   end PrinterTask;
   
   
end graph;
