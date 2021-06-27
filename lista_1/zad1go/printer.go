package main

import "fmt"

type idsForPrinter struct {
	packetId int
	nodeId   int
}

func printer(printJumps <-chan idsForPrinter, packetReceived <-chan *Packet, makeReport <-chan []Packet, printerDone chan<- bool, graph []Node) {

	for {
		select {
		case ids := <-printJumps:
			fmt.Println("Packet", ids.packetId, "in", ids.nodeId, "node")
		case packetReceived := <-packetReceived:
			fmt.Println("\u001B[38;5;51mPacket", packetReceived.packetId, "received\u001b[0m")
		case packets := <-makeReport:
			fmt.Println("\n\u001B[38;5;146m########################################")
			fmt.Println("######## Report of simulation  #########")
			fmt.Println("########################################\u001b[0m")
			fmt.Println()
			fmt.Println("\u001B[38;5;159mPackets for nodes: \u001b[0m")

			c := 0
			for _, node := range graph {
				fmt.Println("Node:", c)
				fmt.Print("  Packets: ")
				for _, val := range node.packetsReceived {
					fmt.Print(val.packetId, ", ")
				}
				fmt.Println()
				c++
			}
			fmt.Println()
			fmt.Println("\u001B[38;5;137mNodes for packets: \u001b[0m")
			for i := 0; i < len(packets); i++ {
				fmt.Println("Packet:", i)
				fmt.Print("  Nodes: ")
				for j := 0; j < len(packets[i].visitedNodes); j++ {
					fmt.Print(packets[i].visitedNodes[j].id, ", ")
				}
				fmt.Println()
			}
			printerDone <- true
		}
	}

}
