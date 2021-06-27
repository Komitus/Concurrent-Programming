//Generating graphs and passing parameters

package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {

	var n, d, b, k, h, numOfTraps int

	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])
	b, _ = strconv.Atoi(os.Args[3])
	k, _ = strconv.Atoi(os.Args[4])
	h, _ = strconv.Atoi(os.Args[5])
	numOfTraps, _ = strconv.Atoi(os.Args[6])

	if d > (n-3)*n/2+1 || b > (n-1)*n/2 {
		fmt.Println("Too big amount of shortcuts, it is impossible to make this")
		return
	}

	//I have many channels because i wanted to avoid global arrays of nodes and packets
	//and then printed messages are sequenced bcs i print everything by printer
	makeReport := make(chan []Packet)                //channel for receiver and printer to pass received/dead/tapped packets
	printJump := make(chan idsForPrinter)            //channel for nodes and printer to priint jumps
	printerDone := make(chan bool)                   //this channel signilize that program ended (we received final reports)
	packetReceivedPrinter := make(chan int)          //channel to pass received packets' ids by receiver to printer
	packetDied := make(chan Packet)                  //channel for receiver and nodes to received dead packet
	packetDiedPrinter := make(chan idsForPrinter)    //channel for printer and receiver to print that packet died
	packetTrapped := make(chan Packet)               //channel for receiver and nodes to received trapped packet
	packetTrappedPrinter := make(chan idsForPrinter) //channel for printer and receiver to print that packet was trapped
	trapMadePrinter := make(chan int)                //channel for hunter and printer to print that trap was made
	graph := GenerateGraph(n, d, b)

	go printer(printJump, packetReceivedPrinter,
		packetDiedPrinter,
		trapMadePrinter, packetTrappedPrinter,
		makeReport, printerDone, graph)
	go receiver(
		*graph[n-1].nextChannels[0], packetReceivedPrinter,
		packetDied, packetDiedPrinter,
		packetTrapped, packetTrappedPrinter,
		makeReport, k)
	if numOfTraps > 0 {
		go hunter(graph, n, numOfTraps, trapMadePrinter)
	}
	for i := 0; i < n; i++ {
		go launchNode(&graph[i], printJump, packetDied, packetTrapped)
	}
	go sender(graph[0].recChannel, k, h)
	//if printer routine ends generating report, sends true to channel and we know we can exit program
	<-printerDone

}
