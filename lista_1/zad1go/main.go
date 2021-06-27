//Generating graphs and passing parameters

package main

import (
	"os"
	"strconv"
)

func main() {

	var n, d, k int

	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])
	k, _ = strconv.Atoi(os.Args[3])

	//printJumps := make(chan idsForPrinter)
	//printerDone := make(chan bool)
	makeReport := make(chan []Packet)
	printJumps := make(chan idsForPrinter)
	printerDone := make(chan bool)
	packetReceived := make(chan *Packet)
	//i avoid here global variable
	graph := GenerateGraph(n, d)

	for i := 0; i < n; i++ {
		go launchNode(&graph[i], printJumps)
	}
	go printer(printJumps, packetReceived, makeReport, printerDone, graph)
	go receiver(*graph[n-1].nextChannels[0], packetReceived, makeReport, k)
	go sender(graph[0].recChannel, k)
	//if printer routine ends generating report, sends true to channel and we know we can exit program
	<-printerDone

}
