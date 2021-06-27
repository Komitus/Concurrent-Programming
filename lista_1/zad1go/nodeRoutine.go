package main

import (
	"math/rand"
	"time"
)

//func for every node excluding last
func launchNode(node *Node, printMessage chan<- idsForPrinter) {

	s := rand.NewSource(time.Now().UnixNano() + int64(node.id))
	r := rand.New(s)

	randUpperBound := len(node.nextChannels)
	var randNode int
	for {

		recPacket := <-node.recChannel
		printMessage <- idsForPrinter{packetId: recPacket.packetId, nodeId: node.id}

		node.packetsReceived = append(node.packetsReceived, &recPacket)
		recPacket.visitedNodes = append(recPacket.visitedNodes, node)

		time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
		randNode = r.Intn(randUpperBound)
		*node.nextChannels[randNode] <- recPacket
	}

}
