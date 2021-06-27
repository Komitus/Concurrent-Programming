package main

import (
	"math/rand"
	"time"
)

//func for every node excluding last
func launchNode(node *Node, printJump chan<- idsForPrinter, packetDied chan<- Packet, packetTrapped chan<- Packet) {

	s := rand.NewSource(time.Now().UnixNano() + int64(node.id))
	r := rand.New(s)
	var randNode int
	var trapped bool
	randUpperBound := len(node.nextChannels)

	for {
		select {
		case recPacket := <-node.recChannel:

			node.packetsReceived = append(node.packetsReceived, &recPacket)
			recPacket.visitedNodes = append(recPacket.visitedNodes, node)

			if !trapped {

				if recPacket.ttl > 0 {
					printJump <- idsForPrinter{packetId: recPacket.packetId, nodeId: node.id}
					recPacket.ttl -= 1
					time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
					randNode = r.Intn(randUpperBound)
					*node.nextChannels[randNode] <- recPacket

				} else {
					packetDied <- recPacket
				}
			} else {
				trapped = false
				packetTrapped <- recPacket
			}

		case trapped = <-node.trapped:
		}
	}
}
