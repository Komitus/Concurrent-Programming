package main

import (
	"math/rand"
	"time"
)

func receiver(
	receiverChannel <-chan Packet, packetReceivedPrinter chan<- int,
	deadPacket <-chan Packet, deadToPrint chan<- idsForPrinter,
	trappedPacket <-chan Packet, trappedToPrint chan<- idsForPrinter,
	makeReport chan<- []Packet, k int) {

	packetsCounter := 0
	var s rand.Source = rand.NewSource(time.Now().UnixNano() + int64(k))
	r := rand.New(s)
	packets := make([]Packet, 0)
	for packetsCounter < k {
		select {
		case packet := <-receiverChannel:
			packetReceivedPrinter <- packet.packetId
			packets = append(packets, packet)
			packetsCounter++
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
		case packet := <-deadPacket:
			lastNode := len(packet.visitedNodes) - 1
			toPrinter := idsForPrinter{packetId: packet.packetId, nodeId: packet.visitedNodes[lastNode].id}
			deadToPrint <- toPrinter
			packets = append(packets, packet)
			packetsCounter++
		case packet := <-trappedPacket:
			lastNode := len(packet.visitedNodes) - 1
			toPrinter := idsForPrinter{packetId: packet.packetId, nodeId: packet.visitedNodes[lastNode].id}
			trappedToPrint <- toPrinter
			packets = append(packets, packet)
			packetsCounter++
		}

	}
	makeReport <- packets
}
