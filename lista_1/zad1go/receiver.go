package main

import (
	"math/rand"
	"time"
)

func receiver(receiverChannel <-chan Packet, packetReceived chan<- *Packet, makeReport chan<- []Packet, k int) {
	packetsCounter := 0
	var s rand.Source = rand.NewSource(time.Now().UnixNano() + int64(k))
	r := rand.New(s)
	packets := make([]Packet, 0)
	for packetsCounter < k {
		packet := <-receiverChannel
		packetReceived <- &packet
		packets = append(packets, packet)
		packetsCounter++
		time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
	}
	makeReport <- packets
}
