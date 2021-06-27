package main

import (
	"fmt"
	"math/rand"
	"time"
)

type CheckAndSetCell struct {
	packet Packet
	retVal chan bool
}
type CheckAndSetFalse struct {
	retVal chan []PacketData
}

func node_routine(node_data *Node, printer chan<- string) {

	cellChannel := make(chan CheckAndSetCell)
	falseChannel := make(chan CheckAndSetFalse)

	//statefull goroutine
	go func() {
		rt_size := uint16(len(node_data.routing_table))
		packetOffer := make([]PacketData, 0)
		for {
			select {
			case packetToCheck := <-cellChannel:
				for j := 0; j < len(packetToCheck.packet.offer); j++ {
					newCost := packetToCheck.packet.offer[j].cost + 1
					if newCost < node_data.routing_table[packetToCheck.packet.offer[j].node_id].cost {
						node_data.routing_table[packetToCheck.packet.offer[j].node_id].nextHop = packetToCheck.packet.came_from
						node_data.routing_table[packetToCheck.packet.offer[j].node_id].cost = newCost
						node_data.routing_table[packetToCheck.packet.offer[j].node_id].changed = true
						printer <- fmt.Sprintf("RT in %d node at %d : nextHop = %d, cost = %d, changed = true",
							node_data.id, packetToCheck.packet.offer[j].node_id, newCost, packetToCheck.packet.came_from)
					}

				}
				packetToCheck.retVal <- true
			case cellToFalse := <-falseChannel:
				packetOffer = nil
				for j := uint16(0); j < rt_size; j++ {
					if node_data.routing_table[j].changed == true && node_data.id != j {
						packetOffer = append(packetOffer, PacketData{uint16(j), node_data.routing_table[j].cost})
						node_data.routing_table[j].changed = false
						printer <- fmt.Sprintf("RT in %d node at %d : changed = false", node_data.id, j)
					}
				}
				cellToFalse.retVal <- packetOffer
			}
		}
	}()

	//receiver
	go func() {
		node_id := node_data.id
		for {
			packet := <-node_data.rec_channel
			printer <- fmt.Sprint("Node ", node_id, " received ", packet.offer, " from ", packet.came_from)
			packetToCheck := CheckAndSetCell{packet: packet, retVal: make(chan bool)}
			cellChannel <- packetToCheck
			<-packetToCheck.retVal
		}
	}()

	//sender
	go func() {
		node_id := node_data.id
		s := rand.NewSource(time.Now().UnixNano() + int64(node_id))
		r := rand.New(s)

		for {
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			request := CheckAndSetFalse{make(chan []PacketData)}
			falseChannel <- request
			answer := <-request.retVal

			if len(answer) > 0 {
				packetToSend := Packet{came_from: node_id, offer: answer}
				for neighbour_id, channel := range node_data.neighbours {
					printer <- fmt.Sprintf("Node %d is sending %d to %d", node_id, answer, neighbour_id)
					channel <- packetToSend
				}
			}
		}
	}()

}
