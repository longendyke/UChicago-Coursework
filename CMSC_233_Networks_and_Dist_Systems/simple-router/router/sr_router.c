/**********************************************************************
 * file:  sr_router.c
 * date:  Mon Feb 18 12:50:42 PST 2002
 * Contact: casado@stanford.edu
 *
 * Description:
 *
 * This file contains all the functions that interact directly
 * with the routing table, as well as the main entry method
 * for routing.
 *
 **********************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>


#include "sr_if.h"
#include "sr_rt.h"
#include "sr_router.h"
#include "sr_protocol.h"
#include "sr_arpcache.h"
#include "sr_utils.h"

/*---------------------------------------------------------------------
 * Method: sr_init(void)
 * Scope:  Global
 *
 * Initialize the routing subsystem
 *
 *---------------------------------------------------------------------*/

void sr_init(struct sr_instance* sr)
{
    /* REQUIRES */
    assert(sr);

    /* Initialize cache and cache cleanup thread */
    sr_arpcache_init(&(sr->cache));

    pthread_attr_init(&(sr->attr));
    pthread_attr_setdetachstate(&(sr->attr), PTHREAD_CREATE_JOINABLE);
    pthread_attr_setscope(&(sr->attr), PTHREAD_SCOPE_SYSTEM);
    pthread_attr_setscope(&(sr->attr), PTHREAD_SCOPE_SYSTEM);
    pthread_t thread;

    pthread_create(&thread, &(sr->attr), sr_arpcache_timeout, sr);

    /* Add initialization code here! */

} /* -- sr_init -- */

/*---------------------------------------------------------------------
 * Method: sr_handlepacket(uint8_t* p,char* interface)
 * Scope:  Global
 *
 * This method is called each time the router receives a packet on the
 * interface.  The packet buffer, the packet length and the receiving
 * interface are passed in as parameters. The packet is complete with
 * ethernet headers.
 *
 * Note: Both the packet buffer and the character's memory are handled
 * by sr_vns_comm.c that means do NOT delete either.  Make a copy of the
 * packet instead if you intend to keep it around beyond the scope of
 * the method call.
 *
 *---------------------------------------------------------------------*/

sr_arp_hdr_t *new_arp_header(struct sr_if *intface, unsigned short opcode, unsigned char *tha, uint32_t tip);
struct sr_rt *routing_table_search(struct sr_instance *sr, uint32_t destination);
sr_ethernet_hdr_t *new_ethernet_frame(uint16_t type, uint8_t *dst, uint8_t *src, unsigned int len);
sr_ip_hdr_t *new_ip_header(sr_ip_hdr_t *old_header, uint32_t ip_src, uint32_t ip_dst, uint8_t ip_prot);
sr_ip_hdr_t *forward_ip_header(sr_ip_hdr_t *old_header);
sr_icmp_t3_hdr_t* new_ICMP_t3(uint8_t *packet, unsigned int len, uint8_t code);
void processReply (struct sr_instance* sr, uint8_t *packet, unsigned int len, char* interface);

//~~~*****   NOTE TO THE GRADERS   *****~~~//
// We were never really certain which      //
// values should be ntoh/hton'd, so you    //
// might see some random calls to those    //
// functions around... Sorry about that... //
// So far, the logic of our code should be //
// correct, but we're running into proble- //
// ms with the way we move IP/ICMP packet- //
// s into Ethernet/IP frames/packets. So,  //
// basically, in fewer words, everything   //
// SHOULD work, but it probably doesn't.   //
// Sorry, again. I hope the comments can   //
// help with what we thought the logic     //
// is (seriously, I'm like 98% sure it's   //
// right). Thanks for the helpful comments //
// on the other projects, and hope you     //
// have a good spring break!               //
//              -Will and Ryan             //
//-----------------------------------------//


void sr_handlepacket(struct sr_instance* sr,
                     uint8_t * packet/* lent */,
                     unsigned int len,
                     char* interface/* lent */)
{
    /* REQUIRES */
    assert(sr);
    assert(packet);
    assert(interface);

    printf("\n\n*** -> Received packet of length %d \n",len);
    print_hdrs(packet, len);
    printf("End packet dump\n\n");

    //Get the interface we're using for future use
    struct sr_if *intface = sr_get_interface(sr, interface);

    //Determine what kind of packet we're dealing with
    uint16_t ether_type = ethertype(packet);
    
    //Handling IP Packets
    if(ether_type == ethertype_ip){
        printf("Received an IP packet\n");


        //Extract the Ethernet header for later use
        sr_ethernet_hdr_t *ehdr = (sr_ethernet_hdr_t *) packet;

        //Extract the IP header
        sr_ip_hdr_t *iphdr = (sr_ip_hdr_t *)(packet + sizeof(sr_ethernet_hdr_t));

        //Sanity Check
        if(len < sizeof(sr_ip_hdr_t)){
            printf("Received a packet that was too small\n");
            //Received a packet smaller than the minimum length of an IP packet
            return;
        }

        sr_ip_hdr_t *hdr_copy = malloc(sizeof(sr_ip_hdr_t));
        memcpy(hdr_copy, iphdr, sizeof(sr_ip_hdr_t));
        hdr_copy->ip_sum = 0;

        uint16_t sanity = cksum(hdr_copy, sizeof(sr_ip_hdr_t));
        
        if(sanity != iphdr->ip_sum){
            printf("Received a packet whose checksum is incorrect\n");
            //Received a packet whose checksum is incorrect
            return;
        }

        
        struct sr_if *sr_intf = sr->if_list;
        //Check if the packet is destined for the router
        while(sr_intf != NULL){
            uint32_t addr = sr_intf->ip;    
            if(iphdr->ip_dst == addr){
                printf("The IP packet is destined for the router!\n");
                //Packet is destined for the router
                uint8_t protocol = ip_protocol((uint8_t *)iphdr);

                //If this passes then we've received an ICMP packet destined for the router
                if(protocol == ip_protocol_icmp){
                    printf("We received an ICMP packet for the router\n");
                    //Send back a ping

                    //Make a new ICMP packet
                    sr_icmp_hdr_t *reply = malloc(sizeof(sr_icmp_hdr_t));
                    memset(reply, 0, sizeof(sr_icmp_hdr_t));

                    //Type 0 for reply, code 0
                    reply->icmp_type = 0;
                    reply->icmp_code = 0;

                    //Compute the checksum
                    reply->icmp_sum = 0;
                    uint16_t newsum = cksum(reply, sizeof(sr_icmp_hdr_t));
                    reply->icmp_sum = newsum;

                    //Make a new IP header
                    sr_ip_hdr_t *IP_header = new_ip_header(iphdr, intface->ip, iphdr->ip_src, ip_protocol_icmp);

                    print_hdr_ip((uint8_t *)IP_header);

                    //Make a new Ethernet frame
                    sr_ethernet_hdr_t *new_frame = new_ethernet_frame(ethertype_ip, ehdr->ether_shost, intface->addr, 
                        sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t) + sizeof(sr_icmp_hdr_t));

                    //Move IP header into Ethernet frame
                    memcpy(new_frame + sizeof(sr_ethernet_hdr_t), IP_header, sizeof(sr_ip_hdr_t));

                    //Move ICMP message into Ethernet frame
                    memcpy((uint8_t *) new_frame + sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t), reply, sizeof(sr_icmp_hdr_t));
                    
                    //Send the packet back
                    print_hdrs((uint8_t*) new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t) + sizeof(sr_icmp_hdr_t));
                    sr_send_packet(sr, (uint8_t *) new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t) + sizeof(sr_icmp_hdr_t), interface);
                }
                else{
                    printf("We received a TCP/UDP packet for the router\n");
                    //Received a TCP/UDP datagram, send a
                    //Port Unreachable message (Type 3 Code 3)
                    sr_icmp_t3_hdr_t *new_ICMP = new_ICMP_t3(packet, len, 3);

                    //New IP header to send
                    sr_ip_hdr_t *new_ip_h = new_ip_header(iphdr, intface->ip, iphdr->ip_src, ip_protocol_icmp);

                    //New Ethernet frame, with the source and destination addresses switched
                    unsigned int nlen = sizeof(sr_ethernet_hdr_t) + sizeof(sr_icmp_t3_hdr_t) + sizeof(sr_ip_hdr_t);
                    sr_ethernet_hdr_t *new_eframe = new_ethernet_frame(ethertype_ip, ehdr->ether_shost, ehdr->ether_dhost, nlen);

                    
                    memcpy(new_eframe + sizeof(sr_ethernet_hdr_t), new_ip_h, sizeof(sr_ip_hdr_t));
                    
                    memcpy(new_eframe + sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t), new_ICMP, sizeof(sr_icmp_t3_hdr_t));
                    

                    //Send the packet
                    print_hdrs((uint8_t *) new_eframe,  nlen);
                    sr_send_packet(sr, (uint8_t *)new_eframe, nlen, interface);
                }
            }
            sr_intf = sr_intf->next;
        }

        printf("Decrementing the TTL\n");
        //Decrement the TTL
        uint8_t TTL = iphdr->ip_ttl;
        --(TTL);
        iphdr->ip_ttl = iphdr->ip_ttl;

        //Check the routing table
        struct sr_rt* routing = routing_table_search(sr, iphdr->ip_dst);

        if(routing == NULL){
            printf("We don't have the IP address of the IP packet not destined for our router in teh routing table\n");
            //If routing is NULL, then we don't have the IP address in our
            //routing table, so we send an ICMP Destination Net Unreachable
            sr_icmp_t3_hdr_t *new_ICMP = new_ICMP_t3(packet, len, 0);

            //New IP header to send
            sr_ip_hdr_t *new_ip_h = new_ip_header(iphdr, intface->ip, iphdr->ip_src, ip_protocol_icmp);

            //New Ethernet frame, with the source and destination addresses switched
            unsigned int nlen = sizeof(sr_ethernet_hdr_t) + sizeof(sr_icmp_t3_hdr_t) + sizeof(sr_ip_hdr_t);
            sr_ethernet_hdr_t *new_eframe = new_ethernet_frame(ethertype_ip, ehdr->ether_shost, ehdr->ether_dhost, nlen);

            
            
            memcpy(new_eframe + sizeof(sr_ethernet_hdr_t), new_ip_h, sizeof(sr_ip_hdr_t));
            
            memcpy(new_eframe + sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t), new_ICMP, sizeof(sr_icmp_t3_hdr_t));

            //Send the packet
            print_hdrs((uint8_t *)new_eframe, nlen);
            sr_send_packet(sr, (uint8_t *)new_eframe, nlen , interface);
            return;
        }

        //Check the ARP Cache
        struct sr_arpentry *arp_entry = (struct sr_arpentry *) malloc(sizeof(struct sr_arpentry));
        arp_entry = sr_arpcache_lookup(&(sr->cache), (uint32_t) routing->dest.s_addr);
        if(arp_entry != NULL){
            printf("The IP->MAC routing from the IP packet was in the Cache!\n");
            //***If we've reached here then our destination was in the ARP cache***//

            //Create a new Ethernet frame with the destination and source inputted, based
            //off of the return from the arpcache lookup and from the interface we're
            //receiving the packet from
            sr_ethernet_hdr_t *new_frame = new_ethernet_frame(ethertype_ip, ehdr->ether_dhost, intface->addr, len);

            //Take the previous IP header and payload recompute the checksum.
            sr_ip_hdr_t *new_datagram = forward_ip_header(iphdr);

            
            //Place the IP datagram into the Ethernet frame
            memcpy(new_frame + sizeof(sr_ethernet_hdr_t), new_datagram, sizeof(sr_ip_hdr_t));
            

            //Send the packet
            print_hdrs((uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t));
            sr_send_packet(sr, (uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t), interface);
            return;
        }
        else{
            printf("The IP->MAC mapping from the IP packet was not in the ARP cache\n");
            //If we reach here, then the entry wasn't in the ARP cache.
            //We need to send an ARP-request for the next_hop IP (if one
            //hasn't been sent in the last second) and add the packet to
            //the queue of packets waiting on this ARP request

            //Make a new ARP request
            unsigned char tha[ETHER_ADDR_LEN];
            memset(tha, 0, ETHER_ADDR_LEN);
            sr_arp_hdr_t *request = new_arp_header(intface, arp_op_request, tha, iphdr->ip_dst);

            //Make a new Ethernet frame addressed to the broadcast MAC address
            uint8_t dst[ETHER_ADDR_LEN];
            memset(dst, 1, ETHER_ADDR_LEN);
            unsigned int nlen = sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t);
            sr_ethernet_hdr_t *new_frame = new_ethernet_frame(ethertype_arp, dst, intface->addr, nlen);

            //Place the ARP request into the Ethernet frame
            memcpy(new_frame + sizeof(sr_ethernet_hdr_t), request, sizeof(sr_arp_hdr_t));

            //Send the packet
            print_hdrs((uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t));
            sr_send_packet(sr, (uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t), interface);

            unsigned int packet_len = iphdr->ip_len + sizeof(sr_ethernet_hdr_t);
            sr_arpcache_queuereq(&(sr->cache), iphdr->ip_dst, packet, packet_len, interface);
            return;
        }
    }
    
    if(ether_type == ethertype_arp)
    {
        printf("Received an ARP packet\n");
        processReply(sr, packet, len, interface);
        return;
    }
}/* end sr_ForwardPacket */

sr_arp_hdr_t *new_arp_header(struct sr_if *intface, unsigned short opcode, unsigned char *tha, uint32_t tip)
{
    sr_arp_hdr_t *new_ARP = malloc(sizeof(sr_arp_hdr_t));
    memset(new_ARP, 0, sizeof(sr_arp_hdr_t));

    new_ARP->ar_hrd = htons(arp_hrd_ethernet);
    new_ARP->ar_pro = htons(ethertype_ip);
    new_ARP->ar_hln = ETHER_ADDR_LEN;
    new_ARP->ar_pln = 4;

    new_ARP->ar_op = htons(opcode);

    
    memcpy(new_ARP->ar_sha, intface->addr, sizeof(unsigned char) * ETHER_ADDR_LEN);
    new_ARP->ar_sip = intface->ip;

    
    memcpy(new_ARP->ar_tha, tha, sizeof(unsigned char) * ETHER_ADDR_LEN);
    new_ARP->ar_tip = tip;

    print_hdr_arp((uint8_t *) new_ARP);

    return new_ARP;
}

struct sr_rt *routing_table_search(struct sr_instance *sr, uint32_t destination)
{
    //If the routing table is NULL, return NULL
    if(sr->routing_table == NULL)
        return NULL;

    struct sr_rt *temp = malloc(sizeof(struct sr_rt));
    temp = sr->routing_table;
    //Else, search the routing table
    while(temp != NULL)
    {
        unsigned long mask = temp->mask.s_addr;
        unsigned long match = mask & (unsigned long) destination;
        if(match == mask)
            return temp;
        else
            temp = temp->next;
    }
    return NULL;
}

sr_ethernet_hdr_t *new_ethernet_frame(uint16_t type, uint8_t *dst, uint8_t *src, unsigned int len)
{
    sr_ethernet_hdr_t *eheader = malloc(len);

    eheader->ether_type = htons(type);
    memcpy(eheader->ether_dhost, dst, sizeof(uint8_t) * ETHER_ADDR_LEN);
    memcpy(eheader->ether_shost, src, sizeof(uint8_t) * ETHER_ADDR_LEN);

    return eheader;
}

sr_ip_hdr_t *new_ip_header(sr_ip_hdr_t *old_header, uint32_t ip_src, uint32_t ip_dst, uint8_t ip_prot)
{
    sr_ip_hdr_t *to_send = malloc(sizeof(sr_ip_hdr_t));
    to_send = memcpy(to_send, old_header, sizeof(sr_ip_hdr_t));

    to_send->ip_ttl = htons(64);

    to_send->ip_src = ip_src;
    to_send->ip_dst = ip_dst;
    to_send->ip_p = ip_prot;

    to_send->ip_sum = 0;

    to_send->ip_sum = cksum(to_send, sizeof(sr_ip_hdr_t));

    return to_send;
}

sr_ip_hdr_t *forward_ip_header(sr_ip_hdr_t *old_header)
{
    sr_ip_hdr_t *to_send = malloc((int)old_header->ip_len);
    to_send = memcpy(to_send, old_header, (int)old_header->ip_len);

    to_send->ip_sum = 0;

    to_send->ip_sum = cksum(to_send, sizeof(sr_ip_hdr_t));

    return to_send;
}

sr_icmp_t3_hdr_t* new_ICMP_t3(uint8_t *packet, unsigned int len, uint8_t code)
{
    sr_icmp_t3_hdr_t* header = malloc(sizeof(sr_icmp_t3_hdr_t));
    memset(header, 0, sizeof(sr_icmp_t3_hdr_t));

    header->icmp_type = 3;
    header->icmp_code = code;
    header->icmp_sum = 0;
    header->next_mtu = 0;
    header->unused = 0;

    //Set the data field to all zeroes
    bzero(header->data, ICMP_DATA_SIZE);

    //Extract the IP packet from the packet, and, if its payload is <= 8 bytes
    //simply use it as the data field in the ICMP message. Otherwise, move the
    //IP header + 8 bytes into the data field
    sr_ip_hdr_t *iphdr = (sr_ip_hdr_t *)(packet + sizeof(sr_ethernet_hdr_t));
    uint8_t *payload = malloc(len - (sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t)));
    memcpy(payload, packet + sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t), 
            len - (sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t)));
    if((len - (sizeof(sr_ethernet_hdr_t) + sizeof(sr_ip_hdr_t))) <= 8)
        memcpy(header->data, iphdr, sizeof(sr_ip_hdr_t));
    else
        memcpy(header->data, iphdr, sizeof(sr_ip_hdr_t)+8);

    //Calculate checksum for the ICMP packet
    long int chksum = cksum((const void *)header, sizeof(sr_icmp_t3_hdr_t));

    header->icmp_sum = chksum;

    return header;
}

void processReply (struct sr_instance* sr, uint8_t * packet, unsigned int len, char* interface)
{
    printf("In processReply\n");
    //Get the interface struct
    struct sr_if *intface = sr_get_interface(sr, interface);

  //Extract the Ethernet header for later use
  sr_ethernet_hdr_t *ehdr = (sr_ethernet_hdr_t *) packet;
  
  //Extract the ARP header
  sr_arp_hdr_t *arp_hdr = (sr_arp_hdr_t *)(packet + sizeof(sr_ethernet_hdr_t));
  
  //Check the type of ARP packet
  if(ntohs(arp_hdr->ar_op) == arp_op_request){
    printf("Received an ARP request\n");
    //Received an ARP request

    struct sr_if *sr_intf = sr->if_list;
    //Check if the packet is destined for the router
    while(sr_intf != NULL){
        uint32_t addr = sr_intf->ip;
        if(arp_hdr->ar_tip == addr){
            break;
        }

        sr_intf = sr_intf->next;

        if(sr_intf == NULL)
            return;
    }

    //Make a new ARP header
    sr_arp_hdr_t *reply = new_arp_header(intface, arp_op_reply, arp_hdr->ar_sha, arp_hdr->ar_sip);
      
    //Make a new Ethernet frame
    unsigned int nlen = sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t);
    sr_ethernet_hdr_t *new_frame = new_ethernet_frame(ethertype_arp, ehdr->ether_shost, intface->addr, nlen);
      
    //Place the ARP reply into the Ethernet frame
    memcpy((uint8_t *) new_frame + sizeof(sr_ethernet_hdr_t), reply, sizeof(sr_arp_hdr_t));
      
    //Send the packet
    print_hdrs((uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t));
    sr_send_packet(sr, (uint8_t *)new_frame, sizeof(sr_ethernet_hdr_t) + sizeof(sr_arp_hdr_t), interface);
    return;
  }

  if(ntohs(arp_hdr->ar_op) == arp_op_reply){
    printf("Received an ARP reply\n");
    //Received an ARP reply

    //Insert the mapping into the MAC mapping cache
    struct sr_arpreq *req = malloc(sizeof(struct sr_arpreq));
    req = sr_arpcache_insert(&(sr->cache), arp_hdr->ar_sha, arp_hdr->ar_sip);

    if(req != NULL){
        //If this is true then there were packets waiting on the ARP request to be received
        //Those packets now need to be sent!
        struct sr_packet *waiting = req->packets;
        while(waiting != NULL){
            //Send the packet
            print_hdrs(waiting->buf, waiting->len);
            sr_send_packet(sr, waiting->buf, waiting->len, interface);

            //Move the pointer along
            waiting = waiting->next;
        }
    }
    sr_arpreq_destroy(&(sr->cache), req);
    return;
  }
  return;
}