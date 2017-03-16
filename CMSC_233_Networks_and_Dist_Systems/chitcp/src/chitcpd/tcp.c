/*
 *  chiTCP - A simple, testable TCP stack
 *
 *  Implementation of the TCP protocol.
 *
 *  chiTCP follows a state machine approach to implementing TCP.
 *  This means that there is a handler function for each of
 *  the TCP states (CLOSED, LISTEN, SYN_RCVD, etc.). If an
 *  event (e.g., a packet arrives) while the connection is
 *  in a specific state (e.g., ESTABLISHED), then the handler
 *  function for that state is called, along with information
 *  about the event that just happened.
 *
 *  Each handler function has the following prototype:
 *
 *  int f(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event);
 *
 *  si is a pointer to the chiTCP server info. The functions in
 *       this file will not have to access the data in the server info,
 *       but this pointer is needed to call other functions.
 *
 *  entry is a pointer to the socket entry for the connection that
 *          is being handled. The socket entry contains the actual TCP
 *          data (variables, buffers, etc.), which can be extracted
 *          like this:
 *
 *            tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
 *
 *          Other than that, no other fields in "entry" should be read
 *          or modified.
 *
 *  event is the event that has caused the TCP thread to wake up. The
 *          list of possible events corresponds roughly to the ones
 *          specified in http://tools.ietf.org/html/rfc793#section-3.9.
 *          They are:
 *
 *            APPLICATION_CONNECT: Application has called socket_connect()
 *            and a three-way handshake must be initiated.
 *
 *            APPLICATION_SEND: Application has called socket_send() and
 *            there is unsent data in the send buffer.
 *
 *            APPLICATION_RECEIVE: Application has called socket_recv() and
 *            any received-and-acked data in the recv buffer will be
 *            collected by the application (up to the maximum specified
 *            when calling socket_recv).
 *
 *            APPLICATION_CLOSE: Application has called socket_close() and
 *            a connection tear-down should be initiated.
 *
 *            PACKET_ARRIVAL: A packet has arrived through the network and
 *            needs to be processed (RFC 793 calls this "SEGMENT ARRIVES")
 *
 *            TIMEOUT: A timeout (e.g., a retransmission timeout) has
 *            happened.
 *
 */

/*
 *  Copyright (c) 2013-2014, The University of Chicago
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or withsend
 *  modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  - Neither the name of The University of Chicago nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software withsend specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY send OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "chitcp/log.h"
#include "chitcp/buffer.h"
#include "serverinfo.h"
#include "connection.h"
#include "tcp.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

//Args for timeout thread
typedef struct pass_along
{
    serverinfo_t *si;
    chisocketentry_t *entry;
    packet_node_t *node;
} pa_t;

#define max(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

float comp_SRTT(float RTT, float SRTT)
{
    //Alpha = 0.85
    int nSRTT;
    nSRTT = (0.85)*SRTT + (1-0.85)*RTT;
    return nSRTT;
}

float comp_RTO(float UBOUND, float LBOUND, float SRTT)
{
    int RTO;
    RTO = max(UBOUND, max(LBOUND, (1.65*SRTT)));
    return RTO;
}

int seek_seq(const void *packet, const void *keypkt)
{
    tcp_packet_t *key = (tcp_packet_t *)keypkt;
    tcp_packet_t *npacket = (tcp_packet_t *)packet;
    if(SEG_SEQ(npacket) == SEG_SEQ(key))
        return 1;
    else
        return 0;
}

void start_timeout_thread(void *pa)
{
    //Unpacking arguments
    pa_t *pass_along = malloc(sizeof(pa_t));
    pass_along = (pa_t *)pa;

    serverinfo_t *si;
    si = pass_along->si;
    chisocketentry_t *entry;
    entry = pass_along->entry;

    packet_node_t *node;
    node = pass_along->node;
    struct timespec send_time;
    send_time = node->send_time;
    pthread_cond_t cond;
    cond = node->cond;
    bool_t signaled = node->signaled;
    bool_t timeout = node->timeout;

    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    //Timer to be used in pthread_cond_timedwait
    struct timespec time_to_wait;

    //Calculate RTO (note the use of tcp_data)
    float RTO = comp_RTO(60.0, 1.0, tcp_data->SRTT);

    //Updating values of the time_to_wait timer
    time_to_wait.tv_sec = send_time.tv_sec+(time_t)trunc(RTO);
    time_to_wait.tv_nsec = send_time.tv_nsec+(long)(RTO-trunc(RTO));

    //Mutex for pthread_cond_timedwait
    pthread_mutex_t thread_lock;

    //Initialization of mutexes
    pthread_mutex_init(&thread_lock, NULL);
    pthread_cond_init(&cond, NULL);

    pthread_mutex_lock(&thread_lock);

    while(1)
    {
        int rc;
        rc = pthread_cond_timedwait(&cond, &thread_lock, &time_to_wait);
        if(rc == ETIMEDOUT)
        {
            timeout = TRUE;
            chitcpd_timeout(si, entry);
            break;
        }
        else if(rc == 0 && signaled == TRUE)
        {   
            break;
        }
        else{
            continue;
        }
    }
    pthread_mutex_unlock(&thread_lock);

    pthread_mutex_destroy(&thread_lock);

    return;
}

packet_node_t *packet_node_init(serverinfo_t *si, chisocketentry_t *entry, tcp_packet_t *packet)
{
    packet_node_t *new = malloc(sizeof(packet_node_t));
    tcp_packet_t *newPacket = malloc(sizeof(tcp_packet_t));
    memcpy(newPacket, packet, sizeof(tcp_packet_t));
    new->packet = newPacket;
    chilog_tcp(DEBUG, new->packet, LOG_OUTBOUND);

    struct timespec send_time;
    struct timespec recv_time;

    //Initialize send_time to now
    clock_gettime(CLOCK_REALTIME, &send_time);

    new->send_time = send_time;
    new->recv_time = recv_time;

    pthread_cond_t cond;
    new->cond = cond;

    pthread_t thread;
    new->thread = thread;

    bool_t signaled = FALSE;
    new->signaled = signaled;
    bool_t timeout = FALSE;
    new->timeout = timeout;

    // new->signaled = FALSE;
    // new->timeout = FALSE;

    pa_t *pass_along = malloc(sizeof(pa_t));
    pass_along->si = si;
    pass_along->entry = entry;
    pass_along->node = new;
    pthread_create(&thread, NULL, (void *) &start_timeout_thread, (void *) &pass_along);

    return new;
}

void packet_node_free(packet_node_t *node)
{
    //pthread_cond_destroy(&node->cond);
    //chitcp_tcp_packet_free(node->packet);
    return;
}

tcp_packet_t *tcp_data_seek_seq(tcp_data_t *tcp_data, tcp_packet_t *key)
{
    tcp_packet_t *ret = malloc(sizeof(tcp_packet_t));

    pthread_mutex_lock(&(tcp_data->lock_unordered_packets));

    list_attributes_seeker(&(tcp_data->unordered_packets), seek_seq);

    ret = list_seek(&(tcp_data->unordered_packets), key);

    pthread_mutex_unlock(&(tcp_data->lock_unordered_packets));

    return ret;
}

//removes all acked packets in retransmission queue
void remove_acked_packets(tcp_data_t *tcp_data, int ackNum)
{
    packet_node_t *curr_node = malloc(sizeof(packet_node_t));
    tcp_packet_t *curr_packet = malloc(sizeof(tcp_packet_t));

    list_t temp;
    list_init(&temp);

    //look at every packet in retransmission queue and remove if it has been acked.
    while(list_size(&(tcp_data->retrans)) != 0)
    {
        curr_node = list_fetch(&(tcp_data->retrans));
        curr_packet= curr_node->packet;
        if(SEG_SEQ(curr_packet) < ackNum)
        {
            curr_node->signaled = TRUE;
            pthread_cond_signal(&curr_node->cond);
	  struct timespec  rTime;
	  clock_gettime(CLOCK_REALTIME, &rTime);
	  struct timespec sTime =  curr_node->send_time;
	  long nDiff = rTime.tv_nsec - sTime.tv_nsec;
	  double diff = difftime(rTime.tv_sec, sTime.tv_sec);
	  float rtt = diff + (nDiff * 1000000000);
	  tcp_data->SRTT = comp_SRTT(rtt, tcp_data->SRTT);
            //Free the retransmit packet (list fetch already removes it from the list)
            packet_node_free(curr_node);
            free(curr_node);
        }
        else
        {
            list_append(&(temp), curr_node);
        }
    }

    packet_node_t *temp_node = malloc(sizeof(packet_node_t));
    while(list_size(&temp) != 0){
        temp_node = list_fetch(&(temp));
        list_append(&(tcp_data->retrans), temp_node);
        packet_node_free(temp_node);
    }
    list_destroy(&temp);
    free(temp_node);
    
    return;
}


void send_timedout_packets(serverinfo_t *si, chisocketentry_t *entry)
{
    packet_node_t *curr_node = malloc(sizeof(packet_node_t));
    tcp_packet_t *curr_packet = malloc(sizeof(tcp_packet_t));
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    list_t temp;
    list_init(&temp);

    //look at every packet in retransmission queue and resend as needed
    while(list_size(&(tcp_data->retrans)) != 0)
    {
        curr_node = list_fetch(&(tcp_data->retrans));
        curr_packet = curr_node->packet;
        if(curr_node->timeout == TRUE)
        {
            chitcpd_send_tcp_packet(si, entry, curr_packet);

            //make a fresh node and re-append here.
            packet_node_t *new_node = packet_node_init(si, entry, curr_packet);
            list_append(&(temp), new_node);
            //Free the retransmit packet (list fetch already removes it from the list)
            packet_node_free(curr_node);
            free(curr_node);
        }
        else
        {
            list_append(&(temp), curr_node);
        }
    }

    packet_node_t *tempNode = malloc(sizeof(packet_node_t));
    while (list_size(&temp) != 0)
    {
	   tempNode = list_fetch(&(temp));
	   list_append(&(tcp_data->retrans), tempNode);
    }
    list_destroy(&temp);
    packet_node_free(tempNode);
    free(tempNode);

    return;
}

void tcp_data_init(tcp_data_t *tcp_data)
{
    list_init(&tcp_data->pending_packets);
    pthread_mutex_init(&tcp_data->lock_pending_packets, NULL);
    pthread_cond_init(&tcp_data->cv_pending_packets, NULL);
    list_init(&tcp_data->withheld_packets);
    pthread_mutex_init(&tcp_data->lock_withheld_packets, NULL);

    /* Initialization of additional tcp_data_t fields goes here */
    list_init(&tcp_data->unordered_packets);
    pthread_mutex_init(&tcp_data->lock_unordered_packets, NULL);

    list_init(&tcp_data->retrans);
    pthread_mutex_init(&tcp_data->lock_retrans, NULL);

    //Initialize SRTT to 0
    tcp_data->SRTT = 0;
}

void tcp_data_free(tcp_data_t *tcp_data)
{
    circular_buffer_free(&tcp_data->send);
    circular_buffer_free(&tcp_data->recv);
    list_destroy(&tcp_data->pending_packets);
    pthread_mutex_destroy(&tcp_data->lock_pending_packets);
    pthread_cond_destroy(&tcp_data->cv_pending_packets);

    list_destroy(&tcp_data->withheld_packets);
    pthread_mutex_destroy(&(tcp_data->lock_withheld_packets));

    /* Cleanup of additional tcp_data_t fields goes here */
    list_destroy(&tcp_data->unordered_packets);
    pthread_mutex_destroy(&(tcp_data->lock_unordered_packets));

    list_destroy(&tcp_data->retrans);
    pthread_mutex_destroy(&(tcp_data->lock_retrans));
}


int chitcpd_tcp_state_handle_CLOSED(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == APPLICATION_CONNECT)
    {
        /* Your code goes here */
        tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

        //If passive enter the LISTEN state and return.
        if(entry->actpas_type == 2)
        {
            chitcpd_update_tcp_state(si, entry, LISTEN);
            return 1;
        }
        //If active and foreign socket is specified, issue a SYN segment.
        if(entry->actpas_type == 1)
        {
            tcp_packet_t *packet = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;

            chitcpd_tcp_packet_create(entry, packet, NULL, 0);
            header = TCP_PACKET_HEADER(packet);
            header->syn = 1;

            //Select and assign an ISS.
            long int r = rand() % 255;
            r *= 10000;
            tcp_data->ISS = r;

            tcp_data->RCV_WND = circular_buffer_available(&(tcp_data->recv));
            header->win = chitcp_htons(tcp_data->RCV_WND);
            header->seq = chitcp_htonl(r);

            //Set SND.UNA to ISS, SND.NXT to ISS+1, enter SYN-SENT state, and return.
            tcp_data->SND_UNA = r;
            tcp_data->SND_NXT = r + 1;
            chitcpd_send_tcp_packet(si, entry, packet);
            packet_node_t *new_node = packet_node_init(si, entry, packet);
	        list_append(&(tcp_data->retrans), new_node);
            chitcpd_update_tcp_state(si, entry, SYN_SENT);
            chilog(DEBUG, "Sending packet in APPLICATION_CONNECT...");
            chilog_tcp(DEBUG, packet, LOG_OUTBOUND);
            return CHITCP_OK;
        }

    }
    else if (event == CLEANUP)
    {
        /* Any additional cleanup goes here */
    }
    else
        chilog(WARNING, "In CLOSED state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_LISTEN(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else
        chilog(WARNING, "In LISTEN state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_SYN_RCVD(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets(si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In SYN_RCVD state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_SYN_SENT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets(si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In SYN_SENT state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_ESTABLISHED(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    chilog(DEBUG, "In ESTABLISHED handler");

    if (event == APPLICATION_SEND)
    {
        /* Your code goes here */
        tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
        circular_buffer_t *sbuff = &tcp_data->send;

        if (circular_buffer_count(sbuff) > tcp_data->SND_WND)
        {
            //respond with "error:  insufficient resources"//
            chilog(ERROR, "error: insufficient resouces");
            return 0;
        }

        uint32_t x = tcp_data->SND_WND + tcp_data->SND_NXT - tcp_data->SND_UNA;
        uint32_t readLen = circular_buffer_count(sbuff);
        int count = circular_buffer_count(sbuff);
        if (x < count)
            readLen = x;

        //Read from buff, create, and send packets until receive buffer is full
        while (readLen)
        {
            tcp_packet_t *packet = malloc(sizeof(tcp_packet_t));
            uint8_t payload [TCP_MSS];

            int numRead;
            if (readLen > TCP_MSS)
                numRead = TCP_MSS;
            else
                numRead = readLen;

            circular_buffer_read(sbuff, payload, numRead, TRUE);
            readLen -= numRead;

            chitcpd_tcp_packet_create(entry, packet, payload, numRead);
            tcphdr_t *header;

            header = TCP_PACKET_HEADER(packet);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->ack = 1;
            header->win = chitcp_htons(tcp_data->RCV_WND);
            header->seq = chitcp_htonl(tcp_data->SND_NXT);

            tcp_data->SND_NXT += numRead;

            chilog(DEBUG, "Sending packet from APPLICATION_SEND:");
            chilog_tcp(DEBUG, packet, LOG_OUTBOUND);
            chitcpd_send_tcp_packet(si, entry, packet);
            packet_node_t *new_node = packet_node_init(si, entry, packet);
            list_append(&(tcp_data->retrans), new_node);

            chitcp_tcp_packet_free(packet);
            free(packet);
        }

        if(circular_buffer_count(sbuff) == 0 && tcp_data->closing == TRUE)
        {
            chitcpd_update_tcp_state(si, entry, CLOSED);

            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->fin = 1;
            header->seq = chitcp_htonl(tcp_data->ISS);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);

            chitcpd_send_tcp_packet(si, entry, to_send);


        }
        return 1;
    }

    else if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == APPLICATION_RECEIVE)
    {
        /* Your code goes here */
        tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
        circular_buffer_t *rbuff = &tcp_data->recv;

        tcp_data->RCV_WND = circular_buffer_available(rbuff);
    }
    else if (event == APPLICATION_CLOSE)
    {
        if(chitcpd_handle_application_close(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets (si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In ESTABLISHED state, received unexpected event (%i).", event);

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_FIN_WAIT_1(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets (si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In FIN_WAIT_1 state, received unexpected event (%i).", event);

    return CHITCP_OK;
}


int chitcpd_tcp_state_handle_FIN_WAIT_2(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets (si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In FIN_WAIT_2 state, received unexpected event (%i).", event);

    return CHITCP_OK;
}


int chitcpd_tcp_state_handle_CLOSE_WAIT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == APPLICATION_CLOSE)
    {
        if(chitcpd_handle_application_close(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets (si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In CLOSE_WAIT state, received unexpected event (%i).", event);


    return CHITCP_OK;
}


int chitcpd_tcp_state_handle_CLOSING(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else if (event == TIMEOUT)
    {
        /* Your code goes here */
        send_timedout_packets (si, entry);
        return CHITCP_OK;
    }
    else
        chilog(WARNING, "In CLOSING state, received unexpected event (%i).", event);

    return CHITCP_OK;
}


int chitcpd_tcp_state_handle_TIME_WAIT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    chilog(WARNING, "Running handler for TIME_WAIT. This should not happen.");

    return CHITCP_OK;
}


int chitcpd_tcp_state_handle_LAST_ACK(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        if(chitcpd_handle_packet_arrival(si, entry, event) == 1)
            return CHITCP_OK;
    }
    else
        chilog(WARNING, "In LAST_ACK state, received unexpected event (%i).", event);

    return CHITCP_OK;
}

int chitcpd_handle_packet_arrival(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    //Extract tcp data struct
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    //Extracting packet received
    pthread_mutex_lock(&(tcp_data->lock_pending_packets));
    tcp_packet_t *packet = list_fetch(&(tcp_data->pending_packets));
    pthread_mutex_unlock(&(tcp_data->lock_pending_packets));

    chilog(DEBUG, "PACKET_ARRIVAL receive packet dump");
    chilog_tcp(DEBUG, packet, LOG_INBOUND);

    //Extracting "received header" ie r_header
    tcphdr_t *r_header = TCP_PACKET_HEADER(packet);

    //=========================================//
    //~~~       OUT OF ORDER HANDLING       ~~~//
    //=========================================//

    //Immediately checking to see if the sequence number of the packet is
    //the same as we expected to receive. If not, immediately send the packet
    //to the out of order buffer to be processed afterwards.
    if(SEG_SEQ(packet) != tcp_data->RCV_NXT && tcp_data->RCV_NXT != 0)
    {
        chilog(WARNING, "Received a packet out of order:");
        chilog_tcp(WARNING, packet, LOG_INBOUND);
        list_append(&(tcp_data->unordered_packets), packet);
        tcp_data->unordered = 1;

        return 1;
    }

    // END ADDITION //

    if(entry->tcp_state == CLOSED)
    {
        // all data in the incoming segment is discarded.  An incoming
        // segment containing a RST is discarded.  An incoming segment not
        // containing a RST causes a RST to be sent in response.  The
        // acknowledgment and sequence field values are selected to make the
        // reset sequence acceptable to the TCP that sent the offending
        // segment.

        //if the ACK bit is off, sequence number zero is used
        if(r_header->ack)
        {
            chilog(WARNING, "received an ack in CLOSED state...");
            return 1;
        }
        //if the ACK bit is on, return
        else
        {
            chilog(WARNING, "received a packet in CLOSED state...");
            return 1;
        }
    }

    if(entry->tcp_state == LISTEN)
    {
        //first check for an RST
        if(r_header->rst)
        {
            chilog(WARNING, "received an RST in LISTEN state...");
            return 1;
        }
        //second check for an ACK
        if(r_header->ack)
        {
            //Any acknowledgment is bad if it arrives on a connection still in
            //the LISTEN state.
            chilog(WARNING, "received an ACK in LISTEN state...");
            return 1;
        }
        //third check for a SYN
        if(r_header->syn)
        {
            //Set RCV.NXT to SEG.SEQ+1, IRS is set to SEG.SEQ
            tcp_data->RCV_NXT = SEG_SEQ(packet)+1;
            tcp_data->IRS = SEG_SEQ(packet);

            //ISS should be selected and a SYN segment sent of the form:

            //  <SEQ=ISS> <ACK=RCV.NXT> <CTL=SYN,ACK>
            //Select and assign an ISS.
            long int r = rand() % 255;
            r *= 10000;
            tcp_data->ISS = r;

            tcp_data->SND_WND = SEG_WND(packet);
            tcp_data->RCV_WND = circular_buffer_available(&(tcp_data->recv));

            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->syn = 1;
            header->seq = chitcp_htonl(tcp_data->ISS);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 1");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chilog(DEBUG, "in response to:");
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);
            

            //SND.NXT is set to ISS+1 and SND.UNA to ISS.  The connection
            //state should be changed to SYN-RECEIVED.  Note that any other
            //incoming control or data (combined with SYN) will be processed
            //in the SYN-RECEIVED state, but processing of SYN and ACK should
            //not be repeated.
            tcp_data->SND_NXT = tcp_data->ISS + 1;
            tcp_data->SND_UNA = tcp_data->ISS;
            chitcpd_update_tcp_state(si, entry, SYN_RCVD);
        }
        else
        {
            chilog(WARNING, "received a strange packet in LISTEN state...");
            //return 1;
        }
    }
    if(entry->tcp_state == SYN_SENT)
    {
        //first check the ACK bit
        if(r_header->ack)
        {
            //If SEG.ACK =< ISS, or SEG.ACK > SND.NXT, send a reset
            if((SEG_ACK(packet) <= tcp_data->ISS)||
                    (SEG_ACK(packet) > tcp_data->SND_NXT))
            {
                if(r_header->rst)
                {
                    chilog(WARNING, "in SYN_SENT state, PACKET_ARRIVAL handler, SEG_ACK <= ISS, or SEG_ACK > SND_NXT...");
                    return 1;
                }
            }
            //If SND.UNA =< SEG.ACK =< SND.NXT then the ACK is acceptable.
            if((tcp_data->SND_UNA <= SEG_ACK(packet) &&
                    SEG_ACK(packet) < tcp_data->SND_NXT))
                chilog(DEBUG, "In SYN_SENT state, PACKET_ARRIVAL handler, ACK was acceptable");
        }

        //second check the RST bit
        if(r_header->rst)
        {
            //If the ACK was acceptable then signal the user "error:
            //connection reset", drop the segment, enter CLOSED state,
            //delete TCB, and return.  Otherwise (no ACK) drop the segment
            //and return.

            chilog(ERROR, "in SYN_SENT, PACKET_ARRIVAL, a packet arrived with the RST bit set...");
            return 1;
        }

        //third check security and precedence (SKIPPED)

        //fourth check the SYN bit
        if(r_header->syn)
        {
            //This step should be reached only if the ACK is ok, or there is
            //no ACK, and it the segment did not contain a RST.

            //If the SYN bit is on and the security/compartment and precedence
            //are acceptable then, RCV.NXT is set to SEG.SEQ+1, IRS is set to
            //SEG.SEQ.  SND.UNA should be advanced to equal SEG.ACK (if there
            //is an ACK), and any segments on the retransmission queue which
            //are thereby acknowledged should be removed.

            tcp_data->RCV_NXT = SEG_SEQ(packet)+1;
            tcp_data->IRS = SEG_SEQ(packet);
            tcp_data->SND_WND = SEG_WND(packet);
            tcp_data->RCV_WND = circular_buffer_available(&(tcp_data->recv));
            if(r_header->ack)
                tcp_data->SND_UNA = SEG_ACK(packet);
            //Removing any acked packets
            remove_acked_packets(tcp_data, tcp_data->SND_UNA);


            //If SND.UNA > ISS (our SYN has been ACKed), change the connection
            //state to ESTABLISHED, form an ACK segment

            //<SEQ=SND.NXT> <ACK=RCV.NXT> <CTL=ACK>
            if(tcp_data->SND_UNA > tcp_data->ISS)
            {


                tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
                tcphdr_t *header;
                chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
                header = TCP_PACKET_HEADER(to_send);
                header->ack = 1;
                header->seq = chitcp_htonl(tcp_data->SND_NXT);
                header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
                header->win = chitcp_htons(tcp_data->RCV_WND);
                chilog(DEBUG, "PACKET_ARRIVAL send dump 2");
                chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
                chilog(DEBUG, "in response to:");
                chilog_tcp(DEBUG, packet, LOG_INBOUND);
                chitcpd_send_tcp_packet(si, entry, to_send);
                chitcpd_update_tcp_state(si, entry, ESTABLISHED);

                //If there are other controls or
                //text in the segment then continue processing at the sixth step
                //below where the URG bit is checked, otherwise return.
                if(TCP_PAYLOAD_LEN(packet) != 0)
                {
                    goto SYN_SENT_WITH_PAYLOAD;
                }
                else
                    return 1;
            }

            //Otherwise enter SYN-RECEIVED, form a SYN,ACK segment

            //<SEQ=ISS> <ACK=RCV.NXT> <CTL=SYN,ACK>

            //and send it.
            else
            {
                chitcpd_update_tcp_state(si, entry, SYN_RCVD);

                tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
                tcphdr_t *header;
                chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
                header = TCP_PACKET_HEADER(to_send);
                header->ack = 1;
                header->syn = 1;
                header->seq = chitcp_htonl(tcp_data->ISS);
                header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
                header->win = chitcp_htons(tcp_data->RCV_WND);
                chilog(DEBUG, "PACKET_ARRIVAL send dump 3");
                chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
                chilog(DEBUG, "in response to:");
                chilog_tcp(DEBUG, packet, LOG_INBOUND);
                chitcpd_send_tcp_packet(si, entry, to_send);
                packet_node_t *new_node = packet_node_init(si, entry, to_send);
                list_append(&(tcp_data->retrans), new_node);
                return 1;
            }
        }

        //fifth, if neither of the SYN or RST bits is set then drop the
        //segment and return.
        else
        {
            chilog(WARNING, "in SYN_SENT state, in PACKET_ARRIVAL, neither SYN bit nor RST bit were set...");
            return 1;
        }
    }

    //=====================================================================================================//
    //From here on out, the states we must deal with are SYN_RCVD, ESTABLISHED, FIN_WAIT_1, FIN_WAIT_2,
    //CLOSE_WAIT, CLOSING, LAST_ACK, and TIME_WAIT.
    //=====================================================================================================//

    //Otherwise, first check the sequence number
    //All states other than those tested above MUST go through this part
    if(1)
    {
        //There are four cases for the acceptability test for an incoming
        //segment:
        // Segment Receive  Test
        // Length  Window
        // ------- -------  -------------------------------------------

        //    0       0     SEG.SEQ = RCV.NXT

        //    0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND

        //   >0       0     not acceptable

        //   >0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND
        //                   or RCV.NXT =< SEG.SEQ+SEG.LEN-1 < RCV.NXT+RCV.WND

        //CASE 1: 0       0     SEG.SEQ = RCV.NXT
        if((SEG_SEQ(packet) == tcp_data->RCV_NXT) &&
                (TCP_PAYLOAD_LEN(packet) == 0) &&
                (tcp_data->RCV_WND == 0))
        {
            chilog(DEBUG, "in PACKET_ARRIVAL, received ACK with receive window = 0");
            //If the RCV.WND is zero, no segments will be acceptable, but
            //special allowance should be made to accept valid ACKs, URGs and
            //RSTs.

            //If an incoming segment is not acceptable, an acknowledgment
            //should be sent in reply (unless the RST bit is set, if so drop
            //the segment and return):

            //<SEQ=SND.NXT> <ACK=RCV.NXT> <CTL=ACK>
            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->seq = chitcp_htonl(tcp_data->SND_NXT);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 4");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chilog(DEBUG, "in response to:");
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);

            //After sending the acknowledgment, drop the unacceptable segment
            //and return.
            return 1;
        }
        //CASE 2: 0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND
        if((tcp_data->RCV_NXT <= SEG_SEQ(packet)) &&
                (SEG_SEQ(packet) < (tcp_data->RCV_NXT + tcp_data->RCV_WND)) &&
                TCP_PAYLOAD_LEN(packet) == 0 &&
                (tcp_data->RCV_WND > 0))
        {
            chilog(DEBUG, "in PACKET_ARRIVAL, packet passed acceptability test!");
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
        }
        //CASE 3:>0       0     not acceptable
        if((TCP_PAYLOAD_LEN(packet) > 0) &&
                (tcp_data->RCV_WND == 0))
        {
            chilog(ERROR, "in PACKET_ARRIVAL, got a packet with PAYLOAD_LEN > 0 and receive window = 0...");

            //If an incoming segment is not acceptable, an acknowledgment
            //should be sent in reply (unless the RST bit is set, if so drop
            //the segment and return):

            //<SEQ=SND.NXT> <ACK=RCV.NXT> <CTL=ACK>
            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->seq = chitcp_htonl(tcp_data->SND_NXT);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 5");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chilog(DEBUG, "in response to:");
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);
            

            //After sending the acknowledgment, drop the unacceptable segment
            //and return.
            return 1;
        }
        //CASE 4:>0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND --> CHECKED ABOVE IN CASE 2
        //                       or RCV.NXT =< SEG.SEQ+SEG.LEN-1 < RCV.NXT+RCV.WND
        if((tcp_data->RCV_NXT <= (SEG_SEQ(packet) + TCP_PAYLOAD_LEN(packet) - 1)) &&
                ((SEG_SEQ(packet) + TCP_PAYLOAD_LEN(packet) - 1) < (tcp_data->RCV_NXT + tcp_data->RCV_WND)) &&
                (TCP_PAYLOAD_LEN(packet) > 0) && (tcp_data->RCV_WND > 0))
        {
            chilog(DEBUG, "in PACKET_ARRIVAL, packet passed acceptability test!");
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
        }
        // else{
        //     chilog(WARNING, "in PACKET_ARRIVAL, got a packet that did not pass acceptability tests");
        //     chilog(WARNING, "THIS IS THAT PACKET");
        //     chilog_tcp(DEBUG, packet, LOG_INBOUND);
        //     return 1;
        // }
    }

    //=====================================//
    //packet has passed acceptability tests//
    //=====================================//

    //second check the RST bit
    if(entry->tcp_state == SYN_RCVD)
    {
        if(r_header->rst)
        {
            //The RFC states multiple options regarding the nature of whether or
            //not there was a passive/active open, however, since we aren't
            //implementing the RST bit, I'm choosing to drop this packet.
            chilog(ERROR, "in SYN_RCVD, PACKET_ARRIVAL, a packet arrived with the RST bit set...");
            return 1;
        }
    }
    if(entry->tcp_state == ESTABLISHED || entry->tcp_state == FIN_WAIT_1 ||
            entry->tcp_state == FIN_WAIT_2 || entry->tcp_state == CLOSE_WAIT)
    {
        if(r_header->rst)
        {
            chilog(ERROR, "in %i, PACKET_ARRIVAL, a packet arrived with the RST bit set...", entry->tcp_state);
            return 1;
        }
    }
    if(entry->tcp_state == CLOSING ||entry->tcp_state == LAST_ACK || entry->tcp_state == TIME_WAIT)
    {
        //If the RST bit is set then, enter the CLOSED state, delete the
        //TCB, and return.
        //gonna ignore this for now.....
        if(r_header->rst)
        {
            chilog(DEBUG, "in %i state, in PACKET_ARRIVAL, a packet arrived with RST bit set");
            return 1;
        }
    }
    //third check the security and precedence (SKIPPED)

    //fourth check the SYN bit
    if(entry->tcp_state == SYN_RCVD || entry->tcp_state == ESTABLISHED ||
            entry->tcp_state == FIN_WAIT_1 || entry->tcp_state == FIN_WAIT_2 ||
            entry->tcp_state == CLOSE_WAIT || entry->tcp_state == CLOSING ||
            entry->tcp_state == LAST_ACK || entry->tcp_state == TIME_WAIT)
    {
        if(r_header->syn)
        {
            //If the SYN is in the window it is an error, send a reset
            chilog(ERROR, "in %s, PACKET_ARRIVAL, a packet arrived with the SYN bit set...", entry->tcp_state);
            chilog_tcp(DEBUG, packet, LOG_INBOUND);
            return 1;

            //NOTE: If the SYN is not in the window this step would not be reached
            //and an ack would have been sent in the first step (sequence
            //number check).
        }
    }

    //fifth check the ACK bit
    if(!(r_header->ack))
    {
        chilog(ERROR, "in %i, PACKET_ARRIVAL, received a packet with the ACK bit turned off...", entry->tcp_state);
        return 1;
    }

    if(entry->tcp_state == SYN_RCVD)
    {
        if(r_header->ack)
        {
            //If SND.UNA =< SEG.ACK =< SND.NXT then enter ESTABLISHED state
            //and continue processing.
            if((tcp_data->SND_UNA <= SEG_ACK(packet) &&
                    (SEG_ACK(packet) <= tcp_data->SND_NXT)))
            {
                tcp_data->RCV_WND = SEG_WND(packet);
                tcp_data->SND_UNA = SEG_ACK(packet);
                chitcpd_update_tcp_state(si, entry, ESTABLISHED);
                tcp_data->SND_NXT = SEG_ACK(packet);
            }

            //If the segment acknowledgment is not acceptable, form a
            //reset segment, and send it
            else
            {
                chilog(ERROR, "in SYN_RCVD, PACKET_ARRIVAL, a packet arrived without an acceptable ACK bit...");
                return 1;
            }
        }
    }

    if(entry->tcp_state == ESTABLISHED || entry->tcp_state == FIN_WAIT_1 ||
            entry->tcp_state == FIN_WAIT_2 || entry->tcp_state == CLOSE_WAIT ||
            entry->tcp_state == CLOSING || entry->tcp_state == LAST_ACK ||
            entry->tcp_state == TIME_WAIT)
    {
        if(r_header->ack)
        {
            //If SND.UNA < SEG.ACK =< SND.NXT then, set SND.UNA <- SEG.ACK
            if(tcp_data->SND_UNA < SEG_ACK(packet) && SEG_ACK(packet) <= tcp_data->SND_NXT)
            {
                chilog(DEBUG, "updating SND_NXT and SND_UNA: %i\t%i", tcp_data->SND_NXT, tcp_data->SND_UNA);
                tcp_data->SND_UNA = SEG_ACK(packet);
                chilog(DEBUG, "updated SND_NXT and SND_UNA: %i\t%i", tcp_data->SND_NXT, tcp_data->SND_UNA);

                //Removing ACK'd packets
                remove_acked_packets(tcp_data, tcp_data->SND_UNA);
                //P2B: any segments on the retransmission queue which are thereby entirely ack'd are removed
                //If SND.UNA < SEG.ACK =< SND.NXT, the send window should be updated.
                tcp_data->SND_WND = SEG_WND(packet);
            }
            //If the ACK is a duplicate (SEG.ACK < SND.UNA), it can be ignored.
            if(SEG_ACK(packet) < tcp_data->SND_UNA)
            {
                chilog(DEBUG, "in %i state, PACKET_ARRIVAL, received a duplicate ACK...", entry->tcp_state);
                return 1;
            }
            //If the ACK acks something not yet sent (SEG.ACK > SND.NXT) then send an ACK,
            //drop the segment, and return.
            if(SEG_ACK(packet) > tcp_data->SND_NXT)
            {
                tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
                tcphdr_t *header;
                chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
                header = TCP_PACKET_HEADER(to_send);
                header->ack = 1;
                header->seq = chitcp_htonl(tcp_data->SND_NXT);
                header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
                header->win = chitcp_htons(tcp_data->RCV_WND);
                chilog(DEBUG, "PACKET_ARRIVAL send dump 6");
                chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
                chilog(DEBUG, "in response to:");
                chilog_tcp(DEBUG, packet, LOG_INBOUND);
                chitcpd_send_tcp_packet(si, entry, to_send);
                
                chilog(DEBUG, "in %i state, PACKET_ARRIVAL, received an ACK for something not yet sent...", entry->tcp_state);
                return 1;
            }

            if(entry->tcp_state == FIN_WAIT_1)
            {
                //In addition to the processing for the ESTABLISHED state, if
                //our FIN is now acknowledged then enter FIN-WAIT-2 and continue
                //processing in that state.
                if(tcp_data->closing && (tcp_data-> SND_UNA == tcp_data->SND_NXT))
                {
                    chitcpd_update_tcp_state(si, entry, FIN_WAIT_2);
                }
            }
            if(entry->tcp_state == FIN_WAIT_2)
            {
                if(r_header->fin)
                {
                    chilog(INFO, "Nothing to see here, move along");
                }
            }

            if(entry->tcp_state == CLOSING)
            {
                //In addition to the processing for the ESTABLISHED state, if
                //the ACK acknowledges our FIN then enter the TIME-WAIT state,
                //otherwise ignore the segment
                if(tcp_data->closing)
                {
                    chitcpd_update_tcp_state(si, entry, TIME_WAIT);
                    chitcpd_update_tcp_state(si, entry, CLOSED);
                }
            }

            if(entry->tcp_state == LAST_ACK)
            {
                //The only thing that can arrive in this state is an
                //acknowledgment of our FIN.  If our FIN is now acknowledged,
                //delete the TCB, enter the CLOSED state, and return.
                if(tcp_data->closing)
                {
                    chitcpd_update_tcp_state(si, entry, CLOSED);
                    return 1;
                }
            }
        }
    }

    //sixth, check the URG bit (SKIPPED)

SYN_SENT_WITH_PAYLOAD:

    //seventh, process the segment text
    if(entry->tcp_state == ESTABLISHED || entry->tcp_state == FIN_WAIT_1 || entry->tcp_state == FIN_WAIT_2)
    {
        if(TCP_PAYLOAD_LEN(packet) != 0)
        {
            //Once in the ESTABLISHED state, it is possible to deliver segment text to user RECEIVE buffers.
            circular_buffer_write(&(tcp_data->recv), TCP_PAYLOAD_START(packet), TCP_PAYLOAD_LEN(packet), 0);
            //Once the TCP takes responsibility for the data it advances
            //RCV.NXT over the data accepted, and adjusts RCV.WND as
            //apporopriate to the current buffer availability.  The total of
            //RCV.NXT and RCV.WND should not be reduced.
            tcp_data->RCV_NXT += SEG_LEN(packet);
            tcp_data->RCV_WND = circular_buffer_available(&(tcp_data->recv));

            //=========================================//
            //~~~       OUT OF ORDER HANDLING       ~~~//
            //=========================================//
            //Check if a packet has arrived out of order before sending an ack
            //for the packet just processed. If there has been an out of order packet
            //sent, look for the next correct packet in the out-of-order list.
            //If the next packet we need is there, add it to the pending packets queue.
            //If not, continue.

            if(tcp_data->unordered == FALSE)
            {
                //Send an acknowledgment of the form:
                //<SEQ=SND.NXT> <ACK=RCV.NXT> <CTL=ACK>
                tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
                tcphdr_t *header;
                chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
                header = TCP_PACKET_HEADER(to_send);
                header->ack = 1;
                header->seq = chitcp_htonl(tcp_data->SND_NXT);
                header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
                header->win = chitcp_htons(tcp_data->RCV_WND);
                chilog(DEBUG, "PACKET_ARRIVAL send dump 7");
                chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
                chilog(DEBUG, "in response to:");
                chilog_tcp(DEBUG, packet, LOG_INBOUND);
                chitcpd_send_tcp_packet(si, entry, to_send);
                
            }
            else
            {
                //Holder for the packet received by the search function
                tcp_packet_t *next_packet = malloc(sizeof(tcp_packet_t));

                //Mockup searcher packet
                tcp_packet_t *packet = malloc(sizeof(tcp_packet_t));
                tcphdr_t *header;
                chitcpd_tcp_packet_create(entry, packet, NULL, 0);
                header = TCP_PACKET_HEADER(packet);
                header->seq = chitcp_htonl(tcp_data->RCV_NXT);
                next_packet = tcp_data_seek_seq(tcp_data, packet);
                if(next_packet != NULL)
                    list_append(&(tcp_data->pending_packets), next_packet);
                else if(next_packet == NULL && list_size(&tcp_data->unordered_packets) != 0)
                {
                    tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
                    tcphdr_t *header;
                    chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
                    header = TCP_PACKET_HEADER(to_send);
                    header->ack = 1;
                    header->seq = chitcp_htonl(tcp_data->SND_NXT);
                    header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
                    header->win = chitcp_htons(tcp_data->RCV_WND);
                    chilog(DEBUG, "PACKET_ARRIVAL send dump 7.5");
                    chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
                    chilog(DEBUG, "in response to:");
                    chilog_tcp(DEBUG, packet, LOG_INBOUND);
                    chitcpd_send_tcp_packet(si, entry, to_send);
                    
                }

                chitcp_tcp_packet_free(packet);

                if(list_size(&tcp_data->unordered_packets) == 0)
                    tcp_data->unordered = FALSE;
            }
            //   END ADDITION   //
        }
    }
    if(entry->tcp_state == CLOSE_WAIT || entry->tcp_state == CLOSING ||
            entry->tcp_state == LAST_ACK || entry->tcp_state == TIME_WAIT)
    {
        //This should not occur, since a FIN has been received from the
        //remote side.  Ignore the segment text.
        chilog(DEBUG, "in %i state, PACKET_ARRIVAL, received a packet with payload after a FIN packet has been sent...",
               entry->tcp_state);
        chilog(DEBUG, "this is that packet:");
        chilog_tcp(DEBUG, packet, LOG_INBOUND);
    }

    //eighth, check the FIN bit
    if(r_header->fin)
    {
        if(entry->tcp_state == CLOSED || entry->tcp_state == LISTEN || entry->tcp_state == SYN_SENT)
        {
            //Do not process the FIN if the state is CLOSED, LISTEN or SYN-SENT
            //since the SEG.SEQ cannot be validated; drop the segment and
            //return.
            chilog(ERROR, "in %i state, in PACKET_ARRIVAL, made it to eighth check (FIN) even though should have returned by now",
                   entry->tcp_state);
            return 1;
        }
        //If the FIN bit is set, signal the user "connection closing" and
        //return any pending RECEIVEs with same message, advance RCV.NXT
        //over the FIN, and send an acknowledgment for the FIN.  Note that
        //FIN implies PUSH for any segment text not yet delivered to the
        //user.
        tcp_data->RCV_NXT = SEG_SEQ(packet)+1;

        tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
        tcphdr_t *header;
        chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
        header = TCP_PACKET_HEADER(to_send);
        header->ack = 1;
        header->seq = chitcp_htonl(tcp_data->SND_NXT);
        header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
        header->win = chitcp_htons(tcp_data->RCV_WND);
        chilog(DEBUG, "PACKET_ARRIVAL send dump 8");
        chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
        chilog(DEBUG, "in response to:");
        chilog_tcp(DEBUG, packet, LOG_INBOUND);
        chitcpd_send_tcp_packet(si, entry, to_send);
        

        if(entry->tcp_state == SYN_RCVD || entry->tcp_state == ESTABLISHED)
        {
            //Enter the CLOSE-WAIT state.
            circular_buffer_close(&(tcp_data->recv));
            chitcpd_update_tcp_state(si, entry, CLOSE_WAIT);
        }

        if(entry->tcp_state == FIN_WAIT_1)
        {
            //If our FIN has been ACKed (perhaps in this segment), then
            //enter TIME-WAIT, start the time-wait timer, turn off the other
            //timers; otherwise enter the CLOSING state.
            circular_buffer_close(&(tcp_data->recv));
            if(tcp_data->closing && (tcp_data-> SND_UNA == tcp_data->SND_NXT))
            {
                chitcpd_update_tcp_state(si, entry, TIME_WAIT);
                chitcpd_update_tcp_state(si, entry, CLOSED);
                return 1;
            }
            else
                chitcpd_update_tcp_state(si, entry, CLOSING);
        }

        if(entry->tcp_state == FIN_WAIT_2)
        {
            //Enter the TIME-WAIT state.  Start the time-wait timer, turn
            //off the other timers.
            circular_buffer_close(&(tcp_data->recv));
            chitcpd_update_tcp_state(si, entry, TIME_WAIT);
            chitcpd_update_tcp_state(si, entry, CLOSED);
            return 1;
        }

        //Remain in the CLOSE-WAIT state.
        if(entry->tcp_state == CLOSE_WAIT)
        {
            chilog(DEBUG, "in CLOSE_WAIT state, in PACKET_ARRIVAL, remaining in CLOSE_WAIT state");
            return 1;
        }
        //Remain in the CLOSING state.
        if(entry->tcp_state == CLOSING)
        {
            chilog(DEBUG, "in CLOSING state, in PACKET_ARRIVAL, remaining in CLOSING state");
            return 1;
        }
        //Remain in the LAST_ACK state.
        if(entry->tcp_state == LAST_ACK)
        {
            chilog(DEBUG, "in LAST_ACK state, in PACKET_ARRIVAL, remaining in LAST_ACK state");
            return 1;
        }
    }
    return 0;
}

int chitcpd_handle_application_close(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    if(entry->tcp_state == CLOSED)
    {
        chilog(ERROR, "error: connection does not exist");
        return 1;
    }
    if(entry->tcp_state == LISTEN)
    {
        chilog(ERROR, "error: closing");
        chitcpd_update_tcp_state(si, entry, CLOSED);
        return 1;
    }
    if(entry->tcp_state == SYN_SENT)
    {
        chilog(ERROR, "error: connection closing");
        tcp_data_free(tcp_data);
        return 1;
    }
    if(entry->tcp_state == SYN_RCVD)
    {
        //If no SENDs have been issued and there is no pending data to send,
        //then form a FIN segment and send it, and enter FIN-WAIT-1 state;
        //otherwise queue for processing after entering ESTABLISHED state.
        if(circular_buffer_count(&(tcp_data->send)) != 0)
        {
            tcp_data->closing = 1;
            return 1;
        }
        if(circular_buffer_count(&(tcp_data->send)) == 0)
        {
            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->fin = 1;
            header->seq = chitcp_htonl(tcp_data->SND_NXT);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 9");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);
            packet_node_t *new_node = packet_node_init(si, entry, to_send);
            list_append(&(tcp_data->retrans), new_node);
            chitcpd_update_tcp_state(si, entry, FIN_WAIT_1);
            tcp_data->SND_NXT++;
        }
        return 1;
    }
    if(entry->tcp_state == ESTABLISHED)
    {
        //Queue this until all preceding SENDs have been segmentized, then
        //form a FIN segment and send it.  In any case, enter FIN-WAIT-1
        //state.
        if(circular_buffer_count(&(tcp_data->send)) == 0)
        {
            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->fin = 1;
            header->seq = chitcp_htonl(tcp_data->SND_NXT);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 10");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);
            packet_node_t *new_node = packet_node_init(si, entry, to_send);
            list_append(&(tcp_data->retrans), new_node);
            tcp_data->SND_NXT++;
        }
        circular_buffer_close(&(tcp_data->recv));
        tcp_data->closing = 1;
        chitcpd_update_tcp_state(si, entry, FIN_WAIT_1);
        return 1;
    }
    if(entry->tcp_state == FIN_WAIT_1 || entry->tcp_state == FIN_WAIT_2)
    {
        //Strictly speaking, this is an error and should receive a "error:
        //connection closing" response.
        chilog(ERROR, "error: connection closing");
        return 1;
    }
    if(entry->tcp_state == CLOSE_WAIT)
    {
        if(circular_buffer_count(&(tcp_data->send)) == 0)
        {
            tcp_data->closing = 1;
            tcp_packet_t *to_send = malloc(sizeof(tcp_packet_t));
            tcphdr_t *header;
            chitcpd_tcp_packet_create(entry, to_send, NULL, 0);
            header = TCP_PACKET_HEADER(to_send);
            header->ack = 1;
            header->fin = 1;
            header->seq = chitcp_htonl(tcp_data->SND_NXT);
            header->ack_seq = chitcp_htonl(tcp_data->RCV_NXT);
            header->win = chitcp_htons(tcp_data->RCV_WND);
            chilog(DEBUG, "PACKET_ARRIVAL send dump 12");
            chilog_tcp(DEBUG, to_send, LOG_OUTBOUND);
            chitcpd_send_tcp_packet(si, entry, to_send);
            packet_node_t *new_node = packet_node_init(si, entry, to_send);
            list_append(&(tcp_data->retrans), new_node);
            chitcpd_update_tcp_state(si, entry, LAST_ACK);
            tcp_data->SND_NXT++;
        }
        return 1;
    }
    if(entry->tcp_state == CLOSING || entry->tcp_state == LAST_ACK || entry->tcp_state == TIME_WAIT)
    {
        chilog(ERROR, "error: connection closing");
        return 1;
    }
    return 1;
}
