/*
 *  chiTCP - A simple, testable TCP stack
 *
 *  Type and data structure declarations for implementing the
 *  TCP protocol. See tcp.c for the functions that actually
 *  run the TCP protocol.
 *
 */

/*
 *  Copyright (c) 2013-2014, The University of Chicago
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
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
 *    software without specific prior written permission.
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
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "chitcp/buffer.h"
#include "chitcp/packet.h"

#ifndef TCP_H_
#define TCP_H_

#define TCP_BUFFER_SIZE (4096)
#define TCP_MSS (536)

/* TCP events. Roughly correspond to the ones specified in
 * http://tools.ietf.org/html/rfc793#section-3.9 */
typedef enum
{
    APPLICATION_CONNECT = 1,
    APPLICATION_SEND    = 2,
    APPLICATION_RECEIVE = 3,
    APPLICATION_CLOSE   = 4,
    PACKET_ARRIVAL      = 5,
    TIMEOUT             = 6,
    CLEANUP             = 7
} tcp_event_type_t;


/*  Many values in tcp_data have identifiers from RFC 793, as below     */

/*  From RFC 793 definition of the Transmission Control Block:

    Begin quote:

    Send Sequence Variables

    SND.UNA - send unacknowledged
    SND.NXT - send next
    SND.WND - send window
    SND.UP  - send urgent pointer
    SND.WL1 - segment sequence number used for last window update
    SND.WL2 - segment acknowledgment number used for last window
    update
    ISS     - initial send sequence number

    Receive Sequence Variables

    RCV.NXT - receive next
    RCV.WND - receive window
    RCV.UP  - receive urgent pointer
    IRS     - initial receive sequence number

    End quote.    */

/* SND.UP, SND.WL1, SND.WL2, and RCV.UP are unused */

static char *tcp_event_type_names[] =
{
    "APPLICATION_CONNECT",
    "APPLICATION_SEND",
    "APPLICATION_RECEIVE",
    "APPLICATION_CLOSE",
    "PACKET_ARRIVAL",
    "TIMEOUT",
    "CLEANUP"
};

static inline char *tcp_event_str (tcp_event_type_t evt)
{
    return tcp_event_type_names[evt-1];
}

/* TCP data. Roughly corresponds to the variables and buffers
 * one would expect in a Transmission Control Block (as
 * specified in RFC 793). */
typedef struct tcp_data
{
    /* Queue with pending packets received from the network */
    list_t pending_packets;
    pthread_mutex_t lock_pending_packets;
    pthread_cond_t cv_pending_packets;

    /* Queue for withheld packets (simulating unreliable network) */
    list_t withheld_packets;
    pthread_mutex_t lock_withheld_packets;

    /* Queue for packets received out of order */
    list_t unordered_packets;
    pthread_mutex_t lock_unordered_packets;

    /*Retransmission queue*/
    list_t retrans;
    pthread_mutex_t lock_retrans;

    /* Transmission control block */

    /* Send sequence variables */
    uint32_t ISS;      /* Initial send sequence number */
    uint32_t SND_UNA;  /* First byte sent but not acknowledged */
    uint32_t SND_NXT;  /* Next sendable byte */
    uint16_t SND_WND;  /* Send Window */

    /* Receive sequence variables */
    uint32_t IRS;      /* Initial receive sequence number */
    uint32_t RCV_NXT;  /* Next byte expected */
    uint16_t RCV_WND;  /* Receive Window */

    /* Buffers */
    circular_buffer_t send;
    circular_buffer_t recv;

    /* Has a CLOSE been requested on this socket? */
    bool_t closing;

    /* Has a packet been received out of order? */
    bool_t unordered;

    /* The SRTT */
    float SRTT;
} tcp_data_t;


typedef struct packet_node
{
    // A copy of the packet
    tcp_packet_t *packet;
    //The time the packet was sent
    struct timespec send_time;
    //The time the packet was received
    struct timespec recv_time;
    //The condition variable to be used in the timeout thread
    pthread_cond_t cond;
    //The pthread to be initialized
    pthread_t thread;
    //Boolean predicate for pthread_cond_timedwait
    bool_t signaled;
    //Boolean to see if a packet has timed out
    bool_t timeout;
}packet_node_t;

void tcp_data_init(tcp_data_t *tcp_data);
void tcp_data_free(tcp_data_t *tcp_data);

#endif /* TCP_H_ */
