/*
 *
 *  CMSC 23300 / 33300 - Networks and Distributed Systems
 *
 *  main() code for chirc project
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include "simclist.h"
#include "structs.h"
#include "mesg.h"
#include "server.h"
#include "user.h"
//#include "replies.h"
//#include "reply.h"
//#include "channel.h"

const char *def_port = "6667";

typedef struct pass_along
{
    server *curr_server;
    user *curr_user;
} pass_along;

// typedef struct chirc_server
// {
//     char *hostname;
//     char *port;
//     char *password;
    
//     list_t channels;
//     pthread_mutex_t channel_lock;

//     list_t users;
//     pthread_mutex_t users_lock;

//     // char *version;
//     // struct_tm *created;
// } server;

void listen_client(void *args)
{   
    struct pass_along *pa = (pass_along *)malloc(sizeof(pass_along));
    pa = (pass_along *)args;

    server *new_server = server_init();
    new_server = pa->curr_server;

    user *new_user = user_init();
    new_user = pa->curr_user;

    
    ssize_t recv_cnt;
    char *recv_buf = malloc(1024*sizeof(char));
    char *cmd_build = malloc(1024*sizeof(char));

    printf("In listen_client\n");

    while(1){
        while((recv_cnt = recv(new_user->clientSocket, recv_buf, 1024, 0)) > 0)
                {
                    
                    fprintf(stdout, "this was the message! ::%s...\n message length: %u\n", recv_buf, recv_cnt);
                }  
            }
    return;
}

void accept_clients(void *args)
{
    pass_along *pa = malloc(sizeof(pass_along));

    server *new_server = server_init();
    new_server = (server *)args;
    pa->curr_server = new_server;

    user *new_user = malloc(sizeof(user));

    int serverSocket;
    int clientSocket;
    pthread_t worker_thread;
    struct addrinfo hints, *res, *p;
    struct sockaddr_storage *clientAddr;
    socklen_t sinSize = sizeof(struct sockaddr_storage);
    
    int yes = 1;

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE; 

    if (getaddrinfo(NULL, new_server->port, &hints, &res) != 0)
    {
        perror("getaddrinfo() failed");
        pthread_exit(NULL);
    }

    for(p = res; p != NULL; p = p->ai_next)
    {
        if ((serverSocket = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1)
        {
            perror("Could not open socket");
            continue;
        }

        if (setsockopt(serverSocket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1)
        {
            perror("Socket setsockopt() failed");
            close(serverSocket);
            continue;
        }

        if (bind(serverSocket, p->ai_addr, p->ai_addrlen) == -1)
        {
            perror("Socket bind() failed");
            close(serverSocket);
            continue;
        }

        if (listen(serverSocket, 5) == -1)
        {
            perror("Socket listen() failed");
            close(serverSocket);
            continue;
        }

        break;
    }

    freeaddrinfo(res);

    if (p == NULL)
    {
        fprintf(stderr, "Could not find a socket to bind to.\n");
        pthread_exit(NULL);
    }

    /* Loop and wait for connections */
    while (1)
    {
        /* Call accept(). The thread will block until a client establishes a connection. */
        clientAddr = malloc(sinSize);
        if ((clientSocket = accept(serverSocket, (struct sockaddr *) clientAddr, &sinSize)) == -1)
        {
            /* If this particular connection fails, no need to kill the entire thread. */
            free(clientAddr);
            perror("Could not accept() connection");
            continue;
        }
        
        new_user = user_init();
        new_user->clientSocket = clientSocket;
        
        char *hostname = malloc(50*sizeof(char));
        getnameinfo((struct sockaddr *)clientAddr, sizeof(*clientAddr), hostname, 50, NULL, 0, 0);
        new_user->hostname = hostname;
        
        printf("Got a new user!\n");

        user_list_add(new_server, new_user);

        pa->curr_user = new_user;

        if (pthread_create(&worker_thread, NULL, (void *) &listen_client, (void *) pa) != 0) 
        {
            perror("Could not create a worker thread");
            free(clientAddr);
            close(clientSocket);
            close(serverSocket);
            pthread_exit(NULL);
        }
    }

    pthread_exit(NULL);

    return;
}

int main(int argc, char *argv[])
{
    server *new_server = server_init();
    
    pthread_t server_thread;
    
    int opt;

    while ((opt = getopt(argc, argv, "p:o:h")) != -1)
        switch (opt)
        {
        case 'p':
            def_port = strdup(optarg);
            new_server->port = strdup(optarg);
            break;
        case 'o':
            new_server->password = strdup(optarg);
            break;
        default:
            printf("ERROR: Unknown option -%c\n", opt);
            exit(-1);
        }

    if(!(new_server->port))
         new_server->port = strdup(def_port);

    // if (!(new_server->password)
    // {
    //     fprintf(stderr, "ERROR: You must specify an operator password\n");
    //     exit(-1);
    // }

    if (pthread_create(&server_thread, NULL, (void *) &accept_clients, (void *) new_server) < 0)
    {
        perror("Could not create server thread");
        exit(-1);
    }

    pthread_join(server_thread, NULL);

    pthread_exit(NULL);

    return 0;
}