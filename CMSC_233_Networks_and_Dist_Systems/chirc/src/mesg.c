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
//#include "server.h"
#include "user.h"
//#include "replies.h"
//#include "reply.h"
//#include "channel.h"

typedef void (*handler_function)(server *server, user *curr_user, chat_message *message);

// typedef struct chirc_msg
// {
//     char *prefix;
//     char *command;
//     char **params;
//     char *l_mesg;
// } chat_message;

// typedef void (*handler_function)(c_server * server, user *curr_user, chat_message *message);

// struct handler_entry
// {
//     char *name;
//     handler_function func;
// };

// struct handler_entry handlers[] = {
//     {"NICK", nick_handler},
//     {"USER", username_handler},
//     {"LUSERS", luser_handler},
//     {"MOTD", motd_handler},
//     {"PRIVMSG", privmsg_handler},
//     {"NOTICE", notice_handler},
//     {"WHOIS", whois_handler},
//     {"PING", ping_handler},
//     {"PONG", pong_handler},
//     {"JOIN", join_handler},
//     {"PART", part_handler},
//     {"QUIT", quit_handler},
//     {"OPER", oper_handler}
// };

//int num_handlers = sizeof(handlers)/sizeof(struct handler_entry);

chat_message *message_init()
{
    chat_message *message = malloc(sizeof(chat_message));

    char *prefix = NULL;
    char **params = malloc(sizeof(char *) * 15);
    char *command = NULL;
    char *mesg = NULL;

    message->prefix = prefix;
    message->command = command;
    message->params = params;
    message->message = mesg;

    return message;
}

chat_message *message_parse(server *curr_server, user *curr_user, char *command)
{
    return;
}

//Decoding commands from the received bytes to string commands
void msg_decode(unsigned int bytes_recv, char *recv_buf, char *cmd_build, server *curr_server, user *curr_user)
{
    // if(strcmp(recv_buf, "\r\n") == 0)
    //     return;

    // //Indices for copying over bytes
    // int recv_i;
    // //NOTE: the strlen sets the 'cmd_build' index to the last written
    // //spot, ie if an incomplete message was written to cmd_build last time
    // //recv was called
    // int cb_i = strlen(cmd_build);

    // for(recv_i=0; recv_i<brecv; recv_i++, cb_i++)
    // {
    //     //Copy each byte from the received buffer to the command
    //     //being built
    //     cmd_build[cb_i] = recv_buf[recv_i];
    //     //Check to see if we're at the end of a command
    //     if(cmd_build[cb_i] == '\n')
    //     {
    //         if(cmd_build[cb_i-1] == '\r')
    //         {

    //             //Make the finished command into a string
    //             cmd_build[cb_i-1] = '\0';

    //             //Pass the command to the parser
    //             message_parse(server, curr_user, cmd_build);

    //             //Reset the in-progress command buffer and the
    //             //corresponding index
    //             memset(cmd_build, 0, cb_i);
    //             cb_i = -1;
    //         }
    //     }
    // }
    return;
}