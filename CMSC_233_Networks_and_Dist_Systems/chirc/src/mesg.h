#ifndef MESG
#define MESG

typedef void (*handler_function)(server * curr_server, user *curr_user, chat_message *message);

struct handler_entry
{
    char *name;
    handler_function handle;
};

void msg_decode(unsigned int bytes_recv, char *recv_buf, char *cmd_build, server *curr_server, user *curr_user);

chat_message *message_parse(server *curr_server, user *curr_user, char *command);

#endif