#ifndef STRUCTS
#define STRUCTS

typedef struct chirc_user
{
    int clientSocket;
    char *nickname;
    char *username;
    char *realname;

    char *hostname;
    
    char *modes;
    char **channels;
    char **ch_modes;

    pthread_mutex_t iuser_lock;
} user;

typedef struct chirc_server
{
    char *hostname;
    char *port;
    char *password;
    
    list_t channels;
    pthread_mutex_t channels_lock;

    list_t users;
    pthread_mutex_t users_lock;

    // char *version;
    // struct_tm *created;
} server;

typedef struct chirc_msg
{
	user *user_sender;
	server *server_sender;

    char *prefix;
    char *command;
    char **params;

    char *message;


} chat_message;

typedef struct chirc_channel
{
    list_t channel_users;
    pthread_mutex_t channel_users_lock;

    char *channel_name;
    char *Password;
    char *modes;
} channel;

#endif