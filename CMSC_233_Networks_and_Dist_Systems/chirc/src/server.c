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

// typedef struct chirc_server
// {
//     char *hostname;
//     char *port;
//     char *Password;
    
//     list_t channels;
//     pthread_mutex_t channels_lock;

//     list_t users;
//     pthread_mutex_t users_lock;

//     // char *version;
//     // struct_tm *created;
// } c_server;

// typedef struct chirc_user
// {
//     int clientSocket;
//     char* nick;
//     char* username;
    
//     pthread_mutex_t iuser_lock;
// } c_user;

server *server_init()
{
	server *new_server = malloc(sizeof(server));

	list_t channels;
	list_init(&channels);
	new_server->channels = channels;
	pthread_mutex_t channels_lock;
	pthread_mutex_init(&channels_lock, NULL);
	new_server->channels_lock = channels_lock;

	list_t users;
	list_init(&users);
	new_server->users = users;
	pthread_mutex_t users_lock;
	pthread_mutex_init(&users_lock, NULL);
	new_server->users_lock = users_lock;

	pthread_mutex_t msg_lock;
	pthread_mutex_init(&msg_lock, NULL);

	char *vport = NULL;
	new_server->port = vport;

	char *password = NULL;
	new_server->password = password;

	char *hostname = malloc(25);
	gethostname(hostname, 25);
	new_server->hostname = hostname;

	return new_server;
}

int seek_nick(const void *dummy_user, const void *key)
{
	user *new_user = (user *)dummy_user;
	user *nkey = (user *)key;
	if(new_user->nickname == NULL)
		return 0;
	if(!strcmp(new_user->nickname, nkey->nickname))
		return 1;
	else
		return 0;
}

user *user_list_seek_nick(server *curr_server, user *key)
{
	user *ret = malloc(sizeof(user));

	pthread_mutex_lock(&(curr_server->users_lock));

	list_attributes_seeker(&(curr_server->users), seek_nick);

	ret = list_seek(&(curr_server->users), key);

	pthread_mutex_unlock(&(curr_server->users_lock));

	return ret;
}

int seek_username(const void *dummy_user, const void *key)
{
	user *new_user = (user *)dummy_user;
	user *nkey = (user *)key;
	if(new_user->username == NULL)
		return 0;
	if(!strcmp(new_user->username, nkey->username))
		return 1;
	else
		return 0;
}

user *user_list_seek_username(server *curr_server, user *key)
{
	user *ret = malloc(sizeof(user));

	pthread_mutex_lock(&(curr_server->users_lock));

	list_attributes_seeker(&(curr_server->users), seek_username);

	ret = list_seek(&(curr_server->users), key);

	pthread_mutex_unlock(&(curr_server->users_lock));

	return ret;
}

void user_list_add(server *curr_server, user *curr_user)
{
	pthread_mutex_lock(&(curr_server->users_lock));

	list_append(&(curr_server->users), curr_user);

	pthread_mutex_unlock(&(curr_server->users_lock));

	return;
}

void server_delete_user(server *curr_server, user *curr_user)
{
	int pos;

	pthread_mutex_lock(&(curr_server->users_lock));

	list_attributes_seeker(&(curr_server->users), seek_nick);

	pos = list_locate(&(curr_server->users), curr_user);

	list_delete_at(&(curr_server->users), pos);

	pthread_mutex_unlock(&(curr_server->users_lock));

	return;
}