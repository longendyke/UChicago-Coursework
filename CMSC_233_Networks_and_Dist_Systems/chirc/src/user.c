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

// typedef struct chirc_user
// {
//     int clientSocket;
//     char* nick;
//     char* username;
    
//     pthread_mutex_t iuser_lock;
// } c_user;

user *user_init()
{
	// Just initializing all the elements of a user

	user *new_user = malloc(sizeof(user));

	new_user->nickname = NULL;
	new_user->username = NULL;
	new_user->realname = NULL;

	new_user->modes = malloc(sizeof(char)*2);

	new_user->channels = NULL;
	new_user->ch_modes = NULL;

	pthread_mutex_t iuser_lock;
	pthread_mutex_init(&iuser_lock, NULL);
	new_user->iuser_lock = iuser_lock;

	return new_user;
}