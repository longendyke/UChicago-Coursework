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
#include "replies.h"
#include "reply.h"
#include "channel.h"

// typedef struct chirc_channel
// {
//     list_t channel_users;
//     pthread_mutex_t channel_users_lock;

//     char *channel_name;
//     char *Password;
// } c_channel;

c_channel *channel_init()
{
	c_channel *channel = malloc(sizeof(c_channel));

	list_t channel_users;
	list_init(&channel_users);
	channel->channel_users = channel_users;
	pthread_mutex_t channel_users_lock;
	pthread_mutex_init(&channel_users_lock, NULL);
	channel->channel_users_lock = channel_users_lock;

	char *channel_name = NULL;
	channel->channel_name = channel_name;

	char *Password = NULL;
	channel->Password = Password;

	return channel;
}

int seek_channel_name(const void *channel, const void *key)
{
	c_channel *nchannel = (c_channel *)channel;
	c_channel *nkey = (c_channel *)key;
	if(nchannel->channel_name == NULL)
		return 0;
	if(!strcmp(nchannel->channel_name, nkey->channel_name))
		return 1;
	else
		return 0;
}

c_channel *server_seek_channel(c_server *server, c_channel *key)
{
	c_channel *ret = malloc(sizeof(c_channel));

	pthread_mutex_lock(&(server->channels_lock));

	list_attributes_seeker(&(server->channels), seek_channel_name);

	ret = list_seek(&(server->channels), key);

	pthread_mutex_unlock(&(server->channels_lock));

	return ret;
}

c_user *channel_user_list_seek_nick(c_channel *channel, c_user *key)
{
	c_user *ret = malloc(sizeof(c_user));

	pthread_mutex_lock(&(channel->channel_users_lock));

	list_attributes_seeker(&(channel->channel_users), seek_nick);

	ret = list_seek(&(channel->channel_users), key);

	pthread_mutex_unlock(&(channel->channel_users_lock));

	return ret;
}

void channel_user_list_add(c_channel *channel, c_user *user)
{
	pthread_mutex_lock(&(channel->channel_users_lock));

	list_append(&(channel->channel_users), user);

	pthread_mutex_unlock(&(channel->channel_users_lock));

	return;
}

void server_channel_list_add(c_server *server, c_channel *channel)
{
	pthread_mutex_lock(&(server->channels_lock));
	
	list_append(&(server->channels), channel);

	pthread_mutex_unlock(&(server->channels_lock));

	return;
}

void channel_send_all(c_server *server, c_channel *channel, char *message)
{
	pthread_mutex_lock(&(channel->channel_users_lock));

		list_iterator_start(&(channel->channel_users));

		while(list_iterator_hasnext(&(channel->channel_users)))
		{
			c_user *iterator = list_iterator_next(&(channel->channel_users));
			if(iterator->nick != NULL)
				send_message(server, iterator, message);
		}

		list_iterator_stop(&(channel->channel_users));

		pthread_mutex_unlock(&(channel->channel_users_lock));

		return;
}

void channel_send_allbutme(c_server *server, c_channel *channel, c_user *user, char *message)
{
	pthread_mutex_lock(&(channel->channel_users_lock));

		list_iterator_start(&(channel->channel_users));

		while(list_iterator_hasnext(&(channel->channel_users)))
		{
			c_user *iterator = list_iterator_next(&(channel->channel_users));
			if(iterator->nick != NULL && strcmp(iterator->nick, user->nick))
				send_message(server, iterator, message);
		}

		list_iterator_stop(&(channel->channel_users));

		pthread_mutex_unlock(&(channel->channel_users_lock));

		return;
}

void server_delete_channel(c_server *server, c_channel *channel)
{
	int pos;

	pthread_mutex_lock(&(server->channels_lock));

	list_attributes_seeker(&(server->channels), seek_channel_name);

	pos = list_locate(&(server->channels), channel);

	list_delete_at(&(server->channels), pos);

	pthread_mutex_unlock(&(server->channels_lock));

	return;
}

void channel_delete_user(c_server *server, c_channel *channel, c_user *user)
{
	int pos;

	pthread_mutex_lock(&(channel->channel_users_lock));

	list_attributes_seeker(&(channel->channel_users), seek_nick);

	pos = list_locate(&(channel->channel_users), user);

	list_delete_at(&(channel->channel_users), pos);

	if(list_size(&(channel->channel_users)) == 0){
		server_delete_channel(server, channel);
		list_destroy(&(channel->channel_users));
		// free(channel->channel_name);
		// free(channel->Password);
		pthread_mutex_unlock(&(channel->channel_users_lock));
		// pthread_mutex_destroy(&(channel->channel_users_lock));
		// free(channel);
		return;
	}

	pthread_mutex_unlock(&(channel->channel_users_lock));

	return;
}