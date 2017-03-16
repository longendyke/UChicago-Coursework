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

void send_message(c_server *server, c_user *user, char *reply)
{
	pthread_mutex_lock(&(user->iuser_lock));
	int msg_len = strlen(reply);
	int sent;
	while(msg_len > 0){
		sent = (int) send(user->clientSocket, reply, msg_len, 0);
		msg_len = msg_len - sent;
	}

	pthread_mutex_unlock(&(user->iuser_lock));
	return;
}

void ping_handler(c_server *server, c_user *user, c_message *message)
{
	char *repl = malloc(600);
	sprintf(repl, "PONG %s\r\n", 
		server->hostname);
	send_message(server, user, repl);

	return;
}

void pong_handler(c_server *server, c_user *user, c_message *message)
{
	return;
}

void motd_handler(c_server *server, c_user *user, c_message *message)
{
	FILE *file = NULL;
	//char *motd = NULL;
	int fsize = 0;

	if((file = fopen("motd.txt", "r")) == NULL){
		char *repl = malloc(100);
		sprintf(repl, ":%s %s %s :MOTD File is missing\r\n", 
			server->hostname, ERR_NOMOTD, user->nick);
		send_message(server, user, repl);
	}
	else{
		
		fseek(file, 0, SEEK_END);
		fsize = ftell(file);
		rewind(file);

		char *motd = (char *) malloc(sizeof(char) * fsize);
		fread(motd, 1, fsize, file);

		fclose(file);

		send_message(server, user, motd);
	}
	return;
}

void whois_handler(c_server *server, c_user *user, c_message *message)
{
	char *nick = message->params[0];
	c_user *searcher = c_user_init();
	searcher->nick = nick;
	c_user *ret_user;

	if((ret_user = user_list_seek_nick(server, searcher)) != NULL){
		char *repl0 = malloc(600);
		char *repl1 = malloc(600);
		char *repl2 = malloc(600);
		sprintf(repl0, ":%s %s %s %s %s %s * %s\r\n", 
			server->hostname, RPL_WHOISUSER, user->nick, 
			ret_user->nick, ret_user->username, ret_user->hostname, ret_user->realname);
		sprintf(repl1, ":%s %s %s %s %s :chirc-0.2.2\r\n", 
			server->hostname, RPL_WHOISSERVER, user->nick, 
			user->nick, server->hostname);
		sprintf(repl2, ":%s %s %s %s :End of WHOIS list\r\n", 
			server->hostname, RPL_ENDOFWHOIS, user->nick, 
			user->nick);
		send_message(server, user, repl0);
		send_message(server, user, repl1);
		send_message(server, user, repl2);
		}
	else{
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :No such nick/channel\r\n", 
			server->hostname, ERR_NOSUCHNICK, user->nick, nick);
		send_message(server, user, repl);
	}
}

void luser_handler(c_server *server, c_user *user, c_message *message)
{
	int size = 0;
	int unknown = 0;
	int registered = 0;

	pthread_mutex_lock(&(server->users_lock));

	size = list_size(&(server->users));

	list_iterator_start(&(server->users));

	while(list_iterator_hasnext(&(server->users)))
	{
		c_user *iterator = list_iterator_next(&(server->users));
		if(iterator->nick == NULL || iterator->username == NULL)
			unknown++;
	}

	list_iterator_stop(&(server->users));

	registered = size - unknown;

	char *repl0 = malloc(100);
	char *repl1 = malloc(100);
	char *repl2 = malloc(100);
	char *repl3 = malloc(100);
	char *repl4 = malloc(100);
	sprintf(repl0, ":%s %s %s :There are %d users and 0 services on 1 servers\r\n", 
		server->hostname, RPL_LUSERCLIENT, user->nick, registered);
	sprintf(repl1, ":%s %s %s 0 :operator(s) online\r\n", 
		server->hostname, RPL_LUSEROP, user->nick);
	sprintf(repl2, ":%s %s %s %d :unknown connection(s)\r\n", 
		server->hostname, RPL_LUSERUNKNOWN, user->nick, unknown);
	sprintf(repl3, ":%s %s %s 0 :channels formed\r\n", 
		server->hostname, RPL_LUSERCHANNELS, user->nick);
	sprintf(repl4, ":%s %s %s :I have %d clients and 1 servers\r\n", 
		server->hostname, RPL_LUSERME, user->nick, size);
	send_message(server, user, repl0);
	send_message(server, user, repl1);
	send_message(server, user, repl2);
	send_message(server, user, repl3);
	send_message(server, user, repl4);

	pthread_mutex_unlock(&(server->users_lock));

	return;
}

void nick_handler(c_server *server, c_user *user, c_message *message)
{
	char *nick = message->params[0];
	c_user *searcher = c_user_init();
	searcher->nick = nick;
	c_user *ret_user;

	if((ret_user = user_list_seek_nick(server, searcher)) != NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s * %s :Nickname is already in use\r\n", 
			server->hostname, ERR_NICKNAMEINUSE, nick);
		send_message(server, user, repl);
		}
	else
	{
		if(user->username != NULL && user->nick == NULL){
			user->nick = nick;
			char *repl0 = malloc(600);
			char *repl1 = malloc(600);
			char *repl2 = malloc(600);
			char *repl3 = malloc(100);
			sprintf(repl0, ":%s %s %s :Welcome to the Internet Relay Network %s!%s@%s\r\n", 
				server->hostname, RPL_WELCOME, user->nick, user->nick, user->username, user->hostname);
			sprintf(repl1, ":%s %s %s :Your host is %s running version chirc-0.2.2\r\n", 
				server->hostname, RPL_YOURHOST, user->nick, server->hostname);
			sprintf(repl2, ":%s %s %s :This server was created 10:07:05\r\n", 
				server->hostname, RPL_CREATED, user->nick);
			sprintf(repl3, ":%s %s %s %s chirc-0.2.2 ao mtov\r\n", 
				server->hostname, RPL_MYINFO, user->nick, server->hostname);
			send_message(server, user, repl0);
			send_message(server, user, repl1);
			send_message(server, user, repl2);
			send_message(server, user, repl3);
			luser_handler(server, user, message);
			motd_handler(server, user, message);
		}
		else
		{
			if(user->nick != NULL){
				int i = 0;
				while(user->channels[i] != NULL){
					
					c_channel *chsearcher = channel_init();
					chsearcher->channel_name = user->channels[i];
					c_channel *ret_channel = NULL;
					ret_channel = server_seek_channel(server, chsearcher);
					char *send_all = malloc(sizeof(char)*600);
					sprintf(send_all, ":%s!%s@%s NICK :%s\r\n", 
						user->nick, user->username, user->hostname, message->params[0]);
					
					channel_send_all(server, ret_channel, send_all);
					i++;
				}

			}
			user->nick = nick;
		}
	}
	return;
}

void username_handler(c_server *server, c_user *user, c_message *message)
{

	char *username = message->params[0];
	char *realname = message->l_mesg;

	c_user *searcher = c_user_init();
	searcher->username = username;
	c_user *ret_user;

	if((ret_user = user_list_seek_username(server, searcher)) != NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s * %s :Username is already registered\r\n", 
			server->hostname, ERR_ALREADYREGISTRED, username);
		send_message(server, user, repl);
	}
	else
	{
		if(user->nick != NULL){
			user->username = username;
			user->realname = realname;
			char *repl0 = malloc(600);
			char *repl1 = malloc(600);
			char *repl2 = malloc(600);
			char *repl3 = malloc(100);
			sprintf(repl0, ":%s %s %s :Welcome to the Internet Relay Network %s!%s@%s\r\n", 
				server->hostname, RPL_WELCOME, user->nick, user->nick, user->username, user->hostname);
			sprintf(repl1, ":%s %s %s :Your host is %s running version chirc-0.2.2\r\n", 
				server->hostname, RPL_YOURHOST, user->nick, server->hostname);
			sprintf(repl2, ":%s %s %s :This server was created 10:07:05\r\n", 
				server->hostname, RPL_CREATED, user->nick);
			sprintf(repl3, ":%s %s %s %s chirc-0.2.2 ao mtov\r\n", 
				server->hostname, RPL_MYINFO, user->nick, server->hostname);
			send_message(server, user, repl0);
			send_message(server, user, repl1);
			send_message(server, user, repl2);
			send_message(server, user, repl3);
			luser_handler(server, user, message);
			motd_handler(server, user, message);
		}
		else
		{
			user->username = username;
			user->realname = realname;
		}

	}

	return;
}

void quit_handler(c_server *server, c_user *user, c_message *message)
{
	int i = 0;
	if(user->channels == NULL){
		char *repl = malloc(600);
		sprintf(repl, "ERROR :Closing Link %s (client Quit)\r\n", user->hostname);

		send_message(server, user, repl);
		close(user->clientSocket);
		server_delete_user(server, user);
		return;
	}
	while(user->channels[i] != NULL){
		char *parting_message = message->l_mesg;
		
		c_channel *chsearcher = channel_init();
		chsearcher->channel_name = user->channels[i];
		c_channel *ret_channel = NULL;
		ret_channel = server_seek_channel(server, chsearcher);

		char *send_all = malloc(sizeof(char)*600);
		if(parting_message != NULL){
			sprintf(send_all, ":%s!%s@%s QUIT %s\r\n", 
				user->nick, user->username, user->hostname, parting_message);
		}
		else{
			sprintf(send_all, ":%s!%s@%s QUIT :Client Quit\r\n", 
				user->nick, user->username, user->hostname);
		}
		printf("send_all:\t%s\n", send_all);
		channel_send_all(server, ret_channel, send_all);
		channel_delete_user(server, ret_channel, user);
		i++;
	}
	char *repl = malloc(600);
	sprintf(repl, "ERROR :Closing Link %s (client Quit)\r\n", user->hostname);

	send_message(server, user, repl);
	close(user->clientSocket);
	server_delete_user(server, user);
	return;
}


void channel_privmsg(c_server *server, c_user *user, c_message *message)
{
	char *channel_name = message->params[0];
	c_channel *searcher = channel_init();
	searcher->channel_name = channel_name;
	c_channel *ret_channel = NULL;
	c_user *ret_user = NULL;
	
	if((ret_channel = server_seek_channel(server, searcher)) == NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :No such nick/channel\r\n", 
			server->hostname, ERR_NOSUCHNICK, user->nick, channel_name);
		send_message(server, user, repl);
	}
	else if((ret_user = channel_user_list_seek_nick(ret_channel, user)) == NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :Cannot send to channel\r\n", 
			server->hostname, ERR_CANNOTSENDTOCHAN, user->nick, channel_name);
		send_message(server, user, repl);
	}
	else{
		char *send_all = malloc(sizeof(char)*600);
		sprintf(send_all, ":%s!%s@%s PRIVMSG %s %s\r\n", 
			user->nick, user->username, user->hostname, ret_channel->channel_name, message->l_mesg);		
		printf("send_all:\t%s\n", send_all);
		channel_send_allbutme(server, ret_channel, user, send_all);
	}
	return;
}

void privmsg_handler(c_server *server, c_user *user, c_message *message)
{	
	
	c_user *searcher = malloc(sizeof(c_user));

	searcher->nick = message->params[0];
	
	c_user *ret_user = NULL;

	if(message->params[0][0] == '#'){
		channel_privmsg(server, user, message);
		return;
	}

	if((ret_user = user_list_seek_nick(server, searcher)) != NULL){
		char *priv_msg = malloc(sizeof(char)*700);
		sprintf(priv_msg, ":%s!%s@%s PRIVMSG %s %s\r\n", 
			user->nick, user->username, user->hostname, message->params[0], message->l_mesg);
		
		send_message(server, ret_user, priv_msg);
		
	}
	else{
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :No such nick/channel\r\n", 
			server->hostname, ERR_NOSUCHNICK, user->nick, message->params[0]);
		send_message(server, user, repl);
	}
	return;
}

void notice_handler(c_server *server, c_user *user, c_message *message)
{
	char *send_nick = message->params[0];
	c_user *searcher = c_user_init();
	searcher->nick = send_nick;
	c_user *ret_user = NULL;

	if((ret_user = user_list_seek_nick(server, searcher)) != NULL){
		char *priv_msg = malloc(512);
		sprintf(priv_msg, ":%s!%s@%s NOTICE %s %s\r\n", 
			user->nick, user->username, user->hostname, send_nick, message->l_mesg);
		send_message(server, ret_user, priv_msg);
	}
	return;
}

void join_handler(c_server *server, c_user *user, c_message *message)
{
	char *channel_name = message->params[0];
	c_channel *searcher = channel_init();
	searcher->channel_name = channel_name;
	c_channel *ret_channel = NULL;

	if((ret_channel = server_seek_channel(server, searcher)) != NULL){
		c_user *error_check = c_user_init();
		if((error_check = channel_user_list_seek_nick(ret_channel, user))!=NULL)
			return;

		channel_user_list_add(ret_channel, user);

		char *join0 = malloc(sizeof(char)*700);
		char *join1 = malloc(sizeof(char)*700);
		sprintf(join0, ":%s %s %s = %s :", 
			server->hostname, RPL_NAMREPLY, user->nick, ret_channel->channel_name);
		
		pthread_mutex_lock(&(ret_channel->channel_users_lock));

		list_iterator_start(&(ret_channel->channel_users));

		c_user *first = list_iterator_next(&(ret_channel->channel_users));
		if(first->nick != NULL)
				sprintf(join0, "%s%s", join0, first->nick);

		while(list_iterator_hasnext(&(ret_channel->channel_users)))
		{
			c_user *iterator = list_iterator_next(&(ret_channel->channel_users));
			if(iterator->nick != NULL)
				sprintf(join0, "%s %s", join0, iterator->nick);
		}
		sprintf(join0, "%s\r\n", join0);

		list_iterator_stop(&(ret_channel->channel_users));

		pthread_mutex_unlock(&(ret_channel->channel_users_lock));

		sprintf(join1, ":%s %s %s %s :End of NAMES list\r\n", 
			server->hostname, RPL_ENDOFNAMES, user->nick, 
			ret_channel->channel_name);

		char *send_all = malloc(sizeof(char) * 600);
		sprintf(send_all, ":%s!%s@%s JOIN %s\r\n", 
			user->nick, user->username, user->hostname, channel_name);
		channel_send_all(server, ret_channel, send_all);

		send_message(server, user, join0);
		send_message(server, user, join1);

		if(user->channels == NULL){
			char **channels = malloc(sizeof(char *)*100);
			channels[0] = channel_name;
			user->channels = channels;
		}
	}
	else{
		ret_channel = channel_init();
		ret_channel->channel_name = channel_name;
		
		server_channel_list_add(server, ret_channel);
		
		channel_user_list_add(ret_channel, user);
		
		char *join0 = malloc(sizeof(char)*700);
		char *join1 = malloc(sizeof(char)*700);
		sprintf(join0, ":%s %s %s = %s :%s\r\n", 
			server->hostname, RPL_NAMREPLY, user->nick, ret_channel->channel_name, user->nick);
		sprintf(join1, ":%s %s %s %s :End of NAMES list\r\n", 
			server->hostname, RPL_ENDOFNAMES, user->nick, 
			ret_channel->channel_name);
		
		char *send_all = malloc(sizeof(char) * 600);
		sprintf(send_all, ":%s!%s@%s JOIN %s\r\n", 
			user->nick, user->username, user->hostname, channel_name);
		channel_send_all(server, ret_channel, send_all);

		send_message(server, user, join0);
		send_message(server, user, join1);

		if(user->channels == NULL){

			char **channels = malloc(sizeof(char *)*100);
			char **ch_modes = malloc(sizeof(char *)*100);

			channels[0] = channel_name;
			user->channels = channels;

			char *oper = "o";
			ch_modes[0] = oper;
			user->ch_modes = ch_modes;
			
		}
		else{
			int i = 0;
			while(user->channels[i] != NULL)
				i++;
			user->channels[i] = channel_name;
			user->ch_modes[i] = "o";
		}
	}
	return;
}

void part_handler(c_server *server, c_user *user, c_message *message)
{
	char *channel_name = message->params[0];
	char *parting_message = message->l_mesg;
	
	c_channel *searcher = channel_init();
	searcher->channel_name = channel_name;
	c_channel *ret_channel = NULL;
	c_user *ret_user = NULL;
	if((ret_channel = server_seek_channel(server, searcher)) == NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :No such channel\r\n", 
			server->hostname, ERR_NOSUCHCHANNEL, user->nick, channel_name);
		send_message(server, user, repl);
	}
	else if((ret_user = channel_user_list_seek_nick(ret_channel, user)) == NULL){
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s %s :You're not on that channel\r\n", 
			server->hostname, ERR_NOTONCHANNEL, user->nick, channel_name);
		send_message(server, user, repl);
	}
	else{
		char *send_all = malloc(sizeof(char) * 600);
		if(parting_message != NULL){
			sprintf(send_all, ":%s!%s@%s PART %s %s\r\n", 
				user->nick, user->username, user->hostname, ret_channel->channel_name, parting_message);
		}
		else{
			sprintf(send_all, ":%s!%s@%s PART %s\r\n", 
				user->nick, user->username, user->hostname, ret_channel->channel_name);
		}
		
		channel_send_all(server, ret_channel, send_all);
		channel_delete_user(server, ret_channel, user);
	}
	return;
}

void oper_handler(c_server *server, c_user *user, c_message *message)
{
	if(!strcmp(server->Password, message->params[1])){
		pthread_mutex_lock(&(user->iuser_lock));
		sprintf(user->modes, "o");
		pthread_mutex_unlock(&(user->iuser_lock));
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s :You are now an IRC operator\r\n", 
			server->hostname, RPL_YOUREOPER, user->nick);
		send_message(server, user, repl);
	}
	else{
		char *repl = malloc(600);
		sprintf(repl, ":%s %s %s :Password Incorrect\r\n", 
			server->hostname, RPL_YOUREOPER, user->nick);
		send_message(server, user, repl);
	}
	return;
}

// void channel_mode_handler(c_server *server, c_user *user, c_message *message)
// {
// 	char *channel_name = message->params[0];
	
// 	c_channel *searcher = channel_init();
// 	searcher->channel_name = channel_name;
// 	c_channel *ret_channel = NULL;
// 	c_user *ret_user = NULL;
// 	if((ret_channel = server_seek_channel(server, searcher)) == NULL){
// 		char *repl = malloc(600);
// 		sprintf(repl, ":%s %s %s %s :No such channel\r\n", 
// 			server->hostname, ERR_NOSUCHCHANNEL, user->nick, channel_name);
// 		send_message(server, user, repl);
// 	}
// 	while(!strcmp(user->channels[i], channel_name))
// 		i++;
// 	if(user->)
// }

// void mode_handler(c_server *server, c_user *user, c_message *message)
// {
// 	if(message->params[0][0] == '#'){
// 		channel_mode_handler(server, user, message);
// 		return;
// 	}

// }