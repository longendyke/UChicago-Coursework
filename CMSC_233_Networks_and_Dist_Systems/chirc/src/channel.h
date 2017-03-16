#ifndef CHANNEL
#define CHANNEL

c_channel *channel_init();

int seek_channel_name(const void *channel, const void *key);

c_channel *server_seek_channel(c_server *server, c_channel *key);

c_user *channel_user_list_seek_nick(c_channel *channel, c_user *key);

void channel_user_list_add(c_channel *channel, c_user *user);

void server_channel_list_add(c_server *server, c_channel *channel);

void channel_send_all(c_server *server,c_channel *channel, char *message);

void channel_send_allbutme(c_server *server, c_channel *channel, c_user *user, char *message);

void server_delete_channel(c_server *server, c_channel *channel);

void channel_delete_user(c_server *server, c_channel *channel, c_user *user);

#endif