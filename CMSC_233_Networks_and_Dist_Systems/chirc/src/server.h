#ifndef SERVER
#define SERVER

server *server_init();

int seek_nick(const void *dummy_user, const void *key);

user *user_list_seek_nick(server *curr_server, user *key);

int seek_username(const void *dummy_user, const void *key);

user *user_list_seek_username(server *curr_server, user *key);

void user_list_add(server *curr_server, user *curr_user);

void server_delete_user(server *curr_server, user *curr_user);

#endif