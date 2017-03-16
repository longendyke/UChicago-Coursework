#include "community.h"

Community::Community()
  : name(""), people(map<string,Person>()) {
}

Community::Community(string _name, map<string,Person> _people) 
  : name(_name), people(_people) {
}

string Community::get_name() {
	return name;
}

bool Community::set_name(string _name) {
	//TODO
	// set name
    return false;
}

bool Community::add_person(Person _person) {
    contact to_add(_person.get_username(), _person);
    
    return false;
}

// return the person object for a given username
Person Community::get_member(string username) {
    if (people.find(username) != people.end()) {
        return people[username];
    }
    else {
        return Person();
    }
}
    
list<string> Community::get_all_usernames() {
    list<string> usernames;
    // TODO
    // return all usernames of members as a list
    return usernames;
}

void Community::print_all_usernames() {
    // TODO
	// print all usernames of members to the console
}

list<Person> Community::find_member(string firstname) {
    list<Person> ret;
    //TODO
    // find users with a certain first name
    return ret;
}

list<Person> Community::find_member(int age_lb, int age_ub) {
  list<Person> ret;
  //TODO
  // find users within certain ages [age_lb, age_ub], both lower bound and upper bound shall be inclusive
  return ret;
}

bool Community::send_msg(list<string> usernames, string msg) {
	//TODO
	// send msg to a Person addressed by username
	// make sure the username is validated
	return false;
}

