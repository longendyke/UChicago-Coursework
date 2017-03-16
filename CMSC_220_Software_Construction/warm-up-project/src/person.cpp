#include "person.h"
#include <ctype.h>

bool str_isalpha(const string str){
    for(int i = 0; i < (int)str.size(); i++)
    	if((isalpha(str[i]) == 0) || (str[i] == ' '))
    		return false;
    return true;
}

bool str_isalnum(const string s)
{
    std::string::const_iterator it = s.begin();
    while (it != s.end() && std::isalnum(*it)) ++it;
    return !s.empty() && it == s.end();
}

Person::Person() 
  : username(""), firstname(""), lastname(""), gender(""), age(0), tagline("") {
}

Person::Person(string _username, string _firstname, string _lastname, 
               string _gender, int _age, string _tagline) 
  :  username(_username), firstname(_firstname), lastname(_lastname),
    gender(_gender), age(_age), tagline(_tagline) {
       if (!this->set_info(_username, _firstname, _lastname, _age, _tagline)) {
        new (this) Person();
   }
   else { 
    this->set_gender(_gender);
}
}

string Person::get_username() {
    return username;
}
string Person::get_firstname() {
    return firstname;
}
string Person::get_lastname() {
    return lastname;
}
string Person::get_gender(){
    return gender;
}
int Person::get_age() {
    return age;
}
string Person::get_tagline() {
    return tagline;
}
string Person::get_info() {
	string ret = "";
    return ret;
}

bool Person::set_username(string _username) {
    if ((_username.length() > 64) ||
        (str_isalnum(_username) == false) ||
        (_username == "") || 
        (isdigit(_username.at(0))))

    { 
        return false;
    } 
    else { 
        username = _username;
        return true;

    }

}


bool Person::set_firstname(string _firstname) {
	if ((_firstname.length() < 65) && 
        (str_isalpha(_firstname))) {
        firstname = _firstname;
        return true;
    }
    else {
        return false;
    }
}


bool Person::set_lastname(string _lastname) {
	if ((_lastname.length() < 65) && 
        (str_isalpha(_lastname))) {
        lastname = _lastname;
        return true;
    }
    else {
        return false;
    }
}

bool Person::set_gender(string _gender){
    if ((_gender == "m") || 
        (_gender == "f"))
    {
        gender = _gender; 
        return true;
    }
    else{
    return false;
}
}

bool Person::set_age(int _age) {
    if ( _age >= 0 && _age <128) {
        age = _age;
        return true;
    }
    else {
        return false;
    }
}
bool Person::set_tagline(string _tagline) {
    if (_tagline.length() <= 512) {
        tagline = _tagline;
        return true;
    }
    else 
    {
        return false;
    }
}


bool Person::set_info(string _username, string _firstname, string _lastname,
                      int _age, string _tagline) {
    Person *current = this;
    string oldusername = current->get_username().c_str();
    string oldfirstname = current->get_firstname().c_str(); 
    string oldlastname = current->get_lastname().c_str(); 
    int oldage = current->get_age(); 
    string oldtagline = current->get_tagline().c_str();

   if((set_username(_username)) && 
    (set_firstname(_firstname)) && 
    (set_lastname(_lastname)) && 
    (set_age(_age)) && 
    (set_tagline(_tagline)) ) 
   { 
    return true; 
    }
    else 
    {
    set_username(oldusername);
    set_firstname(oldfirstname); 
    set_lastname(oldlastname); 
    set_age(oldage); 
    set_tagline(oldtagline);
	return false;
    }
}



void Person::send_msg(Person &recipient, string msg) {
    recipient.inbox.push(msg);
    }

void Person::get_msg(string msg) {
	inbox.push(msg);
    }

bool Person::read_msg() {
	if (inbox.empty()) {
        cout << "Your Inbox is Empty. There are no messages to be read\n";
        return false;
    }
    cout << inbox.front().c_str();
    inbox.pop();
    return true;
}

