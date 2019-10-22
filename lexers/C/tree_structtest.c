#include "uthash.h"

struct my_struct {
    int id;            /* we'll use this field as the key */
    char name[10];             
    UT_hash_handle hh; /* makes this structure hashable */
};

struct my_struct *users = NULL;

void add_user(struct my_struct *s) {
    HASH_ADD_INT( users, id, s );    
}

struct my_struct *find_user(int user_id) {
    struct my_struct *s;

    HASH_FIND_INT( users, &user_id, s );  
    return s;
}

void delete_user(struct my_struct *user) {
    HASH_DEL( users, user);  
}
