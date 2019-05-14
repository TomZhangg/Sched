#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 =====================================================
                    TYPEDEFS
 =====================================================
 */

 typedef struct Array_element {
     struct Array_element *next;
     struct Array_element *prev;
     void *data;
 } array_element;

 typedef struct Array {
     int32_t length;
     array_element *head;
     array_element *tail;
     int32_t contains_struct;
 } array;



/*
  =====================================================
                  FUNCTION DECLARATIONS
  =====================================================
  */

array* arr_init();
array* arr_set_contains_struct(array *a);
array_element* arr_append(array *l, void *data);
void* arr_get(array *l, int index);
array_element* arr_set(array *l, void *data, int index);
int32_t arr_contains(array *l, void *data);
int32_t arr_length(array *l);



/*
 =====================================================
                    ARRAY FUNCTIONS
 =====================================================
 */

 //init array
 array* arr_init() {
     array* l = (array *) malloc(sizeof(array));
     l->length = 0;
     l->head = NULL;
     l->tail = NULL;
     l->contains_struct = 0;
     return l;
 }

// call this if the array contains a struct
array* arr_set_contains_struct(array *a) {
  a->contains_struct = 1;
  return a;
}

 //append element: can be any type
 array_element* arr_append(array *l, void *data) {
     array_element *le = (array_element*) malloc(sizeof(array_element));
     le->data = data;
     le->next = NULL;
     le->prev = l->tail;
     if(l->tail) {
         l->tail->next = le;
     }
     else { //empty array
         l->head = le;
     }
     l->tail = le;
     l->length += 1;

     return le;
 }

 //access element
 void* arr_get(array *l, int index) {
     if(index >= l->length) {
         return NULL;
     }
     array_element *curr = l->head;
     int count = 0;
     while(count < index) {
         curr = curr->next;
         count++;
     }
     return curr->data;
 }

 //set element
 array_element* arr_set(array *l, void *data, int index) {
     if(index >= l->length) {
         return NULL;
     }
     array_element *curr = l->head;
     int count = 0;
     while(count < index) {
         curr = curr->next;
         count++;
     }
     free(curr->data);
     curr->data = data;
     return curr;
 }

 int32_t arr_contains(array *l, void *data) {
   array_element *curr = l->head;
   while(curr != NULL) {
     if (
       (
         (
           data == curr->data ||
           strcmp(data, curr->data) == 0
         ) && !l->contains_struct
       ) 
     ) {
       return 1;
     }
     curr = curr->next;
   }
   return 0;

 }

 int32_t arr_length(array *l) {
     return l->length;
 }


