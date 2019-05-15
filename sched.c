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

 struct time {
     int32_t year;
     int32_t month;
     int32_t day;
     int32_t hour;
     int32_t minute;
     int32_t second;
 };
 typedef struct time time;

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
time* time_init(int y, int mo, int d, int h, int mi, int s);
int32_t compare(time t1, time t2);


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


 time* time_init(int y, int mo, int d, int h, int mi, int s) {
   time* r = malloc(sizeof(time));
   r->year = y;
   r->month = mo;
   r->day = d;
   r->hour = h;
   r->minute = mi;
   r->second = s;
   return r;
 }


 /*
  =====================================================
                     TIME FUNCTIONS
  =====================================================
  */

 int32_t time_compare(time *t1, time *t2){
   long year1 = (long)t1->year;
   long year2 = (long)t2->year;
   long yeardiff = year1-year2;
   long month1 = (long)t1->month;
   long month2 = (long)t2->month;
   long mdiff = month1-month2;
   long day1 = (long)t1->day;
   long day2 = (long)t2->day;
   long ddiff = day1-day2;
   long hour1 = (long)t1->hour;
   long hour2 = (long)t2->hour;
   long hdiff = hour1-hour2;
   long min1 = (long)t1->minute;
   long min2 = (long)t2->minute;
   long mindiff = min1-min2;
   long s1 = (long)t1->second;
   long s2 = (long)t2->second;
   long secdiff = s1-s2;
   long minsecs = (long)60;
   long hoursecs = (long)60*60;
   long daysecs = (long)24*60*60;
   long monthsecs = (long)30*24*60*60;
   long yearsecs = (long)365*24*60*60;
   long diff = secdiff + mindiff*minsecs + hdiff*hoursecs + ddiff*daysecs + mdiff*monthsecs +  yeardiff*yearsecs;
   if(diff < (long)0){
   return 0; 
   }
   else
   {
   return 1;
   }
 }

 int32_t time_equal(time *t1, time *t2){
   long year1 = (long)t1->year;
   long year2 = (long)t2->year;
   long yeardiff = year1-year2;
   long month1 = (long)t1->month;
   long month2 = (long)t2->month;
   long mdiff = month1-month2;
   long day1 = (long)t1->day;
   long day2 = (long)t2->day;
   long ddiff = day1-day2;
   long hour1 = (long)t1->hour;
   long hour2 = (long)t2->hour;
   long hdiff = hour1-hour2;
   long min1 = (long)t1->minute;
   long min2 = (long)t2->minute;
   long mindiff = min1-min2;
   long s1 = (long)t1->second;
   long s2 = (long)t2->second;
   long secdiff = s1-s2;
   long minsecs = (long)60;
   long hoursecs = (long)60*60;
   long daysecs = (long)24*60*60;
   long monthsecs = (long)30*24*60*60;
   long yearsecs = (long)365*24*60*60;
   long diff = secdiff + mindiff*minsecs + hdiff*hoursecs + ddiff*daysecs + mdiff*monthsecs +  yeardiff*yearsecs;
   if(diff == (long)0){
   return 1; 
   }
   else
   {
   return 0;
   }
 }


void print_time(time* t1){
  printf("<%d-%d-%dT%d:%d:%d>\n",t1->year,t1->month,t1->day,t1->hour,t1->minute,t1->second);
}


