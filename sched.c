#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*----------------------------------*/
/* TYPE DEFINITIONS */
/*----------------------------------*/

struct time {
    int32_t year;
    int32_t month;
    int32_t day;
    int32_t hour;
    int32_t minute;
    int32_t second;
};
typedef struct time time;

/*----------------------------------*/
/* FUNCTION DECLARATIONS */
/*----------------------------------*/

time* time_init(int y, int mo, int d, int h, int mi, int s);
int32_t compare(time t1, time t2);


/*----------------------------------*/
/* FUNCTION IMPLEMENTATIONS */
/*----------------------------------*/

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

int32_t compare(time t1, time t2){
  int year1 = t1.year;
  int year2 = t2.year;
  int yeardiff = year1-year2;
  int month1 = t1.month;
  int month2 = t2.month;
  int mdiff = month1-month2;
  int day1 = t1.day;
  int day2 = t2.day;
  int ddiff = day1-day2;
  int hour1 = t1.hour;
  int hour2 = t2.hour;
  int hdiff = hour1-hour2;
  int min1 = t1.minute;
  int min2 = t2.minute;
  int mindiff = min1-min2;
  int s1 = t1.second;
  int s2 = t2.second;
  int secdiff = s1-s2;
  return 0;
}
