/* srpbuffer.h */

extern Scheme_Object *numericStructType;
extern Scheme_Object *dateStructType;
extern Scheme_Object *timeStructType;
extern Scheme_Object *timeStampStructType;
extern Scheme_Object *guidStructType;
extern Scheme_Object *yearIntervalStructType;
extern Scheme_Object *monthIntervalStructType;
extern Scheme_Object *dayIntervalStructType;
extern Scheme_Object *hourIntervalStructType;
extern Scheme_Object *minuteIntervalStructType;
extern Scheme_Object *secondIntervalStructType;
extern Scheme_Object *yearToMonthIntervalStructType;
extern Scheme_Object *dayToHourIntervalStructType;
extern Scheme_Object *dayToMinuteIntervalStructType;
extern Scheme_Object *dayToSecondIntervalStructType;
extern Scheme_Object *hourToMinuteIntervalStructType;
extern Scheme_Object *hourToSecondIntervalStructType;
extern Scheme_Object *minuteToSecondIntervalStructType;

#define WHOLE_BUFFER (-1)

#if (ODBCVER >= 0x0300)
SQLUINTEGER *getIntervalYear(SQL_INTERVAL_STRUCT *);
SQLUINTEGER *getIntervalMonth(SQL_INTERVAL_STRUCT *);
SQLUINTEGER *getIntervalDay(SQL_INTERVAL_STRUCT *);
SQLUINTEGER *getIntervalHour(SQL_INTERVAL_STRUCT *);
SQLUINTEGER *getIntervalMinute(SQL_INTERVAL_STRUCT *);
SQLUINTEGER *getIntervalSecond(SQL_INTERVAL_STRUCT *);
#endif

