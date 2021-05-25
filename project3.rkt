#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require typed/racket/date)
;; Andrew DeVoe
;; CMSC 151 - Project 2

;; Definitions —————————————————————————————————————————————————————————————————

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct CalFormat
  ([cell-size : Integer]
   [title-bar-bg : Image-Color]
   [title-bar-font : Image-Color]
   [title-bar-height : Integer]
   [day-label-bg : Image-Color]
   [day-label-font : Image-Color]
   [day-label-height : Integer]
   [cell-bg : Image-Color]
   [cell-font : Image-Color]))

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-struct Date
  ([month : Integer] ;; 1 for January, ..., 12 for December
   [day : Integer]
   [year : Integer]))

(define-struct Day-Of-Week
  ([num : Integer]))

(define-struct Span
  ([start : Time]
   [end : Time]))

(define-struct Event
  ([date : Date]
   [time : (U 'all-day Time Span)]
   [description : String]))

(define-type EventTree
  (U 'Empty EventNode))

(define-struct EventNode
  ([date : Date]
   [events : (Listof Event)] ;; maintain this list in ascending order
   [lsub : EventTree]
   [rsub : EventTree]))

(define-struct CalWorld3
  ([mode : (U 'calendar 'help 'entry)]
   [entry-mode : (U 'start 'end 'description)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]
   [notepad : String]
   [opt-start : (Optional Time)]
   [opt-end : (Optional Time)]
   [events : EventTree]))

;; CalFormats ——————————————————————————————————————————————————————————————————

(: fmt0 CalFormat)
(define fmt0
  (CalFormat 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30
             'lightyellow 'black))

;; American Flag Colors
(: fmt1 CalFormat)
(define fmt1
  (CalFormat 60
             (color 60 59 110) 'white 60
             (color 178 34 52) 'white 40
             'white (color 60 59 110)))

(: fmt2 CalFormat)
(define fmt2
  (CalFormat 20
             'gold 'black 20
             'black 'gold 10
             'gold 'black))

(: fmt3 CalFormat)
(define fmt3
  (CalFormat 100
             'gold 'black 30
             'black 'gold 50
             'gold 'black))

;; UChiago Colors
(: fmt4 CalFormat)
(define fmt4
  (CalFormat 70
             (color 117 31 28) 'white 60
             (color 127 124 119) 'white 50
             'white (color 117 31 28)))

;; Events ——————————————————————————————————————————————————————————————————————

(: event1 Event)
(define event1
  (Event (Date 3 9 2021)
         (Span (Time 12 12 12) (Time 12 12 12))
         "A"))

(: event2 Event)
(define event2
  (Event (Date 3 9 2021)
         (Span (Time 13 12 12) (Time 12 12 12))
         "B"))

(: event3 Event)
(define event3
  (Event (Date 3 9 2021)
         'all-day
         "C"))

(: event4 Event)
(define event4
  (Event (Date 3 9 2021)
         (Time 10 0 0)
         "D"))

(: event5 Event)
(define event5
  (Event (Date 3 9 2021)
         (Time 14 12 12)
         "E"))

(: event6 Event)
(define event6
  (Event (Date 3 8 2021)
         (Time 14 12 12)
         "F"))

(: event7 Event)
(define event7
  (Event (Date 3 8 2021)
         (Time 11 12 12)
         "G"))

(: event8 Event)
(define event8
  (Event (Date 3 8 2021)
         (Time 14 15 12)
         "H"))

(: event9 Event)
(define event9
  (Event (Date 3 11 2021)
         (Time 14 12 12)
         "I"))

(: event10 Event)
(define event10
  (Event (Date 3 11 2021)
         (Time 19 12 12)
         "J"))

(: tree1 EventTree)
(define tree1
  (EventNode (Date 3 9 2021) (list event3 event5)
             (EventNode (Date 3 8 2021) '() 'Empty 'Empty)
             (EventNode (Date 3 11 2021) (list event9) 'Empty 'Empty)))

;; Lab 2 Code ——————————————————————————————————————————————————————————————————
;; Worked on lab with Jake Schroeder

;; Functions

(: leap? (-> Integer Boolean))
;; Takes in a year and determines if it is a leap year
(define (leap? yr)
  (cond
    [(and (= (modulo yr 4) 0)
          (not (= (modulo yr 100) 0))) #t]
    [(= (modulo yr 400) 0) #t]
    [else #f]))

(check-expect (leap? 2000) #t)
(check-expect (leap? 2021) #f)
(check-expect (leap? 2024) #t)
;; (check-expect (leap? -2020) GIGO)

(: days-in-month (-> Integer Integer Integer))
;; Takes ina month and year and returns the number of days in the month
;; that year.
(define (days-in-month mth yr)
  (cond
    [(leap? yr) (match mth
                  [1 31]
                  [2 29]
                  [3 31]
                  [4 30]
                  [5 31]
                  [6 30]
                  [7 31]
                  [8 31]
                  [9 30]
                  [10 31]
                  [11 30]
                  [12 31])]
    [else (match mth
            [1 31]
            [2 28]
            [3 31]
            [4 30]
            [5 31]
            [6 30]
            [7 31]
            [8 31]
            [9 30]
            [10 31]
            [11 30]
            [12 31])]))

(check-expect (days-in-month 1 2021) 31)
(check-expect (days-in-month 2 2024) 29)
(check-expect (days-in-month 2 2021) 28)
(check-expect (days-in-month 3 2020) 31)
(check-expect (days-in-month 4 2021) 30)
(check-expect (days-in-month 5 2021) 31)
(check-expect (days-in-month 6 2020) 30)
(check-expect (days-in-month 7 2021) 31)
(check-expect (days-in-month 8 2020) 31)
(check-expect (days-in-month 9 2020) 30)
(check-expect (days-in-month 10 2021) 31)
(check-expect (days-in-month 11 2019) 30)
(check-expect (days-in-month 12 2021) 31)
;; (check-expect (days-in-month 16 -2021) GIGO)

(: date=? (-> Date Date Boolean))
;; Checks to see if two dates are exactly the same
(define (date=? d1 d2)
  (match* (d1 d2)
    [((Date mth1 day1 yr1) (Date mth2 day2 yr2))
     (and
      (= mth1 mth2)
      (= day1 day2)
      (= yr1 yr2))]))

(check-expect (date=? (make-Date 11 11 1918) (make-Date 11 11 1918)) #t)
(check-expect (date=? (make-Date 11 11 1918) (make-Date 11 11 2018)) #f)
(check-expect (date=? (make-Date 5 8 1945) (make-Date 8 15 1945)) #f)

(: date<? (-> Date Date Boolean))
;; Checks to see if the first date input comes before the second date input
(define (date<? d1 d2)
  (match* (d1 d2)
    [((Date mth1 day1 yr1) (Date mth2 day2 yr2))
     (cond
      [(< yr1 yr2) #t]
      [(and (= yr1 yr2)
            (< mth1 mth2)) #t]
      [(and (= yr1 yr2)
            (= mth1 mth2)
            (< day1 day2)) #t]
      [else #f])]))

(check-expect (date<? (make-Date 11 11 1918) (make-Date 11 11 1918)) #f)
(check-expect (date<? (make-Date 12 25 2018) (make-Date 11 11 1918)) #f)
(check-expect (date<? (make-Date 5 16 1945) (make-Date 8 15 1945)) #t)
(check-expect (date<? (make-Date 5 14 1945) (make-Date 5 15 1945)) #t)

(: days-after (-> Day-Of-Week Integer Day-Of-Week))
;; Takes the day of the week and adds the integer input to the day of the week
;; to get the new day of the week s.t. day x is in [0,6]
(define (days-after day n)
  (cond
    [(> (+ (modulo n 7) (Day-Of-Week-num day)) 6)
     (make-Day-Of-Week (+ (- (modulo n 7) (- 7 (Day-Of-Week-num day))) 0))]
    [else (make-Day-Of-Week (+ (Day-Of-Week-num day) (modulo n 7)))]))

(check-expect (days-after (make-Day-Of-Week 6) 13) (Day-Of-Week 5))
(check-expect (days-after (make-Day-Of-Week 3) 4) (Day-Of-Week 0))
(check-expect (days-after (make-Day-Of-Week 4) 28) (Day-Of-Week 4))
(check-expect (days-after (make-Day-Of-Week 5) 128) (Day-Of-Week 0))
(check-expect (days-after (make-Day-Of-Week 2) 10) (Day-Of-Week 5))
;; (check-expect (days-after (make-Day-Of-Week 17) 10) GIGO)

(: doomsday-in-month (-> Integer Integer Integer))
;; Takes in a month and year and outputs the day of doomsday in that month
(define (doomsday-in-month mth yr)
  (cond
    [(leap? yr) (match mth
                  [1 32]
                  [2 29]
                  [3 0]
                  [4 4]
                  [5 9]
                  [6 6]
                  [7 11]
                  [8 8]
                  [9 5]
                  [10 10]
                  [11 7]
                  [12 12])]
    [else (match mth
                  [1 31]
                  [2 28]
                  [3 0]
                  [4 4]
                  [5 9]
                  [6 6]
                  [7 11]
                  [8 8]
                  [9 5]
                  [10 10]
                  [11 7]
                  [12 12])]))

(check-expect (doomsday-in-month 1 2021) 31)
(check-expect (doomsday-in-month 1 2020) 32)
(check-expect (doomsday-in-month 2 2021) 28)
(check-expect (doomsday-in-month 2 2020) 29)
(check-expect (doomsday-in-month 3 2021) 0)
(check-expect (doomsday-in-month 4 2021) 4)
(check-expect (doomsday-in-month 5 2021) 9)
(check-expect (doomsday-in-month 6 2021) 6)
(check-expect (doomsday-in-month 7 2021) 11)
(check-expect (doomsday-in-month 8 2021) 8)
(check-expect (doomsday-in-month 9 2021) 5)
(check-expect (doomsday-in-month 10 2021) 10)
(check-expect (doomsday-in-month 11 2021) 7)
(check-expect (doomsday-in-month 12 2021) 12)
;; (check-expect (doomsday-in-month 17 657) GIGO)

(: find-day-of-week (-> Date Day-Of-Week))
;; Finds the day of the week of doomsday in a given year
(define (find-day-of-week date)
  (local
    {(define r (modulo (Date-year date) 100))
     (define cent (make-Day-Of-Week
                   (cond
                     [(< (modulo (- (Date-year date) 1900) 400) 100) 3]
                     [(< (modulo (- (Date-year date) 2000) 400) 100) 2]
                     [(< (modulo (- (Date-year date) 2100) 400) 100) 0]
                     [else 5])))}
    (days-after cent (+ r (quotient r 4)))))

(check-expect (find-day-of-week (make-Date 1 1 2020)) (Day-Of-Week 6))
(check-expect (find-day-of-week (make-Date 1 1 2472)) (Day-Of-Week 1))
(check-expect (find-day-of-week (make-Date 1 1 2917)) (Day-Of-Week 0))
(check-expect (find-day-of-week (make-Date 1 1 1936)) (Day-Of-Week 6))
(check-expect (find-day-of-week (make-Date 1 1 2701)) (Day-Of-Week 4))
(check-expect (find-day-of-week (make-Date 1 1 2268)) (Day-Of-Week 6))
(check-expect (find-day-of-week (make-Date 1 1 2138)) (Day-Of-Week 5))
(check-expect (find-day-of-week (make-Date 1 1 5681)) (Day-Of-Week 5))
;; (check-expect (find-day-of-week (make-Date -1 18169)) GIGO)

;; Calculations ————————————————————————————————————————————————————————————————

(: get-month-name : Integer -> String)
;; Converts the number of a month to its name
;; Starting with January = 1 and ending on December = 12
(define (get-month-name num)
  (match num
    [1 "January"]
    [2 "February"]
    [3 "March"]
    [4 "April"]
    [5 "May"]
    [6 "June"]
    [7 "July"]
    [8 "August"]
    [9 "September"]
    [10 "October"]
    [11 "November"]
    [12 "December"]))

(check-expect (get-month-name 6) "June")
(check-expect (get-month-name 1) "January")
(check-expect (get-month-name 9) "September")

(: get-day-of-week-name : Integer -> String)
;; Converts the number of a day of the week to its name
;; Starting with Sunday = 0 and Saturday = 6
(define (get-day-of-week-name num)
  (match num
    [0 "Sunday"]
    [1 "Monday"]
    [2 "Tuesday"]
    [3 "Wednesday"]
    [4 "Thursday"]
    [5 "Friday"]
    [6 "Saturday"]))

(check-expect (get-day-of-week-name 0) "Sunday")
(check-expect (get-day-of-week-name 4) "Thursday")
(check-expect (get-day-of-week-name 6) "Saturday")

(: find-first-day-of-month : Date -> Day-Of-Week)
;; Finds what day of the week a month starts on for a given month in a
;; given year
(define (find-first-day-of-month day)
  (local
    {(: doomsday-for-year Day-Of-Week) ;; saves the day of the week for the year
     (define doomsday-for-year (find-day-of-week day))}
    (match day
      [(Date m d y)
       (match m
         [1 (days-after doomsday-for-year (- 1 (+ (days-in-month 1 y)
                                                  (days-in-month 2 y))))]
         [2 (days-after doomsday-for-year (- 1 (days-in-month 2 y)))]
         [3 (days-after doomsday-for-year 1)]
         [4 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)))]
         [5 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                               (days-in-month 4 y)))]
         [6 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                               (days-in-month 4 y)
                                               (days-in-month 5 y)))]
         [7 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                               (days-in-month 4 y)
                                               (days-in-month 5 y)
                                               (days-in-month 6 y)))]
         [8 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                               (days-in-month 4 y)
                                               (days-in-month 5 y)
                                               (days-in-month 6 y)
                                               (days-in-month 7 y)))]
         [9 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                               (days-in-month 4 y)
                                               (days-in-month 5 y)
                                               (days-in-month 6 y)
                                               (days-in-month 7 y)
                                               (days-in-month 8 y)))]
         [10 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                                (days-in-month 4 y)
                                                (days-in-month 5 y)
                                                (days-in-month 6 y)
                                                (days-in-month 7 y)
                                                (days-in-month 8 y)
                                                (days-in-month 9 y)))]
         [11 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                                (days-in-month 4 y)
                                                (days-in-month 5 y)
                                                (days-in-month 6 y)
                                                (days-in-month 7 y)
                                                (days-in-month 8 y)
                                                (days-in-month 9 y)
                                                (days-in-month 10 y)))]
         [12 (days-after doomsday-for-year (+ 1 (days-in-month 3 y)
                                                (days-in-month 4 y)
                                                (days-in-month 5 y)
                                                (days-in-month 6 y)
                                                (days-in-month 7 y)
                                                (days-in-month 8 y)
                                                (days-in-month 9 y)
                                                (days-in-month 10 y)
                                                (days-in-month 11 y)))])])))

(check-expect (find-first-day-of-month (Date 1 1 2021)) (Day-Of-Week 5))
(check-expect (find-first-day-of-month (Date 3 1 2020)) (Day-Of-Week 0))
(check-expect (find-first-day-of-month (Date 6 1 2020)) (Day-Of-Week 1))
(check-expect (find-first-day-of-month (Date 5 1 2021)) (Day-Of-Week 6))
(check-expect (find-first-day-of-month (Date 5 1 2002)) (Day-Of-Week 3))
(check-expect (find-first-day-of-month (Date 11 1 2035)) (Day-Of-Week 4))
(check-expect (find-first-day-of-month (Date 5 1 1945)) (Day-Of-Week 2))

(: yesterday : Date -> Date)
;; The date for the day before the currently marked day
(define (yesterday date)
  (match date
    [(Date m d y)
     (if (= d 1)
         (if (= m 1)
             (Date 12 31 (- y 1))
             (Date (- m 1) (days-in-month (- m 1) y) y))
         (Date m (- d 1) y))]))

(check-expect (yesterday (Date 3 1 2020)) (Date 2 29 2020))
(check-expect (yesterday (Date 1 1 2021)) (Date 12 31 2020))
(check-expect (yesterday (Date 7 16 2020)) (Date 7 15 2020))
(check-expect (yesterday (Date 3 1 2021)) (Date 2 28 2021))
(check-expect (yesterday (Date 8 5 2020)) (Date 8 4 2020))

(: tomorrow : Date -> Date)
;; The date for the day after the currently marked day
(define (tomorrow date)
  (match date
    [(Date m d y)
     (if (= d (days-in-month m y))
         (if (= m 12)
             (Date 1 1 (+ y 1))
             (Date (+ m 1) 1 y))
         (Date m (+ d 1) y))]))

(check-expect (tomorrow (Date 2 28 2020)) (Date 2 29 2020))
(check-expect (tomorrow (Date 2 29 2020)) (Date 3 1 2020))
(check-expect (tomorrow (Date 12 31 2020)) (Date 1 1 2021))
(check-expect (tomorrow (Date 6 17 2020)) (Date 6 18 2020))
(check-expect (tomorrow (Date 9 30 2020)) (Date 10 1 2020))
(check-expect (tomorrow (Date 1 5 2020)) (Date 1 6 2020))

(: read-date-now : -> Date)
;; Reads the current date and makes a Date
(define (read-date-now)
  (Date (date-month (current-date))
        (date-day (current-date))
        (date-year (current-date))))

(: make-date-string : Date -> String) ;;days after find first day of month
;; Makes a string of a date given a date
(define (make-date-string date)
  (match date
    [(Date m d y) (string-append
                   (get-day-of-week-name
                    (Day-Of-Week-num
                     (days-after (find-first-day-of-month date) (- d 1)))) ", "
                                 (get-month-name m) " "
                                 (number->string d) ", "
                                 (number->string y))]))

(check-expect (make-date-string (Date 3 3 2021))
              "Wednesday, March 3, 2021")
(check-expect (make-date-string (Date 5 20 2002))
              "Monday, May 20, 2002")
(check-expect (make-date-string (Date 10 25 2007))
              "Thursday, October 25, 2007")
(check-expect (make-date-string (Date 12 5 2020))
              "Saturday, December 5, 2020")

(: read-time-now : -> Time)
;; Reads the current time and makes a Time
(define (read-time-now)
  (Time (date-hour (current-date))
        (date-minute (current-date))
        (date-second (current-date))))

(: make-time-string : Time -> String)
;; Makes a string out of a given time
(define (make-time-string time)
  (match time
    [(Time h m s)
     (string-append (cond
                      [(= h 0) "12"]
                      [(< h 13) (number->string h)]
                      [else (number->string (- h 12))])
                    ":"
                    (if (< m 10)
                        (string-append "0" (number->string m))
                        (number->string m))
                    ":"
                    (if (< s 10)
                        (string-append "0" (number->string s))
                        (number->string s))
                    " "
                    (if (< h 12)
                        "am"
                        "pm"))]))

(check-expect (make-time-string (Time 7 8 0)) "7:08:00 am")
(check-expect (make-time-string (Time 19 27 18)) "7:27:18 pm")
(check-expect (make-time-string (Time 0 47 12)) "12:47:12 am")
(check-expect (make-time-string (Time 13 43 56)) "1:43:56 pm")
(check-expect (make-time-string (Time 22 35 48)) "10:35:48 pm")

(: no-seconds-time-string : Time -> String)
;; Makes a string out of a given time
(define (no-seconds-time-string time)
  (match time
    [(Time h m s)
     (string-append (cond
                      [(= h 0) "12"]
                      [(< h 13) (number->string h)]
                      [else (number->string (- h 12))])
                    ":"
                    (if (< m 10)
                        (string-append "0" (number->string m))
                        (number->string m))
                    " "
                    (if (< h 12)
                        "am"
                        "pm"))]))

(check-expect (no-seconds-time-string (Time 7 8 0)) "7:08 am")
(check-expect (no-seconds-time-string (Time 19 27 18)) "7:27 pm")
(check-expect (no-seconds-time-string (Time 0 47 12)) "12:47 am")
(check-expect (no-seconds-time-string (Time 13 43 56)) "1:43 pm")
(check-expect (no-seconds-time-string (Time 22 35 48)) "10:35 pm")

;; Event Calculations ——————————————————————————————————————————————————————————

(: is-valid-time? : (Listof Char) -> Boolean)
;; Determines if the list of characters makes a valid time string
(define (is-valid-time? lst)
  (match lst
    [(cons a (cons #\: (cons b (cons c (cons d (cons e '()))))))
     (and (char-numeric? a) (char-numeric? b) (char-numeric? c)
          (or (char=? d #\a) (char=? d #\p))
          (char=? e #\m))]
    [(cons a (cons b (cons #\: (cons c (cons d (cons e (cons f '())))))))
     (and (char-numeric? a) (char-numeric? b)
          (char-numeric? c) (char-numeric? d)
          (or (char=? e #\a) (char=? e #\p))
          (char=? f #\m))]
    [_ #f]))

(check-expect (is-valid-time? (list #\7 #\: #\1 #\7 #\a #\m)) #t)
(check-expect (is-valid-time? (list #\1 #\2 #\: #\1 #\7 #\p #\m)) #t)
(check-expect (is-valid-time? (list #\7 #\6 #\1 #\7 #\a #\m)) #f)
(check-expect (is-valid-time? (list #\a #\v #\1 #\a #\a #\m)) #f)

(: charlist : (Listof Char) -> (Listof String))
;; Makes a list of the strings of the hours, minutes, and am/pm given
;; a valid time string
(define (charlist lst)
  (match lst
    ['() '()]
    [(cons head1 (cons head2 tail))
     (cond
       [(char=? head1 #\:) (charlist (cons head2 tail))]
       [(and (char-numeric? head1) (char-numeric? head2))
        (cons (string head1 head2) (charlist tail))]
       [(and (char-numeric? head1) (char=? head2 #\:))
        (cons (string head1) (charlist tail))]
       [(and (char=? head1 #\a) (char=? head2 #\m))
        (cons (string head1 head2) (charlist tail))]
       [(and (char=? head1 #\p) (char=? head2 #\m))
        (cons (string head1 head2) (charlist tail))]
       [else '()])]))

(check-expect (charlist (list #\7 #\: #\1 #\7 #\a #\m))
              (list "7" "17" "am"))
(check-expect (charlist (list #\1 #\2 #\: #\1 #\7 #\a #\m))
              (list "12" "17" "am"))
(check-expect (charlist (list #\7 #\: #\1 #\7 #\p #\m))
              (list "7" "17" "pm"))
(check-expect (charlist (list #\1 #\2 #\: #\1 #\7 #\p #\m))
              (list "12" "17" "pm"))

(: string->time : String -> (Optional Time))
;; convert a string like "11:25am" to a Time struct, if possible
;; ex: (string->time "10:00am") -> (Some (Time 10 0 0))
;; ex: (string->time "4:30pm")  -> (Some (Time 16 30 0))
;; ex: (string->time "abcde")   -> 'None
(define (string->time str)
  (local
    {(: charlst (Listof Char))
     (define charlst (string->list str))
     (: get-time-list : (Listof String) -> (Listof Integer))
     ;; Makes a list of integers from the list of strings of a time
     (define (get-time-list lst1)
       (local
         {(: hour Integer)
          (define hour (cast (string->number (list-ref lst1 0)) Integer))
          (: minute Integer)
          (define minute (cast (string->number (list-ref lst1 1)) Integer))
          (: am/pm String)
          (define am/pm (list-ref lst1 2))}
       (cond
         [(and (= hour 12)
               (string=? am/pm "am"))
          (list 0 minute 0)]
         [(and (>= hour 1)
               (< hour 12)
               (string=? am/pm "pm"))
          (list (+ hour 12) minute 0)]
         [else (list hour minute 0)])))
     (: get-time : (Listof Integer) -> Time)
     ;; Makes a time from a list of integers
     (define (get-time lst2)
       (Time (list-ref lst2 0) (list-ref lst2 1) (list-ref lst2 2)))}
    (if (is-valid-time? charlst)
        (Some (get-time (get-time-list (charlist charlst))))
        'None)))

(check-expect (string->time "12:03am") (Some (Time 0 3 0)))
(check-expect (string->time "8:26am") (Some (Time 8 26 0)))
(check-expect (string->time "11:59am") (Some (Time 11 59 0)))
(check-expect (string->time "12:03pm") (Some (Time 12 3 0)))
(check-expect (string->time "1:18pm") (Some (Time 13 18 0)))
(check-expect (string->time "7:46pm") (Some (Time 19 46 0)))
(check-expect (string->time "11:23pm") (Some (Time 23 23 0)))
(check-expect (string->time "12aa3am") 'None)
(check-expect (string->time "1haida") 'None)

(: time<? : Time Time -> Boolean)
;; Determines if a time comes before another time
(define (time<? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1) (Time h2 m2 s2))
     (cond
       [(< h1 h2) #t]
       [(and (= h1 h2) (< m1 m2)) #t]
       [(and (= h1 h2) (= m1 m2) (< s1 s2)) #t]
       [else #f])]))

(check-expect (time<? (Time 12 12 12) (Time 13 12 12)) #t)
(check-expect (time<? (Time 12 12 12) (Time 12 13 12)) #t)
(check-expect (time<? (Time 12 12 12) (Time 12 12 13)) #t)
(check-expect (time<? (Time 13 12 12) (Time 12 12 12)) #f)
(check-expect (time<? (Time 12 13 12) (Time 12 12 12)) #f)
(check-expect (time<? (Time 12 12 13) (Time 12 12 12)) #f)

(: time=? : Time Time -> Boolean)
;; Determines if a time is the same as another time
(define (time=? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1) (Time h2 m2 s2))
     (if (and (= h1 h2) (= m1 m2) (= s1 s2))
         #t
         #f)]))

(check-expect (time=? (Time 12 12 12) (Time 13 12 12)) #f)
(check-expect (time=? (Time 12 12 12) (Time 12 13 12)) #f)
(check-expect (time=? (Time 12 12 12) (Time 12 12 13)) #f)
(check-expect (time=? (Time 13 12 12) (Time 12 12 12)) #f)
(check-expect (time=? (Time 12 13 12) (Time 12 12 12)) #f)
(check-expect (time=? (Time 12 12 13) (Time 12 12 12)) #f)
(check-expect (time=? (Time 12 12 12) (Time 12 12 12)) #t)

(: event-group : Event -> Integer)
;; Gives a number to an event based on its ranking order
(define (event-group e)
  (match e
    [(Event _ 'all-day _) 1]
    [(Event _ _ _) 2]))

(check-expect (event-group event3) 1)
(check-expect (event-group event1) 2)
(check-expect (event-group event4) 2)

(: compare-spans : Event Event -> Boolean)
;; Determines which span even comes first
(define (compare-spans e1 e2)
  (match* (e1 e2)
    [((Event _ (Span t1s t1e) des1)
      (Event _ (Span t2s t2e) des2))
     (local
       {(: start-equal Boolean)
        (define start-equal (time=? t1s t2s))}
     (cond
       [(time<? t1s t2s) #t]
       [(and start-equal (time<? t1e t2e)) #t]
       [(and start-equal (time=? t1e t2e) (string<? des1 des2)) #t]
       [else #f]))]))

(check-expect (compare-spans event1 event1) #f)
(check-expect (compare-spans event1 event2) #t)
(check-expect (compare-spans event2 event1) #f)

(: event<? : Event Event -> Boolean)
;; determine if the first event is "less than" the second according to
;; event order
(define (event<? e1 e2)
  (cond
    [(date=? (Event-date e1) (Event-date e2))
     (local
       {(: group1 Integer)
        (define group1 (event-group e1))
        (: group2 Integer)
        (define group2 (event-group e2))}
       (cond
         [(< group1 group2) #t]
         [(> group1 group2) #f]
         [(and (= group1 1) (= group2 1))
          (match* (e1 e2)
            [((Event _ _ des1) (Event _ _ des2))
             (string<? des1 des2)])]
         [else
          (match* (e1 e2)
            [((Event _ (Time h1 m1 s1) des1)
              (Event _ (Span (Time h2 m2 s2) _) _))
             (not (time<? (Time h2 m2 s2) (Time h1 m1 s1)))]
            [((Event _ (Span (Time h1 m1 s1) _) _)
              (Event _ (Time h2 m2 s2) des2))
             (time<? (Time h1 m1 s1) (Time h2 m2 s2))]
            [((Event _ (Time h1 m1 s1) des1) (Event _ (Time h2 m2 s2) des2))
             (if (time<? (Time h1 m1 s1) (Time h2 m2 s2))
                 #t
                 (if (time=? (Time h1 m1 s1) (Time h2 m2 s2))
                     (string<? des1 des2)
                     #f))]
            [(_ _) (compare-spans e1 e2)])]))]
    [(date<? (Event-date e1) (Event-date e2)) #t]
    [else #f]))

(check-expect (event<? event1 event1) #f)
(check-expect (event<? event1 event2) #t)
(check-expect (event<? event2 event1) #f)
(check-expect (event<? event3 event1) #t)
(check-expect (event<? event3 event5) #t)
(check-expect (event<? event4 event4) #f)
(check-expect (event<? event5 event4) #f)
(check-expect (event<? event1 event4) #f)
(check-expect (event<? event4 event1) #t)

(: insert-event : Event (Listof Event) -> (Listof Event))
;; Inserts the event into a list of events in ascending order
(define (insert-event e lst)
  (match lst
    ['() (list e)]
    [(cons head tail)
     (if (event<? e head)
         (cons e (cons head tail))
         (cons head (insert-event e tail)))]))

(check-expect
 (insert-event event1 (list event3 event4 event1 event2 event5))
 (list
  (Event (Date 3 9 2021) 'all-day "C")
  (Event (Date 3 9 2021) (Time 10 0 0) "D")
  (Event (Date 3 9 2021) (Span (Time 12 12 12) (Time 12 12 12)) "A")
  (Event (Date 3 9 2021) (Span (Time 12 12 12) (Time 12 12 12)) "A")
  (Event (Date 3 9 2021) (Span (Time 13 12 12) (Time 12 12 12)) "B")
  (Event (Date 3 9 2021) (Time 14 12 12) "E")))
(check-expect
 (insert-event event1 (list event3 event4 event2 event5))
 (list
  (Event (Date 3 9 2021) 'all-day "C")
  (Event (Date 3 9 2021) (Time 10 0 0) "D")
  (Event (Date 3 9 2021) (Span (Time 12 12 12) (Time 12 12 12)) "A")
  (Event (Date 3 9 2021) (Span (Time 13 12 12) (Time 12 12 12)) "B")
  (Event (Date 3 9 2021) (Time 14 12 12) "E")))
(check-expect
 (insert-event event3 (list event4 event1 event2 event5))
 (list
  (Event (Date 3 9 2021) 'all-day "C")
  (Event (Date 3 9 2021) (Time 10 0 0) "D")
  (Event (Date 3 9 2021) (Span (Time 12 12 12) (Time 12 12 12)) "A")
  (Event (Date 3 9 2021) (Span (Time 13 12 12) (Time 12 12 12)) "B")
  (Event (Date 3 9 2021) (Time 14 12 12) "E")))
(check-expect
 (insert-event event5 (list event3 event4 event1 event2))
 (list
  (Event (Date 3 9 2021) 'all-day "C")
  (Event (Date 3 9 2021) (Time 10 0 0) "D")
  (Event (Date 3 9 2021) (Span (Time 12 12 12) (Time 12 12 12)) "A")
  (Event (Date 3 9 2021) (Span (Time 13 12 12) (Time 12 12 12)) "B")
  (Event (Date 3 9 2021) (Time 14 12 12) "E")))

(: insert-event-tree : Event EventTree -> EventTree)
;; insert an event into an event tree, creating the node if needed
;; note: no duplicate check is necessary; insert the event no matter what
;; note: maintain the events list in ascending event order
(define (insert-event-tree e tree)
  (match tree
    ['Empty (EventNode (Event-date e) (list e) 'Empty 'Empty)]
    [(EventNode date events lsub rsub)
     (cond
       [(date<? (Event-date e) date)
        (EventNode date events (insert-event-tree e lsub) rsub)]
       [(date<? date (Event-date e))
        (EventNode date events lsub (insert-event-tree e rsub))]
       [else (EventNode date (insert-event e events) lsub rsub)])]))

(check-expect (insert-event-tree event2 tree1)
              (EventNode (Date 3 9 2021) (list event3 event2 event5)
             (EventNode (Date 3 8 2021) '() 'Empty 'Empty)
             (EventNode (Date 3 11 2021) (list event9) 'Empty 'Empty)))
(check-expect (insert-event-tree event10 tree1)
              (EventNode (Date 3 9 2021) (list event3 event5)
             (EventNode (Date 3 8 2021) '() 'Empty 'Empty)
             (EventNode (Date 3 11 2021) (list event9 event10) 'Empty 'Empty)))
              
(: insert-event-world : Event CalWorld3 -> CalWorld3)
;; insert an event into the event tree in a cal world
(define (insert-event-world e world)
  (match world
    [(CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e events)
     (CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e (insert-event-tree e events))]))

;; Not tested since it just calls insert-event-tree

(: retrieve-events : Date EventTree -> (Listof Event))
;; fetch the list of events for the given date
;; return empty list if that date is not present in the tree
(define (retrieve-events date tree)
  (match tree
    ['Empty '()]
    [(EventNode nodedate events lsub rsub)
     (cond
       [(date<? date nodedate) (retrieve-events date lsub)]
       [(date<? nodedate date) (retrieve-events date rsub)]
       [else events])]))

(check-expect (retrieve-events (Date 3 11 2021) tree1) (list event9))
(check-expect (retrieve-events (Date 3 9 2021) tree1) (list event3 event5))

;; Calendar Boxes Image ————————————————————————————————————————————————————————

(: week-boxes : CalFormat Date -> Image)
;; Makes the weeks on the calendar for the given month and year
(define (week-boxes format date)
  (match date
    [(Date m d y)
     (local
       {(: days-of-month (Listof Integer)) ;; Makes list of the days in a month
        (define days-of-month (build-list (days-in-month m y) add1))
        (: first-day-of-month Integer) ;; The first day of week of the month
        (define first-day-of-month (Day-Of-Week-num
                                    (find-first-day-of-month date)))
        (: make-weeks : Integer Boolean -> (Listof Image))
        ;; Makes a list of weeks of boxes in the month
        (define (make-weeks spot first?)
          (if (< spot (length days-of-month))
              (if (boolean=? first? #t)
                  (cons (foldr beside empty-image
                               (day-boxes format days-of-month
                                          first-day-of-month 0 0 m d))
                        (make-weeks (- 7 first-day-of-month) #f))
                  (cons (foldr beside empty-image
                               (day-boxes format days-of-month 0 spot 0 m d))
                        (make-weeks (+ spot 7) #f)))
              (cons empty-image '())))}
       (foldr above empty-image (make-weeks 0 #t)))]))

(: day-boxes : CalFormat (Listof Integer) Integer
               Integer Integer Integer Integer -> (Listof Image))
;; Makes a list of one week of boxes
(define (day-boxes format days-of-month num-until-start spot times m d)
  (match format
    [(CalFormat size title-bg title-color title-h
                day-bg day-color day-h cell-bg cell-color)
     ;; Makes sure it only makes one week
     (if (< times 7)
         ;; Puts empty boxes before first day of month
         (if (> num-until-start 0)
             (cons (overlay (square size 'outline 'black)
                            (square size 'solid cell-bg))
                   (day-boxes format days-of-month
                              (- num-until-start 1) spot (+ times 1) m d))
             ;; Puts numbered boxes for rest of week
             (if (< spot (length days-of-month))
                 (cons (overlay
                        (overlay/align/offset
                         "right" "top"
                         (square size 'outline 'black)
                        (- 0 (quotient size 12)) (quotient size 12)
                        (text (number->string
                               (list-ref days-of-month spot))
                              (cast (quotient size 4) Byte)
                              cell-color))
                        ;; Checks to signify memorial day, labor day, and
                        ;; thanksgiving on the calendar
                        (cond
                          [(and (= m 5) (= times 1)
                                (<= (- (length days-of-month) spot) 7))
                           (square size 'solid (color 60 59 110 150))]
                          [(and (= m 9) (= times 1)
                                (< spot 7))
                           (square size 'solid (color 178 34 52 150))]
                          [(and (= m 11) (= times 4)
                                (and (< (- (length days-of-month) spot) 10)
                                     (>= (- (length days-of-month) spot) 3)))
                           (square size 'solid (color 158 104 42 150))]
                          [else empty-image])
                        ;; Checks to find and mark the current day
                        (if (= spot (- d 1))
                            (overlay (circle (quotient size 6) 'solid title-bg)
                                     (circle (quotient size 4) 'solid day-bg))
                            empty-image)
                        ;; Adds box background
                        (square size 'solid cell-bg))
                       (day-boxes format days-of-month
                                  0 (+ spot 1) (+ times 1) m d))
                 (cons (overlay (square size 'outline 'black)
                                (square size 'solid cell-bg))
                       (day-boxes format days-of-month
                                  0 (+ spot 1) (+ times 1) m d))))
         (cons empty-image '()))]))


;"week-boxes: eyeball tests
;(NOTE: day-boxes eyeball tests are a subcomponent of week-boxes
;so day-boxes is not tested on its own since it relies on week
;boxes to run)"
;(week-boxes fmt0 5 1945)
;(week-boxes fmt0 2 2021)
;(week-boxes fmt0 11 2000)
;(week-boxes fmt0 9 2006)
;(week-boxes fmt0 7 2158)

;; day-boxes not eyeball tested because it relies on week-boxes to run so the
;; eyeball tests for week-boxes prove that day-boxes works
         
;; Universe Implementation —————————————————————————————————————————————————————

(: draw-month : CalFormat Integer Integer Integer -> Image)
;; Draws the image of a calendar based on CalFormat for the given month
;; and year
(define (draw-month format m d y)
  (match format
    [(CalFormat size title-bg title-color title-h
                day-bg day-color day-h cell-bg cell-color)
     (local
       {(: day-of-week-boxes : String -> Image)
        ;; Takes in a day of the week abbreviation and overlays it onto the
        ;; day of the week box
        (define (day-of-week-boxes str)
          (overlay (text str
                         (cast (quotient day-h 2) Byte)
                         day-color)
                   (rectangle size day-h 'outline 'black)
                   (rectangle size day-h 'solid day-bg)))
        (: m-length Integer) ;; Variable for the length of the given month
        (define m-length (days-in-month m y))}
       ;; Makes Calendar
       (overlay/align "left" "top"
                      (rectangle (* size 7) (+ (* size 6) title-h day-h)
                                  'solid (color 0 0 0 0))
                      (above
                       ;; Makes title box on Calendar
                       (overlay (text (string-append (get-month-name m)
                                                     " "
                                                     (number->string y))
                                      (cast (quotient title-h 2) Byte)
                                      title-color)
                                (rectangle (* size 7) title-h 'outline 'black)
                                (rectangle (* size 7) title-h 'solid title-bg))
                       ;; Makes day of week boxes on Calendar
                       (foldr beside empty-image
                              (map day-of-week-boxes (list "Sun" "Mon" "Tue"
                                                           "Wed" "Thu" "Fri"
                                                           "Sat")))
                       ;; Makes day boxes on Calendar
                       (week-boxes format (Date m d y)))))]))

;"draw-month: eyeball tests"
;(draw-month fmt0 2 1 1998)
;(draw-month fmt0 2 1 2021)
;(draw-month fmt0 2 1 2020)
;(draw-month fmt0 5 1 2021)
;(draw-month fmt0 11 1 2000)
;(draw-month fmt0 9 1 2007)
;(draw-month fmt0 8 1 2034)

(: draw-help-boxes : CalFormat -> Image)
;; Draws the help boxes in columns given the format
(define (draw-help-boxes fmt)
  (match fmt
    [(CalFormat size title-bg title-color title-h _ _ day-h cell-bg cell-color)
     (beside/align
      "top"
      (above/align "left"
                    (overlay (text "Calendar Shortcuts"
                                   (cast (quotient size 4) Byte) cell-color)
                             (rectangle (* size 3) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 3) (quotient size 2)
                                        'solid cell-bg))
                   (beside
                    (overlay (text "+" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "next day"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "-" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "previous day"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[right]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "next month"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[left]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "previous month"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[up]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "next year"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[down]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "previous year"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "T" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "today"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[return]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "entry"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "?" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "help"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg))))
      (above/align "left"
                   (overlay (text "Entry Shortcuts"
                                   (cast (quotient size 4) Byte) cell-color)
                             (rectangle (* size 3) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 3) (quotient size 2)
                                        'solid cell-bg))
                   (beside
                    (overlay (text "`" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "clear entry box"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[bksp]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "remove last char"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))
                   (beside
                    (overlay (text "[esc]" (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle size (quotient size 2)
                                        'outline 'black)
                             (rectangle size (quotient size 2)
                                        'solid cell-bg))
                    (overlay (text "calendar"
                                   (cast (quotient size 4) Byte)
                                   cell-color)
                             (rectangle (* size 2) (quotient size 2)
                                        'outline 'black)
                             (rectangle (* size 2) (quotient size 2)
                                        'solid cell-bg)))))]))

(: draw-help : CalFormat -> Image)
;; Draws the help screen with the given format
(define (draw-help fmt)
  (match fmt
    [(CalFormat size title-bg title-color title-h _ _ day-h cell-bg cell-color)
     (overlay/align
      "left" "top"
      (rectangle (* size 12) (+ (* size 7) title-h day-h)
                 'solid (color 0 0 0 0))
      ;; Puts title box over columns
      (above/align "left"
                   (overlay (text "HELP" (cast (quotient size 2) Byte)
                                  title-color)
                            (rectangle (* size 6) size 'outline 'black)
                            (rectangle (* size 6) size 'solid title-bg))
                   (draw-help-boxes fmt)
                   (rectangle (* size 3) (quotient size 2)
                              'solid (color 0 0 0 0))
                   (text "Press esc to return to the calendar."
                         (cast (quotient size 4) Byte) cell-color)))]))

(: draw-entry : CalWorld3 -> Image)
;; Draws the entry screen with the given format
(define (draw-entry world)
  (match world
    [(CalWorld3 _ e-mode fmt date _ _ _ note opt-s opt-e events)
     (match fmt
       [(CalFormat size title-bg title-color title-h _ _
                   day-h cell-bg cell-color)
        (overlay/align
         "left" "top"
         (rectangle (* size 12) (+ (* size 7) title-h day-h)
                    'solid (color 0 0 0 0))
         ;; Makes the screen
         (above/align "left"
                      (text
                       (string-append
                        (cond
                          [(symbol=? e-mode 'start) "Enter a start time"]
                          [(symbol=? e-mode 'end) "Enter an end time"]
                          [else "Enter a description"])
                        " (Press esc to return to the calendar)")
                       (cast (quotient size 3) Byte) cell-color)
                      (rectangle (* size 3) size 'solid (color 0 0 0 0))
                      (local
                        ;; Defines the text for the boxes as images
                        {(: date-str Image)
                         (define date-str
                           (text (make-date-string date)
                                 (cast (quotient size 2) Byte) 'black))
                         (: start-str Image)
                         (define start-str
                           (if (symbol=? e-mode 'start)
                               (text note (cast (quotient size 2) Byte) 'black)
                               (match opt-s
                                 ['None (text "" (cast (quotient size 2) Byte)
                                              'black)]
                                 [(Some (Time h m s))
                                  (text (no-seconds-time-string (Time h m s))
                                         (cast (quotient size 2) Byte)
                                         'black)])))
                         (: end-str Image)
                         (define end-str
                           (if (symbol=? e-mode 'end)
                               (text note (cast (quotient size 2) Byte) 'black)
                               (match opt-e
                                 ['None (text "" (cast (quotient size 2) Byte)
                                              'black)]
                                 [(Some (Time h m s))
                                  (text (no-seconds-time-string (Time h m s))
                                         (cast (quotient size 2) Byte)
                                         'black)])))
                         (: desc-str Image)
                         (define desc-str
                           (if (symbol=? e-mode 'description)
                               (text note (cast (quotient size 2) Byte) 'black)
                               (text "" (cast (quotient size 2) Byte) 'black)))}
                        ;; Makes the text fields
                        (overlay (text (make-date-string date)
                                       (cast (quotient size 2) Byte)
                                       cell-color)
                                 (rectangle (+ (image-width date-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width date-str)
                                               (quotient size 2))
                                            size 'solid 'lightgray))
                        (rectangle (* size 3) size 'solid (color 0 0 0 0))
                        ;; Makes text fields if on start
                        (cond
                          [(symbol=? e-mode 'start)
                           (above/align
                            "left"
                            ;; Start Box
                            (if (> (+ (image-width start-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 start-str
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'solid 'white))
                                (overlay
                                 start-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size 'solid 'white)))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; End Box
                            (overlay
                             end-str
                             (rectangle (* size 3) size 'outline 'black)
                             (rectangle (* size 3) size 'solid 'white))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; Description Box
                            (overlay
                             desc-str
                             (rectangle (* size 3) size 'outline 'black)
                             (rectangle (* size 3) size 'solid 'white)))]
                          [(symbol=? e-mode 'end)
                           (above/align
                            "left"
                            ;; Start box
                            (if (> (+ (image-width start-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 start-str
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'solid 'lightgray))
                                (overlay
                                 start-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size 'solid 'lightgray)))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; End Box
                            (if (> (+ (image-width end-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 end-str
                                 (rectangle (+ (image-width end-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width end-str)
                                               (quotient size 2))
                                            size 'solid 'white))
                                (overlay
                                 end-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size 'solid 'white)))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; Description Box
                            (overlay
                             desc-str
                             (rectangle (* size 3) size 'outline 'black)
                             (rectangle (* size 3) size 'solid 'white)))]
                          [else
                           (above/align
                            "left"
                            ;; Start box
                            (if (> (+ (image-width start-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 start-str
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width start-str)
                                               (quotient size 2))
                                            size 'solid 'lightgray))
                                (overlay
                                 start-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size 'solid 'lightgray)))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; End Box
                            (if (> (+ (image-width end-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 end-str
                                 (rectangle (+ (image-width end-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width end-str)
                                               (quotient size 2))
                                            size 'solid 'lightgray))
                                (overlay
                                 end-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size 'solid 'lightgray)))
                            (rectangle (* size 3) size 'solid (color 0 0 0 0))
                            ;; Description Box
                            (if (> (+ (image-width desc-str) (quotient size 2))
                                   (* size 3))
                                (overlay
                                 desc-str
                                 (rectangle (+ (image-width desc-str)
                                               (quotient size 2))
                                            size 'outline 'black)
                                 (rectangle (+ (image-width desc-str)
                                               (quotient size 2))
                                            size 'solid 'white))
                                (overlay
                                 desc-str
                                 (rectangle (* size 3) size 'outline 'black)
                                 (rectangle (* size 3) size
                                            'solid 'white))))]))))])]))

(: draw-event-section : CalWorld3 -> Image)
;; Draws the event section on the main calendar page
(define (draw-event-section world)
  (match world
    [(CalWorld3 _ _ fmt current-date _ _ _ _ _ _ tree)
     (match fmt
       [(CalFormat size title-bg title-color title-h _ _
                     day-h cell-bg cell-color)
        (local
          {(: events (Listof Event))
           (define events (retrieve-events current-date tree))
           (: draw-event : Event Image -> Image)
           ;; Draws an image of the event
           (define (draw-event e im)
             (above/align
              "left"
              (rectangle (* size 4) (quotient size 10) 'solid title-bg)
              (rectangle (* size 4) (quotient size 10) 'solid (color 0 0 0 0))
              (beside
               (rectangle (quotient size 10) (quotient (* size 3) 5)
                          'solid title-bg)
               (rectangle (quotient size 10) (quotient (* size 3) 5)
                          'solid (color 0 0 0 0))
               (match e
                 [(Event date 'all-day desc)
                  (above/align
                   "left"
                   (text (make-date-string date) (cast (quotient size 5) Byte)
                         'black)
                   (text "(all-day)" (cast (quotient size 5) Byte) 'black)
                   (text desc (cast (quotient size 5) Byte) 'black))]
                 [(Event date (Time h m s) desc)
                  (above/align
                   "left"
                   (text (make-date-string date) (cast (quotient size 5) Byte)
                         'black)
                   (text (no-seconds-time-string (Time h m s))
                         (cast (quotient size 5) Byte) 'black)
                   (text desc (cast (quotient size 5) Byte) 'black))]
                 [(Event date (Span (Time h1 m1 s1) (Time h2 m2 s2)) desc)
                  (above/align
                   "left"
                   (text (make-date-string date) (cast (quotient size 5) Byte)
                         'black)
                   (text (string-append
                          (no-seconds-time-string (Time h1 m1 s1))
                          "-"
                          (no-seconds-time-string (Time h2 m2 s2)))
                         (cast (quotient size 5) Byte) 'black)
                   (text desc (cast (quotient size 5) Byte) 'black))]))
               (rectangle (* size 4) (quotient size 10) 'solid (color 0 0 0 0))
               im))}
          (if (empty? events)
              (above (rectangle (* size 4) (quotient size 10) 'solid title-bg)
                     (text "-- no events --"
                           (cast (quotient size 5) Byte) 'black))
              (foldr draw-event empty-image events)))])]))

(: draw : CalWorld3 -> Image)
;; Draws the world
(define (draw world)
  (match world
    [(CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e events)
     ;; Decides between drawing help or the calendar
     (cond
       [(symbol=? mode 'help) (draw-help fmt)]
       [(symbol=? mode 'entry) (draw-entry world)]
       [else
        (match fmt
          [(CalFormat size _ _ title-h _ _ day-h _ cell-color)
           (beside
            ;; Draws the Calendar side
            (overlay/align "left" "top"
                           (rectangle (* size 7) (+ (* size 7) title-h day-h)
                                      'solid (color 0 0 0 0))
                           (above/align "left"
                                        (draw-month fmt (Date-month date)
                                                    (Date-day date)
                                                    (Date-year date))
                                        (rectangle (* size 7) (quotient size 4)
                                                   'solid (color 0 0 0 0))
                                        (text (make-date-string date)
                                              (cast (quotient size 4) Byte)
                                              cell-color)))
            ;; Draws the current date side
            (overlay/align "right" "top"
                           (rectangle (* size 5) (+ (* size 7) title-h day-h)
                                      'solid (color 0 0 0 0))
                           (above/align "right"
                                        (text now-str
                                              (cast (quotient size 4) Byte)
                                              cell-color)
                                        (rectangle (* size 4) (quotient size 4)
                                                   'solid (color 0 0 0 0))
                                        (text (make-time-string now-t)
                                              (cast (quotient size 4) Byte)
                                              cell-color)
                                        (rectangle (* size 5) (quotient size 4)
                                                   'solid (color 0 0 0 0))
                                        (text (string-append "Press ? for help"
                                                            " or [return] to"
                                                            " enter event.")
                                              (cast (quotient size 4) Byte)
                                              cell-color)
                                        (rectangle (* size 4) (quotient size 4)
                                                   'solid (color 0 0 0 0))
                                        (draw-event-section world))))])])]))

(: keyboard-listener : CalWorld3 String -> CalWorld3)
;; Affect the calendar based on key presses
(define (keyboard-listener world input)
  (cond
    [(symbol=? (CalWorld3-mode world) 'calendar) (calendar-key world input)]
    [(symbol=? (CalWorld3-mode world) 'help) (help-key world input)]
    [else (entry-key world input)]))

(: calendar-key : CalWorld3 String -> CalWorld3)
;; Executes the action for a keystroke on the calendar screen
;; Use arrow keys to "flip" through calendar
;; <- = move back month, -> = move forward month
;; /\ = move forward one year, \/ = move back one year
(define (calendar-key world input)
   (match world
    [(CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e events)
     (match date
       [(Date m d y)
        (match input
          ["+" (CalWorld3 mode e-mode fmt (tomorrow date) now-d now-str now-t
                          note opt-s opt-e events)]
          ["-" (CalWorld3 mode e-mode fmt (yesterday date) now-d now-str now-t
                          note opt-s opt-e events)]
          ["right" (cond
                     [(= m 12)
                      (CalWorld3 mode e-mode fmt (Date 1 d (+ y 1))
                                 now-d now-str now-t note opt-s opt-e events)]
                     [(> d (days-in-month (+ m 1) y))
                      (CalWorld3 mode e-mode fmt
                                 (Date (+ m 1) (days-in-month (+ m 1) y) y)
                                 now-d now-str now-t note opt-s opt-e events)]
                     [else
                      (CalWorld3 mode e-mode fmt (Date (+ m 1) d y)
                                 now-d now-str now-t note opt-s opt-e events)])]
          ["left" (cond
                    [(= m 1)
                     (CalWorld3 mode e-mode fmt (Date 12 d (- y 1))
                                now-d now-str now-t note opt-s opt-e events)]
                    [(> d (days-in-month (- m 1) y))
                     (CalWorld3 mode e-mode fmt
                                (Date (- m 1) (days-in-month (- m 1) y) y)
                                now-d now-str now-t note opt-s opt-e events)]
                    [else
                     (CalWorld3 mode e-mode fmt (Date (- m 1) d y)
                                now-d now-str now-t note opt-s opt-e events)])]
          ["up" (if (and (= m 2) (= d 29))
                    (CalWorld3 mode e-mode fmt (Date 3 1 (+ y 1))
                               now-d now-str now-t note opt-s opt-e events)
                    (CalWorld3 mode e-mode fmt (Date m d (+ y 1))
                               now-d now-str now-t note opt-s opt-e events))]
          ["down" (if (and (= m 2) (= d 29))
                      (CalWorld3 mode e-mode fmt (Date 3 1 (- y 1))
                                 now-d now-str now-t note opt-e opt-s events)
                      (CalWorld3 mode e-mode fmt (Date m d (- y 1))
                                 now-d now-str now-t note opt-s opt-e events))]
          ["T" (CalWorld3 mode e-mode fmt now-d now-d now-str now-t
                          note opt-s opt-e events)]
          ["\r" (CalWorld3 'entry e-mode fmt date now-d now-str now-t
                          note opt-s opt-e events)]
          ["?" (CalWorld3 'help e-mode fmt date now-d now-str now-t
                          note opt-s opt-e events)]
          [_ world])])]))

(: help-key : CalWorld3 String -> CalWorld3)
;; Executes the action for a keystroke on the calendar screen
(define (help-key world input)
  (match world
    [(CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e events)
        (match input
          ["escape" (CalWorld3 'calendar e-mode fmt date now-d now-str now-t
                               note opt-s opt-e events)]
          [_ world])]))

(: entry-key : CalWorld3 String -> CalWorld3)
;; Executes the action for a keystroke on the calendar screen
(define (entry-key world input)
  (match world
    [(CalWorld3 mode e-mode fmt date now-d now-str now-t
                note opt-s opt-e events)
        (match input
          ["\r" (cond
                  [(symbol=? e-mode 'start)
                   (if (is-valid-time? (string->list note))
                       (CalWorld3 mode 'end fmt date now-d now-str now-t
                                  "" (string->time note) opt-e events)
                       (CalWorld3 mode 'description fmt date now-d now-str now-t
                                  "" opt-s opt-e events))]
                  [(symbol=? e-mode 'end)
                   (CalWorld3 mode 'description fmt date now-d now-str now-t
                               "" opt-s (string->time note) events)]
                  [else
                   (match* (opt-s opt-e)
                     [('None _)
                      (insert-event-world (Event date 'all-day note)
                                          (CalWorld3 'calendar 'start fmt date
                                                     now-d now-str now-t ""
                                                     'None 'None events))]
                     [((Some (Time h m s)) 'None)
                      (insert-event-world (Event date (Time h m s) note)
                                          (CalWorld3 'calendar 'start fmt date
                                                     now-d now-str now-t ""
                                                     'None 'None events))]
                     [((Some (Time h1 m1 s1)) (Some (Time h2 m2 s2)))
                      (insert-event-world (Event date (Span (Time h1 m1 s1)
                                                            (Time h2 m2 s2))
                                                 note)
                                          (CalWorld3 'calendar 'start fmt date
                                                     now-d now-str now-t ""
                                                     'None 'None events))])])]
          ["\b" (CalWorld3 mode e-mode fmt date now-d now-str now-t
                               (substring note 0 (- (string-length note) 1))
                               opt-s opt-e events)]
          ["`" (CalWorld3 mode e-mode fmt date now-d now-str now-t
                               "" opt-s opt-e events)]
          ["escape" (CalWorld3 'calendar 'start fmt date now-d now-str now-t
                               "" 'None 'None events)]
          [str (if (= (string-length input) 1)
                   (CalWorld3 mode e-mode fmt date now-d now-str now-t
                              (string-append note str) opt-s opt-e events)
                   world)])]))

(: tick : CalWorld3 -> CalWorld3)
;; Affect the world based on the passing time
(define (tick world)
  (local
    {(: current-date Date)
     (define current-date (read-date-now))}
    (match world
      [(CalWorld3 'calendar e-mode fmt date now-d now-str now-t
                  note opt-e opt-s events)
       (if (date=? date current-date)
           (CalWorld3 'calendar e-mode fmt date now-d now-str (read-time-now)
                      note opt-e opt-s events)
           (CalWorld3 'calendar e-mode fmt date current-date
                      (make-date-string current-date) (read-time-now)
                      note opt-e opt-s events))]
      [_ world])))

(: run : CalFormat Integer Integer -> CalWorld3)
;; run the CalWorld
(define (run fmt m y)
  (local
    {(: current-date Date)
     (define current-date (read-date-now))}
    (big-bang (CalWorld3 'calendar 'start fmt (Date m 1 y)
                         current-date (make-date-string current-date)
                         (read-time-now) "" 'None 'None 'Empty) : CalWorld3
      [to-draw draw]
      [on-key keyboard-listener]
      [on-tick tick 1/4])))

;; Should def run on fmt1 and fmt4
"Eyeball tests are currently commented out."

(test)