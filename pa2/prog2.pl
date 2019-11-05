% Daniel Xiong dxiong5@ucsc.edu id#1660652
% prog2.pl
% due 11/8/19
% Pair Programming partner: Scott Zin nzin@ucsc.edu id#1679510

print_trip( Action, Code, Name, time( Hour, Minute)) :-
   upcase_atom( Code, Upper_code),
   format( "~6s  ~3s  ~s~26|  ~`0t~d~30|:~`0t~d~33|", [Action, Upper_code, Name, Hour, Minute]),
   nl.

print_itinerary([]).
print_itinerary([(Action, Code, Name, time(H, M))|T]) :-
	print_itinerary(T),
	print_trip(Action, Code, Name, time(H, M)).


degToRad(degmin(Deg, Min), Rad) :- 
	Rad is ((pi*(Deg + (Min / 60))) / 180).

% Lat, Long are of type degmin(D,M)
latAndLong(Lat, Long, Lat1, Long1) :- 
	degToRad(Lat, Lat1), 
	degToRad(Long, Long1).

% LatA, LongA, LatB, LongB are of type degmin(D,M)
getDist(LatA, LongA, LatB, LongB, Dist) :-
	latAndLong(LatA, LongA, Lat1, Long1),
	latAndLong(LatB, LongB, Lat2, Long2),
	Dlat is Lat2-Lat1,
	Dlong is Long2-Long1,
	A is (sin(Dlat / 2))^2 + (cos(Lat1) * cos(Lat2) * (sin(Dlong / 2))^2),
	C is 2 * atan2(sqrt(A), sqrt(1-A)),
	Dist is 3956 * C.

% A, B are airport codes, return value Time is time in hours to get from A to B
haversine(A, B, Time) :- 
	airport(A, _, LatA, LongA), 
	airport(B, _, LatB, LongB), 
	getDist(LatA, LongA, LatB, LongB, Dist), 
	Time is (Dist / 500).

travelTime(A, B, Hr, Min) :- 
	haversine(A, B, Time), 
	Temp is round(Time * 60), 
	Min is (Temp mod 60), 
	Hr is floor(Temp / 60).

addTimes(Hr_1, Min_1, Hr_2, Min_2, Hr_f, Min_f) :- 
	Temp is (Hr_1 + Hr_2) * 60 + Min_1 + Min_2,
	Min_f is (Temp mod 60), 
	Hr_f is floor(Temp / 60).

fly(A, B, time(Hr_Current, Min_Current), Itinerary) :- 
	% searches for a flight from A to B
	flight(A, B, time(Hr_Depart, Min_Depart)), 
	% getting here means there is an A to B flight, but I still have to check to see if it is valid
	Hr_Current*60+Min_Current =< Hr_Depart*60+Min_Depart,
	% getting here means it is valid, so now I add add times to get BArrival Time
	travelTime(A, B, Hr_TravelAToB, Min_TravelAToB),
	addTimes(Hr_Depart, Min_Depart, Hr_TravelAToB, Min_TravelAToB, Hr_BArrival, Min_BArrival),
	airport(B, CityB, _, _),
	append([], [(arrive, B, CityB, time(Hr_BArrival, Min_BArrival))], ItineraryUpdated),
	airport(A, CityA, _, _),
	append(ItineraryUpdated, [(depart, A, CityA, time(Hr_Depart, Min_Depart))], Itinerary).

fly(A, B, time(Hr_Current, Min_Current), Itinerary) :- 
	% goes through database and finds flight from A to any layover
	flight(A, Layover, time(Hr_Depart, Min_Depart)),
	% checks if the depart time is after our current time
	Hr_Current*60+Min_Current =< Hr_Depart*60+Min_Depart,
	% gets travel time from A to Layover
	travelTime(A, Layover, Hr_TravelAToLayover, Min_TravelAToLayover), 
	% adds departure time from A and travel time from A to Layover to get time of Arrival at Layover
	addTimes(Hr_Depart, Min_Depart, Hr_TravelAToLayover, Min_TravelAToLayover, Hr_LayoverArrival, Min_LayoverArrival), 
	% adds 30 minutes buffer time at Layover
	addTimes(Hr_LayoverArrival, Min_LayoverArrival, 0, 30, Hr_NewCurrent, Min_NewCurrent),
	Layover \= B,
	% finds flights from Layover to B
	fly(Layover, B, time(Hr_NewCurrent, Min_NewCurrent), ItineraryUpdated),
	% getting here means an ideal path was found, so now we append to Itinerary in reverse order
	airport(Layover, CityL, _, _),
	append(ItineraryUpdated, [(arrive, Layover, CityL, time(Hr_LayoverArrival, Min_LayoverArrival))], ItineraryUpdated1),
	airport(A, CityA, _, _),
	append(ItineraryUpdated1, [(depart, A, CityA, time(Hr_Depart, Min_Depart))], Itinerary).

main :- 
	read(A),
	read(B), 
	fly(A, B, time(0,0), Itinerary), 
	print_itinerary(Itinerary).

% database
airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).

flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).